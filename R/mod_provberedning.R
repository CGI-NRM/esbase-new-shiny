mod_provberedning_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns("provberedning"),
             shiny::div(style = "margin: 20px",
                        shiny::fluidRow(
                          shiny::downloadButton(
                            outputId = ns("download_report"),
                            label = "Generate Report",
                            icon = shiny::icon("download")),
                          shiny::actionButton(
                            inputId = ns("write_to_esbase"),
                            label = "Write to ESBase",
                            icon = shiny::icon("pen"))
                        )
             ),
             shiny::wellPanel(
               shiny::selectizeInput(
                 inputId = ns("projekt"),
                 label = "Projekt /Program",
                 choices = c(""),
                 options = list(placeholder = "Projekt"),
                 width = "100%"
               ),
               shiny::fluidRow(
                 shiny::column(6,
                               shiny::textInput(inputId = ns("accnr_start"), label = "Acc.nr. start", placeholder = "[A]YYYY/XXXXX"),
                               shiny::textOutput(outputId = ns("accnr_start_message"))
                 ),
                 shiny::column(6,
                               shiny::textInput(inputId = ns("accnr_end"), label = "Acc.nr. end", placeholder = "[A]YYYY/XXXXX"),
                               shiny::textOutput(outputId = ns("accnr_end_message"))
                 )
               ),
               shiny::textOutput(outputId = ns("accnr_selection_message")),
               DT::DTOutput(outputId = ns("accnr_selection_dt"))
             ),

             shiny::tabsetPanel(
               type = "tabs",
               shiny::tabPanel(title = "Biologdata",
                               shiny::wellPanel(
                                 mod_biologdata_ui(ns("biologdata")))
               ),
               shiny::tabPanel(title = "Provlista",
                               shiny::wellPanel(
                                 mod_provlista_ui(ns("provlista")))
               ),
               shiny::tabPanel(title = "Validera",
                               shiny::wellPanel(
                                 mod_validera_ui(ns("validera")))
               )
             )
             )
}

mod_provberedning_server <- function(id) {
  # A vector of the currently selected accnrs as specified in the table in the biologdata tab
  selected_accnrs <- shiny::reactiveVal()

  # Containing $df_db which is the table of the biologdata pulled from the db
  # and $df_override which contains mostly NAs, and then values where the user has changed/enetered in the table
  biologdata_table <- shiny::reactiveValues()

  # Containing $df_db which is the accession data gathered from the db
  accession_data_table <- shiny::reactiveValues()

  # Containing provlista_table$dfs which is a list with prov-names as keys and the coresponding dataframe as values
  #                     and $metas which is a dataframe where the
  #                         rowsnames are prov-names, and the
  #                         columns are 'homogenat analyslab analystyp analytiker provtagningsinst vavnads'
  provlista_table <- shiny::reactiveValues()

  # Containing $material_type_vector
  #            $species_vector
  #            $lokaler_vector
  #            $projects_vector
  #            which all are named vectors with ids and representations for the respective help-table
  # session$userData$stodlistor

  shiny::moduleServer(id, function(input, output, session) {

    # ---------- FUNCTIONS ----------
    update_select_inputs_with_stodlistor <- function() {
      # Update project choices from stödlista
      projects <- esbaser::get_options_project()
      session$userData$stodlistor$projects_vector <- projects[, "id", drop = TRUE]
      names(session$userData$stodlistor$projects_vector) <- projects[, "representation", drop = TRUE]
      shiny::updateSelectizeInput(session, "projekt", choices = session$userData$stodlistor$projects_vector,
                                  selected = NA, server = TRUE)
    }

    create_download_handler <- function() {
      content_wrapper <- function(file) {
        report_content(file = file,
                       biologdata = biologdata_table$df_db,
                       biologdata_override = biologdata_table$df_override,
                       biologdata_colnames = esbaser::get_biologdata_colnames(pretty = TRUE),
                       provlistas = provlista_table$dfs,
                       provlistas_colnames = lapply(provlista_table$dfs, colnames),
                       provlistas_metas = provlista_table$metas
        )
      }

      output$download_report <- shiny::downloadHandler(
        filename = "report.pdf",
        content = content_wrapper
      )
    }

    render_dt_clean <- function(df, colnames) {
      DT::renderDT(df,
                   options = list(dom = "t"),
                   rownames = FALSE,
                   colnames = colnames
      )
    }

    update_selected_accnrs <- function() {
      shiny::req(input$accnr_start)
      shiny::req(input$accnr_end)

      if (
        !esbaser::accnr_validate(input$accnr_start) ||
        !esbaser::accnr_validate(input$accnr_end)) {
        return()
      }

      accnr_start_list <- esbaser::accnr_parse(input$accnr_start)
      accnr_end_list <- esbaser::accnr_parse(input$accnr_end)

      if (accnr_start_list$letter != accnr_end_list$letter) {
        output$accnr_selection_message <- shiny::renderText(
          "AccNR start and end are not part of the same series. Ensure they start with the same letter.")
        return()
      } else if (accnr_start_list$year != accnr_end_list$year) {
        output$accnr_selection_message <- shiny::renderText(
          "AccNR start and end are not from the same year.")
        return()
      } else if (accnr_end_list$value < accnr_start_list$value) {
        output$accnr_selection_message <- shiny::renderText(
          "AccNR start comes after end. Ensure AccNR start is before AccNR end.")
        return()
      } else if (accnr_end_list$value - accnr_start_list$value >= 256) {
        output$accnr_selection_message <- shiny::renderText(
          "To large range, select fewer than 256.")
        return()
      } else {
        output$accnr_selection_message <- shiny::renderText("")
      }

      accession_data <- esbaser::load_accessions(input$accnr_start, input$accnr_end)

      series_list <- lapply(seq(0, accnr_end_list$value - accnr_start_list$value), esbaser::accnr_add, accnr_list = accnr_start_list)
      series <- unlist(lapply(series_list, esbaser::accnr_sprint))
      series_db <- unlist(lapply(series_list, esbaser::accnr_to_database_format))

      if (!identical(sort(series_db), sort(accession_data[, "id", drop = TRUE]))) {
        output$accnr_selection_message <- shiny::renderText(
          "Not all AccNRs exists in the database.")
        output$accnr_selection_dt <- render_dt_clean({
          data.frame(
            missing = unlist(lapply(
                setdiff(series_db, accession_data |> select(id) |> unlist()),
                function(accdb) {
                  esbaser::accnr_sprint(esbaser::accdb_parse_to_accnr(accdb))
                }
            ))
          )
        }, colnames = c("Missing from Database"))
        return()
      } else if (length(unique(accession_data[, "catalog_id", drop = TRUE])) != 1) {
        output$accnr_selection_message <- shiny::renderText(
          "The AccNRs belong to different catalogs.")
        output$accnr_selection_dt <- render_dt_clean({
          accession_data |> select(id, catalog_id)
        },
        colnames = c("AccNR", "Catalog"))
        return()
      } else if (length(unique(accession_data[, "species_id", drop = TRUE])) != 1) {
        output$accnr_selection_message <- shiny::renderText(
          "The AccNRs belong to different species.")
        output$accnr_selection_dt <- render_dt_clean({
          accession_data |> select(id, species_id)
        },
        colnames = c("AccNR", "Species"))
        return()
      } else if (length(unique(accession_data[, "locality_id", drop = TRUE])) != 1) {
        output$accnr_selection_message <- shiny::renderText(
          "The AccNRs belong to different localities.")
        output$accnr_selection_dt <- render_dt_clean({
          accession_data |> select(id, locality_id)
        },
        colnames = c("AccNR", "Locality"))
        return()
      } else {
        output$accnr_selection_message <- shiny::renderText("")
        output$accnr_selection_dt <- DT::renderDT(data.frame())
      }

      accession_data_table$tib <- accession_data
      selected_accnrs(series)
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()
    create_download_handler()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$accnr_start, {
      shiny::req(input$accnr_start)
      if (!esbaser::accnr_validate(input$accnr_start)) {
        output$accnr_start_message <- shiny::renderText(
          "Invalid AccNR in start. Please enter on the form '[ABCDGHLXP]YYYY/XXXXX' or '[ABCDGHLXP]YYYYXXXXX'")
        return()
      } else {
        output$accnr_start_message <- shiny::renderText("")
      }

      update_selected_accnrs()
    })

    shiny::observeEvent(input$accnr_end, {
      shiny::req(input$accnr_end)
      if (!esbaser::accnr_validate(input$accnr_end)) {
        output$accnr_end_message <- shiny::renderText(
          "Invalid AccNR in end. Please enter on the form '[ABCDGHLXP]YYYY/XXXXX' or '[ABCDGHLXP]YYYYXXXXX'")
        return()
      } else {
        output$accnr_end_message <- shiny::renderText("")
      }

      update_selected_accnrs()
    })

    # ---------- MODULE SERVERS ----------
    mod_biologdata_server("biologdata", selected_accnrs, biologdata_table)
    mod_provlista_server("provlista", selected_accnrs, provlista_table)
    mod_validera_server("validera")
  })
}
