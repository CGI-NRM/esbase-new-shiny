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
                                 mod_biologdata_ui(ns("biologdata"))
               ),
               shiny::tabPanel(title = "Provlista",
                                 mod_provlista_ui(ns("provlista"))
               )
             )
             )
}

mod_provberedning_server <- function(id, db) {
  # A new holder to move into, to replace selected_accnrs, biologdata_table
  # $acc           - accession table/tibble of selected
  # $accs_db       - vector of selected accnrs, formated in database format
  # $acc_min       - formatted AYYYY/XXXXX
  # $acc_max       - formatted AYYYY/XXXXX
  # $bio           - clam/fish/mammal etc table/tibble of selected accession_id
  # $mat           - all materials that points to selected accession_id (can be multilpe per accession)
  # $specimen      - all specimen that points to selected accession_id (one per accession)
  selected <- dataHolder(
    acc = tibble(),
    accs_db = character(0),
    acc_min = "",
    acc_max = "",
    bio = tibble(),
    mat = tibble(),
    specimen = tibble()
  )
  selected_update <- shiny::reactiveVal(0)

  # A holder for combined biologdata
  # $df         - The combined data from the database
  # $override   - The data as the user has entered it
  # $formats    - A named vector with numbrojs
  # $colnames   - The pretty colnames
  biologdata <- dataHolder()

  # A vector of the currently selected accnrs as specified in the table in the biologdata tab
  # TODO: Remove this and refactor to new pulling
  selected_accnrs <- shiny::reactiveVal()

  # Containing provlista_table$dfs which is a list with prov-names as keys and the coresponding dataframe as values
  #                     and $metas which is a dataframe where the
  #                         rowsnames are prov-names, and the
  #                         columns are 'homogenat analyslab analystyp analytiker provtagningsinst vavnads'
  provlista_table <- dataHolder()

  # Containing $material_type_vector
  #            $species_vector
  #            $lokaler_vector
  #            $projects_vector
  #            which all are named vectors with ids and representations for the respective help-table
  # session$userData$stodlistor

  shiny::moduleServer(id, function(input, output, session) {
    loginfo("mod_provberedning_server.R: module server start")

    # ---------- FUNCTIONS ----------
    update_select_inputs_with_stodlistor <- function() {
      logdebug("mod_provberedning.R - update_select_inputs_with_stodlistor: called")
      # Update project choices from stödlista
      projects_vector <- db$project |> select(id) |> unlist(use.names = FALSE)
      names(projects_vector) <- db$project |> select(number, name) |> apply(1, paste_collapse)
      names(projects_vector)[names(projects_vector) == ""] <- "-"
      shiny::updateSelectizeInput(session, "projekt", choices = projects_vector,
                                  selected = NA, server = TRUE)
      logfine("mod_provberedning.R - update_select_inputs_with_stodlistor: finished")
    }

    create_download_handler <- function() {
      logdebug("mod_provberedning.R - create_download_handler: called")
      content_wrapper <- function(file) {
        report_content(file = file,
                       selected = selected,
                       biologdata = biologdata,
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
      logdebug("mod_provberedning.R - render_dt_clean: called")
      DT::renderDT(df,
                   options = list(dom = "tp"),
                   rownames = FALSE,
                   colnames = colnames
      )
    }

    deselect_selected_accnrs <- function() {
      logdebug("mod_provberedning.R - deselect_selected_accnrs: called")
      selected$acc <- tibble()
      selected$acc_min <- ""
      selected$acc_max <- ""
      selected$accs_db <- character(0)
      selected$bio <- tibble()
      selected$specimen <- tibble()
      selected$material <- tibble()

      selected_update(selected_update() + 1)
      logfine("mod_provberedning.R - deselect_selected_accnrs: finished")
    }

    update_selected_accnrs <- function() {
      logdebug("mod_provberedning.R - update_selected_accnrs: called")
      shiny::req(input$accnr_start)
      shiny::req(input$accnr_end)

      if (!esbaser::accnr_validate(input$accnr_start) ||
          !esbaser::accnr_validate(input$accnr_end)) {
        return()
      }

      if (identical(input$accnr_start, selected$acc_min) &&
          identical(input$accnr_end, selected$acc_max)) {
        return()
      }

      accnr_start_tib <- esbaser::accnr_parse(input$accnr_start)
      accnr_end_tib <- esbaser::accnr_parse(input$accnr_end)

      if (accnr_start_tib$letter != accnr_end_tib$letter) {
        output$accnr_selection_message <- shiny::renderText(
          "AccNR start and end are not part of the same series. Ensure they start with the same letter.")
        deselect_selected_accnrs()
        return()
      } else if (accnr_start_tib$year != accnr_end_tib$year) {
        output$accnr_selection_message <- shiny::renderText(
          "AccNR start and end are not from the same year.")
        deselect_selected_accnrs()
        return()
      } else if (accnr_end_tib$value < accnr_start_tib$value) {
        output$accnr_selection_message <- shiny::renderText("AccNR start comes after end. Ensure AccNR start is before AccNR end.")
        deselect_selected_accnrs()
        return()
      } else if (accnr_end_tib$value - accnr_start_tib$value >= 256) {
        output$accnr_selection_message <- shiny::renderText("To large range, select fewer than 256.")
        deselect_selected_accnrs()
        return()
      } else {
        output$accnr_selection_message <- shiny::renderText("")
      }

      accession_data <- esbaser::get_accessions_between(db$conn, input$accnr_start, input$accnr_end)

      series_tib <- esbaser::accnr_add(accnr_start_tib, seq(0, accnr_end_tib$value - accnr_start_tib$value))
      series <- esbaser::accnr_sprint(series_tib)
      series_db <- esbaser::accnr_db_sprint(series_tib)

      catalog_id <- accession_data |> select(catalog_id) |> unique()
      locality_id <- accession_data |> select(locality_id) |> unique()
      species_id <- accession_data |> select(species_id) |> unique()

      if (!identical(
          series_db |> as.character() |> sort(),
          accession_data |> select(id) |> unlist() |> as.character() |> sort()
      )) {
        output$accnr_selection_message <- shiny::renderText("Not all AccNRs exists in the database.")
        output$accnr_selection_dt <- render_dt_clean(
          data.frame(
            missing = setdiff(series_db, accession_data |> select(id)) |> esbaser::accdb_to_accnr()
          ),
          colnames = c("Missing from Database"))
        deselect_selected_accnrs()
        return()
      } else if (catalog_id |> nrow() != 1) {
        output$accnr_selection_message <- shiny::renderText("The AccNRs belong to different catalogs.")
        output$accnr_selection_dt <- render_dt_clean(
          data.frame(
            id = accession_data |> select(id) |> unlist() |> esbaser::accdb_to_accnr(),
            catalog = accession_data |> select(catalog_id) |> repr_catalog(db)
          ),
          colnames = c("AccNR", "Catalog"))
        deselect_selected_accnrs()
        return()
      } else if (species_id |> nrow() != 1) {
        output$accnr_selection_message <- shiny::renderText("The AccNRs belong to different species.")
        output$accnr_selection_dt <- render_dt_clean(
          data.frame(
            id = accession_data |> select(id) |> unlist() |> esbaser::accdb_to_accnr(),
            species = accession_data |> select(species_id) |> repr_species(db)
          ),
          colnames = c("AccNR", "Species"))
        deselect_selected_accnrs()
        return()
      } else if (locality_id |> nrow() != 1) {
        output$accnr_selection_message <- shiny::renderText("The AccNRs belong to different localities.")
        output$accnr_selection_dt <- render_dt_clean(
          data.frame(
            id = accession_data |> select(id) |> unlist() |> esbaser::accdb_to_accnr(),
            locality = accession_data |> select(locality_id) |> repr_locality(db)
          ),
          colnames = c("AccNR", "Locality"))
        deselect_selected_accnrs()
        return()
      } else {
        output$accnr_selection_message <- shiny::renderText("")
        output$accnr_selection_dt <- DT::renderDT(data.frame())
      }

      selected$acc_min <- input$accnr_start
      selected$acc_max <- input$accnr_end
      selected$acc <- accession_data
      selected$accs_db <- series_db

      if (catalog_id == 0) { # Alla
        shiny::showNotification("Cannot pull biodata for catalog 'Alla'.", duration = 10, type = "warning")
        selected$bio <- tibble(accession_id = series_db)
      } else if (catalog_id == 1) { # Fågel
        selected$bio <- esbaser::get_bird_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 2) { # Fisk
        selected$bio <- esbaser::get_fish_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 3) { # Däggdjur
        selected$bio <- esbaser::get_mammal_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 4) { # Ägg
        selected$bio <- esbaser::get_egg_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 5) { # Mossa
        shiny::showNotification("Cannot pull biodata for catalog 'Mossa'.", duration = 10, type = "warning")
        selected$bio <- tibble(accession_id = series_db)
      } else if (catalog_id == 6) { # Mussla
        selected$bio <- esbaser::get_clam_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 7) { # Övrigt
        shiny::showNotification("Cannot pull biodata for catalog 'Övrigt'.", duration = 10, type = "warning")
        selected$bio <- tibble(accession_id = series_db)
      } else {
        shiny::showNotification(paste0("Unknown catalog_id: ", catalog_id, ". Cannot pull bio data"), duration = 10, type = "warning")
        selected$bio <- tibble(accession_id = series_db)
      }

      selected$specimen <- esbaser::get_specimen_between(db$conn, selected$acc_min, selected$acc_max)
      selected$material <- esbaser::get_material_between(db$conn, selected$acc_min, selected$acc_max)

      selected_update(selected_update() + 1)
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()
    create_download_handler()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$accnr_start, {
      shiny::req(input$accnr_start)
      if (isFALSE(esbaser::accnr_validate(input$accnr_start))) {
        output$accnr_start_message <- shiny::renderText(
          "Invalid AccNR in start. Please enter on the form '[ABCDGHLXP]YYYY/XXXXX' or '[ABCDGHLXP]YYYYXXXXX'")
        deselect_selected_accnrs()
        return()
      } else {
        output$accnr_start_message <- shiny::renderText("")
      }

      update_selected_accnrs()
    })

    shiny::observeEvent(input$accnr_end, {
      shiny::req(input$accnr_end)
      if (isFALSE(esbaser::accnr_validate(input$accnr_end))) {
        output$accnr_end_message <- shiny::renderText(
          "Invalid AccNR in end. Please enter on the form '[ABCDGHLXP]YYYY/XXXXX' or '[ABCDGHLXP]YYYYXXXXX'")
        deselect_selected_accnrs()
        return()
      } else {
        output$accnr_end_message <- shiny::renderText("")
      }

      update_selected_accnrs()
    })

    # ---------- MODULE SERVERS ----------
    mod_biologdata_server("biologdata",
                          db = db,
                          selected = selected,
                          selected_update = selected_update,
                          biologdata = biologdata)
    mod_provlista_server("provlista",
                         db = db,
                         selected = selected,
                          selected_update = selected_update,
                         provlista_table = provlista_table)
    mod_validera_server("validera",
                        db = db,
                        selected = selected,
                        selected_update = selected_update)
  })
}
