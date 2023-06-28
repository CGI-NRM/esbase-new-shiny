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
             shiny::selectizeInput(
               inputId = ns("projekt"),
               label = "Projekt /Program",
               choices = c(""),
               options = list(placeholder = "Projekt"),
               width = "100%"
             ),

             shiny::tabsetPanel(
               type = "tabs",
               shiny::tabPanel(title = "Biologdata", mod_biologdata_ui(ns("biologdata"))),
               shiny::tabPanel(title = "Provlista", mod_provlista_ui(ns("provlista"))),
               shiny::tabPanel(title = "Validera", mod_validera_ui(ns("validera")))
             )
             )
}

mod_provberedning_server <- function(id) {
  # A vector of the currently selected accnrs as specified in the table in the biologdata tab
  selected_accnrs <- shiny::reactiveVal()

  # Containing $df_db which is the table of the biologdata pulled from the db
  # and $df_override which contains mostly NAs, and then values where the user has changed/enetered in the table
  biologdata_table <- shiny::reactiveValues()

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

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()
    create_download_handler()

    mod_biologdata_server("biologdata", selected_accnrs, biologdata_table)
    mod_provlista_server("provlista", selected_accnrs, provlista_table)
    mod_validera_server("validera")
  })
}
