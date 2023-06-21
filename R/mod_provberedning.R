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
  # Containing $df which is the table of the biologdata pulled from the db and entered by the user
  biologdata_table <- shiny::reactiveValues()
  # Containing provlista_table$dfs which is a list where the keys are the names of the prov, and the values are the coresponding dataframe
  provlista_table <- shiny::reactiveValues()

  shiny::moduleServer(id, function(input, output, session) {
    content_wrapper <- function(file) {
      report_content(file, biologdata_table$df, provlista_table$dfs[["prov1"]])
    }

    output$download_report <- shiny::downloadHandler(
      filename = "report.pdf",
      content = content_wrapper
    )

    mod_biologdata_server("biologdata", selected_accnrs, biologdata_table)
    mod_provlista_server("provlista", selected_accnrs, provlista_table)
    mod_validera_server("validera")
  })
}
