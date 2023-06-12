mod_provberedning_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns("provberedning"),
             shiny::div(style = "margin: 20px",
                        shiny::fluidRow(
                          shiny::actionButton(
                            inputId = ns("download_excel"),
                            label = "Download Excel",
                            icon = shiny::icon("download")),
                          shiny::actionButton(
                            inputId = ns("upload_excel"),
                            label = "Upload Excel",
                            icon = shiny::icon("upload")),
                          shiny::actionButton(
                            inputId = ns("write_to_esbase"),
                            label = "Write to ESBase",
                            icon = shiny::icon("pen"))
                        )
             ),

             shiny::tabsetPanel(
               type = "tabs",
               shiny::tabPanel(title = "Biologdata", mod_biologdata_ui(ns("biologdata"))),
               shiny::tabPanel(title = "Provlista", mod_provlista_ui(ns("provlista")))
             )
             )
}

mod_provberedning_server <- function(id) {
  selected_accnrs <- shiny::reactiveVal()

  shiny::moduleServer(id, function(input, output, session) {
    mod_biologdata_server("biologdata", selected_accnrs)
    mod_provlista_server("provlista", selected_accnrs)
  })
}
