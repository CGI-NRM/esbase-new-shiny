testmodule_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(id = ns("test"), shiny::textOutput(outputId = ns("data_output")))
}

testmodule_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$data_output <- shiny::renderText("Hello :)")
  })
}
