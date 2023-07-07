mod_validera_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(id = ns("validera")
  )
}

mod_validera_server <- function(id, db, selected, selected_update) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}
