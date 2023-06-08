mod_provlista_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::div(id = ns("provlista"),
    )
}

mod_provlista_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
    })
}
