mod_biologdata_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns("biologdata"),
             shiny::h3("Biologdata"),
             shiny::fluidRow(
               shiny::column(4,
                             shinyBS::tipify(
                               shiny::selectInput(
                                 inputId = ns("lokal"),
                                 label = "Lokal",
                                 choices = c(""),
                                 selectize = TRUE,
                               ),
                               title = "(Område, sjö, närmsta ort, sjödist.)",
                               placement = "top"
                             ),
                             shiny::fluidRow(
                               shiny::column(6,
                                             shiny::selectInput(
                                               inputId = ns("artnamn"),
                                               label = "Artnamn (sv)",
                                               choices = c(""),
                                               selectize = TRUE
                                             )
                               ),
                               shiny::column(6,
                                             shiny::numericInput(
                                               inputId = ns("antal"),
                                               label = "Antal ind.",
                                               value = 12,
                                               min = 1,
                                               step = 1
                                             )
                               )
                             )
                             ),
                             shiny::column(4,
                                           shinyBS::tipify(
                                             shiny::dateInput(
                                               inputId = ns("beredningsdatum"),
                                               label = "Beredningsdatum",
                                               value = NA,
                                               format = "yyyy-mm-dd"
                                             ),
                                             title = "ÅÅÅÅ-MM-DD",
                                             placement = "top"
                                           ),
                                           shinyBS::tipify(
                                             shiny::textInput(
                                               inputId = ns("projekt"),
                                               label = "Projekt /Program"
                                             ),
                                             title = "(t.ex. x-projekt etc)",
                                             placement = "top"
                                           )
                             ),
                             shiny::column(4,
                                           shiny::textInput(
                                             inputId = ns("provberedare"),
                                             label = "Provberedare, enhet"
                                           ),
                                           shiny::fluidRow(
                                             shiny::column(6,
                                                           shinyBS::tipify(
                                                             shiny::dateInput(
                                                               inputId = ns("fangstdatum_fran"),
                                                               label = "Fångstdatum från",
                                                               value = NA,
                                                               format = "yyyy-mm-dd"
                                                             ),
                                                             title = "ÅÅÅÅ-MM-DD",
                                                             placement = "top"
                                                           )
                                             ),
                                             shiny::column(6,
                                                           shinyBS::tipify(
                                                             shiny::dateInput(
                                                               inputId = ns("fangstdatum_till"),
                                                               label = "Fångstdatum till",
                                                               value = NA,
                                                               format = "yyyy-mm-dd"
                                                             ),
                                                             title = "ÅÅÅÅ-MM-DD",
                                                             placement = "top"
                                                           )
                                             )
                                             )
                                             )
                                             ),
                                             rhandsontable::rHandsontableOutput(ns("details_table"))
                                             )
}


mod_biologdata_server <- function(id, selected_accnrs) {
  shiny::moduleServer(id, function(input, output, session) {
    # ---------- FUNCTIONS ----------
    update_select_inputs_with_stodlistor <- function() {
      paste_collapse <- function(x) {
        paste(x[x != ""], collapse = ", ")
      }
      # Update lokaler choices from stödlista
      lokaler <- esbaser::get_stodlista_lokaler()
      lokaler_vector <- apply(lokaler, 1, paste_collapse) # Combine data from all columns
      shiny::updateSelectInput(session, "lokal", choices = c("", lokaler_vector)) # Empty string so that select is empty when page loads

      # Update arter choices from stödlista
      arter <- esbaser::get_stodlista_arter()
      arter_vector <- apply(arter, 1, paste_collapse) # Combine data from all columns
      shiny::updateSelectInput(session, "artnamn", choices = c("", arter_vector)) # Empty string so that select is empty when page loads
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$antal, {
    })
  })
}
