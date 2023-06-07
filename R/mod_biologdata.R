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
                                                                              choices = c("Östersjön", "Västkusten"),
                                                                              selectize = TRUE
                                                                              ),
                                                           title = "(Område, sjö, närmsta ort, sjödist.)",
                                                           placement = "top"
                                                           ),
                                           shiny::fluidRow(
                                                           shiny::column(6,
                                                                         shiny::textInput(
                                                                                          inputId = ns("artnamn"),
                                                                                          label = "Artnamn (sv)"
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
                                                                            value = NA
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
                                           shinyBS::tipify(
                                                           shiny::dateInput(
                                                                            inputId = ns("fangstdatum"),
                                                                            label = "Fångstdatum fr - till",
                                                                            value = NA
                                                                            ),
                                                           title = "ÅÅÅÅ-MM-DD",
                                                           placement = "top"
                                           )
                             )
             ),
             DT::DTOutput(outputId = ns("details_table"))
  )
}

mod_biologdata_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
                        shiny::observeEvent(input$antal, {
                                              if (is.na(input$antal) || input$antal < 1) {
                                                return()
                                              }

                                              colnames <- c("Annat NRMnr.", "Acc.nr.", "Ålder (år)",
                                                            "Kroppsvikt (g)", "Totallängd (cm)",
                                                            "Kroppslängd (cm)", "Kön", "Gonadvikt (g)",
                                                            "Gonad sparad J/N", "Levervikt (g)", "Lever kvar (g)",
                                                            "Parasit (g)", "Skrottvikt (g)", "Mage sparad J/N",
                                                            "Notering/Avvikelse")
                                              dt <- data.frame(matrix(nrow = input$antal, ncol = length(colnames)))
                                              dt[, 4] <- runif(input$antal, 10, 100)

                                              colnames(dt) <- colnames
                                              output$details_table <- DT::renderDT(
                                                                                   dt,
                                                                                   options = list(paging = FALSE, colnames = NA),
                                                                                   rownames = FALSE)
             }
                        )
  })
}
