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
                                 choices = c("", "Abiskojaure", "Bolmen", "Brännträsket"),
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
                                               choices = c("", "Röding", "Gädda", "Abborre"),
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
                                             DT::DTOutput(outputId = ns("details_table"))
                                             )
}

mod_biologdata_server <- function(id, selected_accnrs) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$antal, {
      if (is.na(input$antal) || input$antal < 0) {
        return()
      }

      colnames <- c("Annat NRMnr.", "Acc.nr.", "Ålder (år)",
                    "Kroppsvikt (g)", "Totallängd (cm)",
                    "Kroppslängd (cm)", "Kön", "Gonadvikt (g)",
                    "Gonad sparad J/N", "Levervikt (g)", "Lever kvar (g)",
                    "Parasit (g)", "Skrottvikt (g)", "Mage sparad J/N",
                    "Notering/Avvikelse")
      dt <- data.frame(matrix(nrow = input$antal, ncol = length(colnames)))
      selected_accnrs(1:input$antal)
      dt[, 2] <- selected_accnrs()

      for (row in 1:input$antal) {
        dt[row, ] <- esbaser::get_accnr_biologdata(dt[row, 2])
      }

      colnames(dt) <- colnames
      output$details_table <- DT::renderDT(
        dt,
        options = list(
          paging = FALSE,
          colnames = NA,
          dom = "t"),
        rownames = FALSE)
    }
    )
  })
}
