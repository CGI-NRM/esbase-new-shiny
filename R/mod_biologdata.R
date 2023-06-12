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
                                             DT::DTOutput(outputId = ns("details_table"))
                                             )
}


mod_biologdata_server <- function(id, selected_accnrs) {
  shiny::moduleServer(id, function(input, output, session) {
    details_table <- shiny::reactiveValues()

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

    update_details_table_new_amount <- function() {
      if (is.na(input$antal) || input$antal < 0) {
        return()
      }

      colnames <- c("Annat NRMnr.", "Acc.nr.", "Ålder (år)",
                    "Kroppsvikt (g)", "Totallängd (cm)",
                    "Kroppslängd (cm)", "Kön", "Gonadvikt (g)",
                    "Gonad sparad J/N", "Levervikt (g)", "Lever kvar (g)",
                    "Parasit (g)", "Skrottvikt (g)", "Mage sparad J/N",
                    "Notering/Avvikelse")
      details_table$dt <- data.frame(matrix(nrow = input$antal, ncol = length(colnames)))
      accnrs <- data.frame(letter = "A", year = 2022, value = 1:input$antal)
      accnrs_strs <- unlist(lapply(1:input$antal, function(row) {
        esbaser::accnr_sprint(accnrs[row, ])
      }))
      selected_accnrs(accnrs_strs)
      details_table$dt[, 2] <- selected_accnrs()

      for (row in 1:input$antal) {
        details_table$dt[row, ] <- esbaser::get_accnr_biologdata(details_table$dt[row, 2])
      }

      colnames(details_table$dt) <- colnames
      output$details_table <- DT::renderDT(
        details_table$dt,
        options = list(
          paging = FALSE,
          colnames = NA,
          dom = "t",
          ordering = FALSE,
          filter = "none"),
        rownames = FALSE,
        server = TRUE,
        selection = "none",
        edit = list(target = "column")
      )
      details_table$proxy <- DT::dataTableProxy("details_table")
    }

    update_details_table_new_accnr <- function() {
      changed <- FALSE

      for (row in seq_len(nrow(input$details_table_cell_edit))) {
        if (input$details_table_cell_edit[row, "col"] == 1) {
          v <- stringr::str_trim(input$details_table_cell_edit[row, "value"])
          empty <- v == "-" || v == ""
          valid <- esbaser::accnr_validate(v)

          if (empty || !valid) {
            v <- ""
          }

          details_table$dt[row, 2] <- shiny::isolate(DT::coerceValue(v, details_table$dt[row, 2]))
          details_table$dt[row, ] <- esbaser::get_accnr_biologdata(details_table$dt[row, 2])
          changed <- TRUE

          if (empty || !valid) {
            details_table$dt[row, 2] <- "-"
          }

          if (!empty && !valid) {
            shiny::showNotification(
              "Invalid AccNR format. Please enter on the form A2022/12345 or A202212345.", duration = 30, type = "warning")
          }
        }
      }
      if (changed) {
        selected_accnrs(details_table$dt[, 2])
        DT::replaceData(details_table$proxy, details_table$dt)
      }
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$antal, {
      update_details_table_new_amount()
    })

    shiny::observeEvent(input$details_table_cell_edit, {
      update_details_table_new_accnr()
    })
  })
}
