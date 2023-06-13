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
                                             shiny::div(class = "monospace",
                                                        DT::DTOutput(outputId = ns("details_table"))
                                             ),
                                             shiny::actionButton(
                                               inputId = ns("klona_accnr_fran_forsta"),
                                               label = "Kopiera AccNR från första"),
                                             shiny::actionButton(
                                               inputId = ns("sekvens_accnr_fran_forsta"),
                                               label = "Sekvens av AccNR från första")
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

    render_dt_details_table <- function() {
      if (is.na(input$antal) || input$antal < 0) {
        return()
      }

      details_table$dt <- data.frame(matrix(nrow = input$antal, ncol = length(esbaser::get_biologdata_colnames(pretty = FALSE))))
      colnames(details_table$dt) <- esbaser::get_biologdata_colnames(pretty = FALSE)

      details_table$dt[, "accnr"] <- rep("", input$antal)
      selected_accnrs(details_table$dt[, "accnr"])

      for (row in 1:input$antal) {
        details_table$dt[row, ] <- esbaser::get_accnr_biologdata(details_table$dt[row, "accnr"])
      }

      output$details_table <- DT::renderDT(
        details_table$dt,
        options = list(
          paging = FALSE,
          processing = FALSE,
          dom = "t",
          ordering = FALSE,
          filter = "none",
          scrollX = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(width = "110px", targets = c(2)),
            list(width = "20px", targets = c(0))
          )
        ),
        server = TRUE,
        selection = "none",
        edit = list(target = "column", disable = list(columns = c(0))),
        colnames = esbaser::get_biologdata_colnames(pretty = TRUE)
      )
      details_table$proxy <- DT::dataTableProxy("details_table")
    }

    handle_details_table_cell_edit <- function() {
      changed <- FALSE

      for (row in seq_len(nrow(input$details_table_cell_edit))) {
        j <- input$details_table_cell_edit[row, "col"]
        if (j == 0 || is.na(colnames(details_table$dt)[j])) {
          next
        }
        if (colnames(details_table$dt)[j] == "accnr") {
          v <- stringr::str_trim(input$details_table_cell_edit[row, "value"])
          empty <- v == "-" || v == ""
          valid <- esbaser::accnr_validate(v)

          if (empty || !valid) {
            v <- ""
          }

          if (v != details_table$dt[row, "accnr"]) {
            if (valid) {
              v <- v %>% esbaser::accnr_parse() %>% esbaser::accnr_sprint()
            }
            details_table$dt[row, "accnr"] <- v
            changed <- TRUE
          }

          if (empty || !valid) {
            details_table$dt[row, "accnr"] <- ""
          }

          if (!empty && !valid) {
            changed <- TRUE
            shiny::showNotification(
              "Invalid AccNR format. Please enter on the form A2022/12345 or A202212345.", duration = 30, type = "error")
          }
        }
      }

      selected_accnrs(details_table$dt[, "accnr"])

      if (changed) {
        update_dt_details_table()
      }
    }

    update_dt_details_table <- function() {
      for (row in seq_len(nrow(input$details_table_cell_edit))) {
        details_table$dt[row, ] <- esbaser::get_accnr_biologdata(details_table$dt[row, "accnr"])
      }
      DT::replaceData(details_table$proxy, details_table$dt, resetPaging = FALSE) #, rownames = FALSE)
    }

    klona_accnr_fran_forsta <- function() {
      if (esbaser::accnr_validate(details_table$dt[1, "accnr"])) {
        first_accnr <- esbaser::accnr_parse(details_table$dt[1, "accnr"])
        details_table$dt[, "accnr"] <- esbaser::accnr_sprint(first_accnr)
        selected_accnrs(details_table$dt[, "accnr"])
        update_dt_details_table()
      } else {
        shiny::showNotification("Invalid AccNR in first position.", duration = 15, type = "error")
      }
    }

    sekvens_accnr_fran_forsta <- function() {
      if (esbaser::accnr_validate(details_table$dt[1, "accnr"])) {
        first_accnr <- esbaser::accnr_parse(details_table$dt[1, "accnr"])
        for (row in 1:input$antal) {
          details_table$dt[row, "accnr"] <- esbaser::accnr_sprint(esbaser::accnr_add(first_accnr, row - 1))
        }
        selected_accnrs(details_table$dt[, "accnr"])
        update_dt_details_table()
      } else {
        shiny::showNotification("Invalid AccNR in first position.", duration = 15, type = "error")
      }
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$antal, {
      render_dt_details_table()
    })

    shiny::observeEvent(input$details_table_cell_edit, {
      handle_details_table_cell_edit()
    })

    shiny::observeEvent(input$klona_accnr_fran_forsta, {
      klona_accnr_fran_forsta()
    })

    shiny::observeEvent(input$sekvens_accnr_fran_forsta, {
      sekvens_accnr_fran_forsta()
    })
  })
}
