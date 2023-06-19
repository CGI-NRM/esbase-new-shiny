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
                                             rhandsontable::rHandsontableOutput(ns("details_table")),
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

    update_details_table_rowcount <- function() {
      if (is.na(input$antal) || input$antal < 0) {
        return()
      }

      if (NROW(details_table$df) < input$antal) {
        new_rows <- bind_rows(lapply(rep("", input$antal - NROW(details_table$df)), esbaser::get_accnr_biologdata))
        details_table$df <- rbind(details_table$df, new_rows)
      } else {
        details_table$df <- details_table$df[1:input$antal, ]
      }

      selected_accnrs(details_table$df[, "accnr"])
    }

    render_details_table <- function() {
      shiny::req(shiny::isolate(details_table$df))
      df <- shiny::isolate(details_table$df)

      output$details_table <- rhandsontable::renderRHandsontable({
        hot <- rhandsontable::rhandsontable(df, rowHeaders = NULL, overflow = "visible", maxRows = nrow(df)) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
        rhandsontable::hot_col("accnr", renderer = rhot_renderer_validate_accnr) |>
        rhandsontable::hot_col(which(colnames(df) != "accnr"), renderer = rhot_renderer_gray_bg_on_read_only) |>
        rhot_set_visual_colheaders(esbaser::get_biologdata_colnames(pretty = TRUE))

        for (row in seq_len(nrow(df))) {
          if (df[row, "accnr"] == "") {
            for (col in which(colnames(df) != "accnr")) {
              hot <- rhandsontable::hot_cell(hot, row, col, readOnly = TRUE)
            }
          }
        }

        hot
      })
    }

    handle_details_table_update <- function(new_table) {
      if (nrow(new_table) != nrow(details_table$df)) {
        shiny::showNotification(
          paste0(
            "You tried to add or remove rows from the specified 'Antal ind.'. Please modify",
            " 'Antal ind.' to add your data."
          ), type = "warning", duration = 30)
        render_details_table()
        return()
      }

      changed <- FALSE

      for (row in seq_len(nrow(new_table))) {
        valid <- esbaser::accnr_validate(new_table[row, "accnr"])

        if (details_table$df[row, "accnr"] != new_table[row, "accnr"]) {

          if (valid || new_table[row, "accnr"] == "") {
            changed <- TRUE
            details_table$df[row, ] <- esbaser::get_accnr_biologdata(new_table[row, "accnr"])
          } else if (!valid && new_table[row, "accnr"] != "") {
            shiny::showNotification("Invalid AccNR format, please enter on the form 'A2022/12345' or 'A202212345'.", type = "warning")
            if (details_table$df[row, "accnr"] != "") {
              details_table$df[row, ] <- esbaser::get_accnr_biologdata("")
              changed <- TRUE
            }
          }
        }
      }

      if (changed) {
        selected_accnrs(details_table$df[, "accnr"])
        render_details_table()
      }
    }

    klona_accnr_fran_forsta <- function() {
      if (!esbaser::accnr_validate(details_table$df[1, "accnr"])) {
        shiny::showNotification(
          "Invalid or missing AccNR in first row. Please enter on the form 'A2022/12345' or 'A202212345'",
          type = "warning")
        return()
      }

      new_table <- details_table$df
      new_table[, "accnr"] <- details_table$df[1, "accnr"]
      handle_details_table_update(new_table)
    }

    sekvens_accnr_fran_forsta <- function() {
      if (!esbaser::accnr_validate(details_table$df[1, "accnr"])) {
        shiny::showNotification(
          "Invalid or missing AccNR in first row. Please enter on the form 'A2022/12345' or 'A202212345'",
          type = "warning")
        return()
      }

      parsed <- esbaser::accnr_parse(details_table$df[1, "accnr"])
      new_table <- details_table$df
      new_table[, "accnr"] <- unlist(
        lapply(
          seq_len(nrow(new_table)),
          function(i) {
            esbaser::accnr_sprint(esbaser::accnr_add(parsed, i - 1))
          }
        )
      )

      handle_details_table_update(new_table)
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$antal, {
      update_details_table_rowcount()
      render_details_table()
    })

    shiny::observeEvent(input$details_table, {
      new_table <- rhandsontable::hot_to_r(input$details_table)
      handle_details_table_update(new_table)
    })

    shiny::observeEvent(input$klona_accnr_fran_forsta, {
      klona_accnr_fran_forsta()
    })

    shiny::observeEvent(input$sekvens_accnr_fran_forsta, {
      sekvens_accnr_fran_forsta()
    })
  })
}
