mod_biologdata_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns("biologdata"),
             shiny::h3("Biologdata"),
             shiny::fluidRow(
               shiny::column(4,
                             shiny::selectizeInput(
                               inputId = ns("lokal"),
                               label = "Lokal",
                               choices = c(""),
                               options = list(placeholder = "Lokal"),
                               width = "100%"
                             ),
                             shiny::fluidRow(
                               shiny::column(6,
                                             shiny::selectizeInput(
                                               inputId = ns("artnamn"),
                                               label = "Artnamn (sv)",
                                               choices = c(""),
                                               options = list(placeholder = "Artnamn"),
                                               width = "100%"
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
                                           shiny::selectizeInput(
                                             inputId = ns("projekt"),
                                             label = "Projekt /Program",
                                             choices = c(""),
                                             options = list(placeholder = "Projekt"),
                                             width = "100%"
                                           ),
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
                                               inputId = ns("sekvens_accnr_fran_forsta"),
                                               label = "Sekvens av AccNR från första")
                                             )
}


mod_biologdata_server <- function(id, selected_accnrs, biologdata_table) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---------- FUNCTIONS ----------
    update_select_inputs_with_stodlistor <- function() {
      # Update lokaler choices from stödlista
      lokaler <- esbaser::get_options_lokaler()
      lokaler_vector <- lokaler[, "id", drop = TRUE]
      names(lokaler_vector) <- lokaler[, "representation", drop = TRUE]
      shiny::updateSelectizeInput(session, "lokal", choices = lokaler_vector,
                                  selected = NA, server = TRUE)

      # Update arter choices from stödlista
      species <- esbaser::get_options_species()
      species_vector <- species[, "id", drop = TRUE]
      names(species_vector) <- species[, "representation", drop = TRUE]
      shiny::updateSelectizeInput(session, "artnamn", choices = species_vector,
                                  selected = NA, server = TRUE)

      # Update project choices from stödlista
      projects <- esbaser::get_options_project()
      projects_vector <- projects[, "id", drop = TRUE]
      names(projects_vector) <- projects[, "representation", drop = TRUE]
      shiny::updateSelectizeInput(session, "projekt", choices = projects_vector,
                                  selected = NA, server = TRUE)
    }

    update_biologdata_table_rowcount <- function() {
      if (is.na(input$antal) || input$antal < 0) {
        return()
      }

      if (NROW(biologdata_table$df_db) < input$antal) {
        new_rows <- dplyr::bind_rows(lapply(rep("", input$antal - NROW(biologdata_table$df_db)), esbaser::get_accnr_biologdata))
        biologdata_table$df_db <- rbind(biologdata_table$df_db, new_rows)
        biologdata_table$df_override <- rbind(biologdata_table$df_override, new_rows)
      } else {
        biologdata_table$df_db <- biologdata_table$df_db[1:input$antal, ]
        biologdata_table$df_override <- biologdata_table$df_override[1:input$antal, ]
      }

      selected_accnrs(biologdata_table$df_db[, "accnr"])
    }

    render_details_table <- function() {
      shiny::req(shiny::isolate(biologdata_table$df_db))
      df <- shiny::isolate(biologdata_table$df_db)
      # Render user overrides

      cn <- colnames(biologdata_table$df_db)
      for (col in cn[cn != "accnr"]) {
        rows <- !is.na(biologdata_table$df_override[, col, drop = TRUE])
        df[rows, col] <- biologdata_table$df_override[rows, col]
      }
      # TODO: Color the cells overridden with another color (orange?). Maybe only if they override another value,
      # if the df_db was NA, no color is needed?
      # Maybe the df_db values are a bit gray, df_override is full black if there was NA before, and if there was
      # a value before, it has orange background

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

    handle_biologdata_table_update <- function(new_table) {
      if (nrow(new_table) != nrow(biologdata_table$df_db) || nrow(new_table) != nrow(biologdata_table$df_override)) {
        shiny::showNotification(
          paste0(
            "You tried to add or remove rows from the specified 'Antal ind.'. Please modify",
            " 'Antal ind.' to add your data."
          ), type = "warning", duration = 30)
        render_details_table()
        return()
      }

      changed <- FALSE

      rows_replaced_accnr <- rep(FALSE, nrow(new_table))
      for (row in seq_len(nrow(new_table))) {
        valid <- esbaser::accnr_validate(new_table[row, "accnr"])

        if (biologdata_table$df_db[row, "accnr"] != new_table[row, "accnr"]) {

          if (valid || new_table[row, "accnr"] == "") {
            changed <- TRUE
            rows_replaced_accnr[row] <- TRUE
            biologdata_table$df_db[row, ] <- esbaser::get_accnr_biologdata(new_table[row, "accnr"])
            biologdata_table$df_override[row, ] <- esbaser::get_accnr_biologdata("")
            biologdata_table$df_override[row, "accnr"] <- new_table[row, "accnr"]
          } else if (!valid && new_table[row, "accnr"] != "") {
            shiny::showNotification("Invalid AccNR format, please enter on the form 'A2022/12345' or 'A202212345'.", type = "warning")
            if (biologdata_table$df_db[row, "accnr"] != "") {
              biologdata_table$df_db[row, ] <- esbaser::get_accnr_biologdata("")
              biologdata_table$df_override[row, ] <- esbaser::get_accnr_biologdata("")
              changed <- TRUE
              rows_replaced_accnr[row] <- TRUE
            }
          }
        }
      }

      # TODO: Solve this for factor colmuns
      # There is a problem where the handsontable seems to convert NA factors into ordered
      cn <- colnames(new_table)
      for (col in cn[cn != "accnr" & cn != "kon" & cn != "gonad_sparad" & cn != "mage_sparad"]) {
        rows_changed <- biologdata_table$df_db[col] != new_table[col]
        rows_changed <- rows_changed | xor(is.na(biologdata_table$df_db[col]), is.na(new_table[col]))
        rows_changed <- rows_changed & !rows_replaced_accnr

        rows <- which(rows_changed)
        biologdata_table$df_override[rows, col] <- new_table[rows, col]

        if (length(rows) > 0) {
          changed <- TRUE
        }
      }

      if (changed) {
        selected_accnrs(biologdata_table$df_db[, "accnr"])
        render_details_table()
      }
    }

    sekvens_accnr_fran_forsta <- function() {
      if (!esbaser::accnr_validate(biologdata_table$df_db[1, "accnr"])) {
        shiny::showNotification(
          "Invalid or missing AccNR in first row. Please enter on the form 'A2022/12345' or 'A202212345'",
          type = "warning")
        return()
      }

      parsed <- esbaser::accnr_parse(biologdata_table$df_db[1, "accnr"])
      new_table <- biologdata_table$df_db
      new_table[, "accnr"] <- unlist(
        lapply(
          seq_len(nrow(new_table)),
          function(i) {
            esbaser::accnr_sprint(esbaser::accnr_add(parsed, i - 1))
          }
        )
      )

      handle_biologdata_table_update(new_table)
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$antal, {
      update_biologdata_table_rowcount()
      render_details_table()
    })

    shiny::observeEvent(input$details_table, {
      new_table <- rhandsontable::hot_to_r(input$details_table)
      handle_biologdata_table_update(new_table)
    })

    shiny::observeEvent(input$sekvens_accnr_fran_forsta, {
      sekvens_accnr_fran_forsta()
    })
  })
}
