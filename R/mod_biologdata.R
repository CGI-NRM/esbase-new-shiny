mod_biologdata_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("biologdata"),
    shiny::fluidRow(
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
      ),
      shiny::column(4,
                    shiny::textInput(
                      inputId = ns("provberedare"),
                      label = "Provberedare, enhet"
                    )
      ),
      shiny::column(4,
      )
    ),
    shiny::fluidRow(
      shiny::column(4,
                    shiny::textInput(
                      inputId = ns("lokal_output"),
                      label = "Lokal",
                      width = "100%"
                    ) |> shinyjs::disabled()
      ),
      shiny::column(4,
                    shiny::textInput(
                      inputId = ns("artnamn_output"),
                      label = "Artnamn",
                      width = "100%"
                    ) |> shinyjs::disabled()
      ),
      shiny::column(4,
                    shiny::fluidRow(
                      shiny::column(6,
                                    shiny::textInput(
                                      inputId = ns("fangstdatum_fran_output"),
                                      label = "Fångstdatum från"
                                    ) |> shinyjs::disabled()
                      ),
                      shiny::column(6,
                                    shiny::textInput(
                                      inputId = ns("fangstdatum_till_output"),
                                      label = "Fångstdatum till"
                                    ) |> shinyjs::disabled()
                      )
                    )
      )
    ),
    rhandsontable::rHandsontableOutput(ns("details_table"))
  )
}


mod_biologdata_server <- function(id, conn, selected_accnrs, accession_data_table, biologdata_table) {
  shiny::moduleServer(id, function(input, output, session) {
    loginfo("mod_biologdata.R: module server start")

    # ---------- FUNCTIONS ----------
#    update_select_inputs_with_stodlistor <- function() {
#      # Update lokaler choices from stödlista
#      lokaler <- esbaser::get_options_lokaler()
#      session$userData$stodlistor$lokaler_vector <- lokaler[, "id", drop = TRUE]
#      names(session$userData$stodlistor$lokaler_vector) <- lokaler[, "representation", drop = TRUE]
#      shiny::updateSelectizeInput(session, "lokal", choices = session$userData$stodlistor$lokaler_vector,
#                                  selected = NA, server = TRUE)
# 
#      # Update arter choices from stödlista
#      species <- esbaser::get_options_species()
#      session$userData$stodlistor$species_vector <- species[, "id", drop = TRUE]
#      names(session$userData$stodlistor$species_vector) <- species[, "representation", drop = TRUE]
#      shiny::updateSelectizeInput(session, "artnamn", choices = session$userData$stodlistor$species_vector,
#                                  selected = NA, server = TRUE)
#    }

    update_from_accession_data <- function() {
      logdebug("mod_biologdata.R - update_from_accession_data: called")
      shiny::req(accession_data_table$db)
      shiny::req(nrow(accession_data_table$db) > 0)

      # TODO: Replace with repr
      shiny::updateTextInput(inputId = "lokal_output", value = accession_data_table$db[1, "locality_id", drop = TRUE])
      shiny::updateTextInput(inputId = "artnamn_output", value = accession_data_table$db[1, "species_id", drop = TRUE])
      shiny::updateTextInput(inputId = "fangstdatum_fran_output", value = accession_data_table$db[1, "discovery_date_start", drop = TRUE])
      shiny::updateTextInput(inputId = "fangstdatum_till_output", value = accession_data_table$db[1, "discovery_date_end", drop = TRUE])
    }

    render_details_table <- function() {
      logdebug("mod_biologdata.R - render_details_table: called")
      df <- biologdata_table$db
      # Render user overrides

      cn <- colnames(biologdata_table$db)
      for (col in cn[cn != "accession_id"]) {
        rows <- !is.na(biologdata_table$override[, col, drop = TRUE])
        df[rows, col] <- biologdata_table$override[rows, col]
      }
      # TODO: Color the cells overridden with another color (orange?). Maybe only if they override another value,
      # if the df_db was NA, no color is needed?
      # Maybe the df_db values are a bit gray, df_override is full black if there was NA before, and if there was
      # a value before, it has orange background

      output$details_table <- rhandsontable::renderRHandsontable({
        hot <- rhandsontable::rhandsontable(df, rowHeaders = NULL, overflow = "visible", maxRows = nrow(df)) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
        rhandsontable::hot_col("accession_id", renderer = rhot_renderer_validate_accnr, readOnly = TRUE) |>
        rhandsontable::hot_col(which(colnames(df) != "accession_id"), renderer = rhot_renderer_gray_bg_on_read_only) |>
        rhot_set_visual_colheaders(esbaser::get_biologdata_colnames(pretty = TRUE)) |>
        rhot_disable_context_menu()

        for (row in seq_len(nrow(df))) {
          if (df[row, "accession_id"] == "") {
            for (col in which(colnames(df) != "accession_id")) {
              hot <- rhandsontable::hot_cell(hot, row, col, readOnly = TRUE)
            }
          }
        }

        hot
      })
    }

    handle_changed_accnrs <- function() {
      logdebug("mod_biologdata.R - handle_changed_accnrs: called")
      biologdata_table$db <- tibble(accession_id = selected_accnrs())
      biologdata_table$override <- biologdata_table$db

      # TODO: Load mammal/fish/clam etc table based on accnrs
      # TODO: Create empty override table

      render_details_table()
      #handle_biologdata_table_update(df)
    }

    handle_biologdata_table_update <- function(new_table) {
      logdebug("mod_biologdata.R - handle_biologdata_table_update: called")
      if (nrow(new_table) != nrow(biologdata_table$db) || nrow(new_table) != nrow(biologdata_table$override)) {
        shiny::showNotification(
          paste0(
            "You tried to add or remove rows from the specified 'Antal ind.'. Please modify",
            " 'Antal ind.' to add your data."
          ), type = "warning", duration = 30)
        render_details_table()
        return()
      }

      changed <- FALSE
      cn <- colnames(new_table)

      # rhandsontable converts all facotrs to ordered factors, convert them back according to what is already in the df_override df
      for (col in cn) {
        if (is.factor(new_table[, col])) {
          new_table[, col] <- factor(new_table[, col], levels = levels(biologdata_table$override[, col]), ordered = FALSE)
        }
      }

      for (col in cn[cn != "accession_id"]) {
        rows_changed <- biologdata_table$db[col] != new_table[col]
        rows_changed <- rows_changed | xor(is.na(biologdata_table$db[col]), is.na(new_table[col]))
        rows_changed <- rows_changed & !rows_replaced_accnr

        rows <- which(rows_changed)
        biologdata_table$override[rows, col] <- new_table[rows, col]

        if (length(rows) > 0) {
          changed <- TRUE
        }
      }

      if (changed) {
        render_details_table()
      }
    }

    # ---------- ONE-TIME SETUP ----------
    # update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$details_table, {
      new_table <- rhandsontable::hot_to_r(input$details_table)
      handle_biologdata_table_update(new_table)
    })

    shiny::observeEvent(selected_accnrs(), {
      handle_changed_accnrs()
    })

    shiny::observeEvent(accession_data_table$db, {
      update_static_data_from_accession_data()
    }, ignoreNULL = FALSE)
  })
}
