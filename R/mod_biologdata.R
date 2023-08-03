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
                    shiny::selectizeInput(
                      inputId = ns("provberedare"),
                      label = "Provberedare, enhet",
                      options = list(placeholder = "Provberedare", highlight = FALSE),
                      choices = c("")
                    )
      ),
      shiny::column(4,
      )
    ),
    shiny::fluidRow(
      shiny::column(4,
                    shiny::textAreaInput(
                      inputId = ns("lokal_output"),
                      label = "Lokal",
                      width = "100%",
                      resize = "vertical"
                    ) |> shinyjs::disabled()
      ),
      shiny::column(4,
                    shiny::textAreaInput(
                      inputId = ns("artnamn_output"),
                      label = "Artnamn",
                      width = "100%",
                      resize = "vertical"
                    ) |> shinyjs::disabled()
      ),
      shiny::column(4,
                    shiny::fluidRow(
                      shiny::column(6,
                                    shiny::textInput(
                                      inputId = ns("fangst_fran_output"),
                                      label = "Fångstdatum från"
                                    ) |> shinyjs::disabled()
                      ),
                      shiny::column(6,
                                    shiny::textInput(
                                      inputId = ns("fangst_till_output"),
                                      label = "Fångstdatum till"
                                    ) |> shinyjs::disabled()
                      )
                    )
      )
    ),
    rhandsontable::rHandsontableOutput(ns("details_table")),
    shiny::br(),
    shiny::actionButton(inputId = ns("save"), label = "Spara biologdata")
  )
}

mod_biologdata_server <- function(id, db, account, selected, biologdata) {

  shiny::moduleServer(id, function(input, output, session) {
    loginfo("mod_biologdata.R: module server start")

    # ---------- FUNCTIONS ----------
    update_select_inputs_with_stodlistor <- function() {
      logdebug("mod_biologdata.R - update_select_inputs_with_stodlistor: called")

      # Update person choices from stödlista
      person_vector <- db$person |> select(id) |> unlist(use.names = FALSE)
      names(person_vector) <- db$person |> select(institution, firstname, lastname, town) |> apply(1, paste_collapse, collapse = " ")
      person_vector <- person_vector[names(person_vector) != ""]
      shiny::updateSelectizeInput(session, "provberedare", choices = person_vector,
                                  selected = NA, server = TRUE)

      logfine("mod_biologdata.R - update_select_inputs_with_stodlistor: finished")
    }

    update_static_data_from_accession_data <- function() {
      logdebug("mod_biologdata.R - update_static_data_from_accession_data: called")

      if (!is.null(selected$acc) && nrow(selected$acc) > 0) {
        shiny::updateTextAreaInput(inputId = "lokal_output", value = selected$acc[1, "locality_id"] |> repr_locality(db))
        shiny::updateTextAreaInput(inputId = "artnamn_output", value = selected$acc[1, "species_id"] |> repr_species(db))
        shiny::updateTextInput(inputId = "fangst_fran_output", value = selected$acc[1, "discovery_date_start", drop = TRUE])
        shiny::updateTextInput(inputId = "fangst_till_output", value = selected$acc[1, "discovery_date_end", drop = TRUE])
      } else {
        shiny::updateTextAreaInput(inputId = "lokal_output", value = "")
        shiny::updateTextAreaInput(inputId = "artnamn_output", value = "")
        shiny::updateTextInput(inputId = "fangst_fran_output", value = "")
        shiny::updateTextInput(inputId = "fangst_till_output", value = "")
      }
      logfine("mod_biologdata.R - update_static_data_from_accession_data: finished")
    }

    render_details_table <- function() {
      logdebug("mod_biologdata.R - render_details_table: called")

      shiny::req(biologdata$override)

      if (isFALSE("acc.id" %in% colnames(biologdata$override))) {
        output$details_table <- rhandsontable::renderRHandsontable(
          rhandsontable::rhandsontable(data.frame(), rowHeaders = FALSE, overflow = "visible", maxRows = 0) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
          rhot_disable_context_menu()
        )
        return()
      }

      df <- biologdata$override

      # TODO: Color the cells overridden with another color (orange?). Maybe only if they override another value,
      # if the df_db was NA, no color is needed?
      # Maybe the df_db values are a bit gray, df_override is full black if there was NA before, and if there was
      # a value before, it has orange background

      output$details_table <- rhandsontable::renderRHandsontable({
        hot <- (
          rhandsontable::rhandsontable(df, rowHeaders = NULL, overflow = "visible", maxRows = nrow(df)) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
          rhandsontable::hot_col("acc.id", renderer = rhot_renderer_validate_accnr, readOnly = TRUE) |>
          rhandsontable::hot_col(which(colnames(df) != "acc.id"), renderer = rhot_renderer_gray_bg_on_read_only) |>
          rhot_set_visual_colheaders(biologdata$colnames) |>
          rhot_disable_context_menu())

        for (row in seq_len(nrow(df))) {
          if (df[row, "acc.id"] == "") {
            for (col in which(colnames(df) != "acc.id")) {
              hot <- rhandsontable::hot_cell(hot, row, col, readOnly = TRUE)
            }
          }
        }

        for (col in names(biologdata$formats)) {
          hot <- rhandsontable::hot_col(hot, col, format = biologdata$formats[[col]])
        }

        hot
      })
      logfine("mod_biologdata.R - render_details_table: finished")
    }

    handle_changed_accnrs <- function() {
      logdebug("mod_biologdata.R - handle_changed_accnrs: called")

      if (is.null(selected$acc) || nrow(selected$acc) == 0) {
        biologdata$df <- tibble()
        biologdata$formats <- character(0)
        biologdata$colnames <- character(0)
        biologdata$override <- tibble()
      } else {
        ret <- create_biologdata_table(db = db, selected = selected)
        biologdata$df <- ret$df
        biologdata$formats <- ret$formats
        biologdata$colnames <- ret$colnames
        biologdata$override <- biologdata$df
      }

      render_details_table()
      logfine("mod_biologdata.R - handle_changed_accnrs: finished")
    }

    handle_biologdata_table_update <- function(new_table) {
      logdebug("mod_biologdata.R - handle_biologdata_table_update: called")

      if (nrow(new_table) != nrow(biologdata$override) || nrow(new_table) != nrow(biologdata$override)) {
        shiny::showNotification(
          "Kan inte lägga till eller ta bort rader i tabellen.",
          type = "warning", duration = 30)
        render_details_table()
        return()
      }

      changed <- FALSE
      cn <- colnames(new_table)

      # rhandsontable converts all facotrs to ordered factors, convert them back according to what is already in the df_override df
      for (col in cn) {
        if (is.factor(new_table[, col])) {
          new_table[, col] <- factor(new_table[, col], levels = levels(biologdata$override[, col, drop = TRUE]), ordered = FALSE)
        }
      }

      changed <- any(xor(is.na(biologdata$override), is.na(new_table)) | unlist(lapply(biologdata$override != new_table, isTRUE)))

      if (changed) {
        biologdata$override <- new_table
        render_details_table()
      }

      logfine("mod_biologdata.R - handle_biologdata_table_update: finished")
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$details_table, {
      new_table <- rhandsontable::hot_to_r(input$details_table)
      handle_biologdata_table_update(new_table)
    })

    shiny::observeEvent(selected$update(), {
      handle_changed_accnrs()
      update_static_data_from_accession_data()
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$save, {
      logdebug("mod_biologdata.R - observeEvent(input$save, {}): called")
      if (is.null(biologdata$override) || nrow(biologdata$override) == 0) {
        shiny::showNotification("Inga valda accessionsnummer", duration = 10, type = "message")
        return()
      }

      update_biologdata_overrides(db, selected, biologdata$override)
      catalog_id <- selected$acc |> select(catalog_id) |> unlist(use.names = FALSE) |> first()
      if (catalog_id == 2) {
        esbaser::update_fish(db$conn, account$id, selected$bio_override)
        esbaser::update_specimen(db$conn, account$id, selected$specimen_override)
        selected$bio <- selected$bio_override
        selected$specimen <- selected$specimen_override
        shiny::showNotification("Biologdata sparat", duration = 10, type = "message")
      } else {
        shiny::showNotification("Can only save fish to database", duration = 10, type = "error")
      }
      logfine("mod_biologdata.R - observeEvent(input$save, {}): finished")
    })
  })
}
