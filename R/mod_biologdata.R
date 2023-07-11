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
                      choices = c(" ")
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
    shinyBS::bsCollapse(
      shinyBS::bsCollapsePanel("Lägg till material",
                               shiny::p("Lägg till material att hantera vikter eller provbereda."),
                               shiny::fluidRow(
                                 shiny::column(4,
                                               shiny::selectizeInput(
                                                 inputId = ns("add_material_vavnad"),
                                                 label = "Materialtyp",
                                                 options = list(placeholder = "Materialtyp", highlight = FALSE),
                                                 choices = c("")
                                               )
                                 ),
                                 shiny::column(4,
                                               shiny::selectizeInput(
                                                 inputId = ns("add_material_storage"),
                                                 label = "Förvaringsplats",
                                                 options = list(placeholder = "Förvaringsplats", highlight = FALSE),
                                                 choices = c("")
                                               )
                                 ),
                                 shiny::column(4,
                                               shiny::actionButton(
                                                 inputId = ns("add_material"),
                                                 label = "Lägg till"
                                               )
                                 )
                               )
      )
    ),
    rhandsontable::rHandsontableOutput(ns("details_table"))
  )
}

mod_biologdata_server <- function(id, db, selected, biologdata, added_material) {

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

      material_type_vector <- db$material_type |> select(id) |> unlist(use.names = FALSE)
      names(material_type_vector) <- db$material_type |> select(swe_name) |> apply(1, paste_collapse)
      material_type_vector <- material_type_vector[names(material_type_vector) != ""]
      shiny::updateSelectizeInput(session, "add_material_vavnad", choices = material_type_vector,
                                  selected = NA, server = FALSE)

      material_storage <- db$material_storage |> arrange()
      material_storage_vector <- db$material_storage |> select(id) |> unlist(use.names = FALSE)
      names(material_storage_vector) <- db$material_storage |> select(name) |> apply(1, paste_collapse)
      material_storage_vector <- material_storage_vector[names(material_storage_vector) != ""]
      shiny::updateSelectizeInput(session, "add_material_storage", choices = material_storage_vector,
                                  selected = NA, server = FALSE)

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
          rhandsontable::rhandsontable(data.frame()) |>
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
        ret <- create_biologdata_table(selected, db, added_material)
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

      changed <- any(xor(is.na(biologdata$override), is.na(new_table)) | isTRUE(biologdata$override != new_table))

      if (changed) {
        biologdata$override <- new_table
        render_details_table()
      }

      logfine("mod_biologdata.R - handle_biologdata_table_update: finished")
    }

    add_material <- function() {
      logdebug("mod_biologdata.R - add_material: called")
      if (input$add_material_vavnad == "") {
        shiny::showNotification("Du måste välja en materialtyp att lägga till.", duration = 10)
        return()
      }

      storage_id <- ifelse(input$add_material_storage == "", 0, as.integer(input$add_material_storage))
      type_id <- as.integer(input$add_material_vavnad)

      if (added_material$mats |> filter(type_id == !!type_id, storage_id == !!storage_id) |> nrow() > 0) {
        shiny::showNotification(
          paste0(
            "'",
            db$material_type |> filter(id == type_id) |> select(swe_name) |> unlist(use.names = FALSE),
            "' lagrat i '",
            db$material_storage |> filter(id == storage_id) |> select(name) |> unlist(use.names = FALSE),
            "' finns redan och kan inte läggas till igen. Detta fall måste specialhanteras."
          ),
          duration = 10
        )
        return()
      }

      added_material$mats <- rbind(
        added_material$mats,
        tibble(
          type_id = type_id,
          storage_id = storage_id
        )
      )
      added_material$update(added_material$update() + 1)

      logfine("mod_biologdata.R - add_material: finished")
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

    shiny::observeEvent(input$add_material, {
      add_material()
      handle_changed_accnrs()
    })
  })
}
