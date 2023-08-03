mod_material_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    id = ns("material"),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectizeInput(inputId = ns("filter_material_type"),
                                          label = "Materialtyp",
                                          choices = c(""),
                                          options = list(placeholder = "Filtrera materialtyp", highlight = FALSE),
                                          multiple = TRUE
                    )
      ),
      shiny::column(6,
                    shiny::selectizeInput(inputId = ns("filter_storage_type"),
                                          label = "Förvarningstyp",
                                          choices = c(""),
                                          options = list(placeholder = "Filtrera förvaringstyp", highlight = FALSE),
                                          multiple = TRUE
                    )
      )
    ),
    shinyBS::bsCollapse(
      shinyBS::bsCollapsePanel(
        "Lägg till material",
        shiny::fluidRow(
          shiny::column(6,
                        shiny::selectizeInput(inputId = ns("add_material_accessionsnummer"),
                                              label = "Accessionsnummer",
                                              choices = c("Alla"),
                                              options = list(placeholder = "Välj Accessionsnummer", highlight = FALSE),
                                              multiple = FALSE)
          ),
          shiny::column(6,
                        shiny::selectizeInput(inputId = ns("add_material_type"),
                                              label = "Materialtyp",
                                              choices = c(""),
                                              options = list(placeholder = "Välj Materialtyp", highlight = FALSE),
                                              multiple = FALSE)
          )
        ),
        shinyBS::tipify(
          shiny::actionButton(inputId = ns("add_material"), label = "Lägg till material"),
          title = "Att lägga till material återställer osparande ändringar",
          placement = "top"
        )
      ),
      shinyBS::bsCollapsePanel(
        "Förvaringsuppgifter",
        shiny::fluidRow(
          shiny::column(3,
                        shiny::selectizeInput(inputId = ns("change_storage_type"),
                                              label = "Förvaringsplats",
                                              choices = c(""))
          ),
          shiny::column(3,
                        shiny::numericInput(inputId = ns("change_storage_id"), label = "Lådnummer", value = NULL, min = 0, step = 1)
          ),
          shiny::column(6,
                        shiny::textInput(inputId = ns("change_storage_note"), label = "Förv. kommentar")
          )
        ),
        shiny::actionButton(inputId = ns("change_storage"),
                            label = "Ändra hela listan"),
        shiny::actionButton(inputId = ns("change_storage_undo"),
                            label = "Ångra") |> shinyjs::disabled()
      )
    ),
    rhandsontable::rHandsontableOutput(ns("material_table")),
    shiny::br(),
    shiny::actionButton(inputId = ns("save_changes"), label = "Spara material/inventering")
    )
}

mod_material_server <- function(id, db, account, selected) {
  shiny::moduleServer(id, function(input, output, session) {
    loginfo("mod_material.R: module server start")

    # ---------- FUNCTIONS ----------
    render_material_table <- function() {
      logdebug("mod_material.R - render_material_table: called")
      if (is.null(selected$material_override) || nrow(selected$material_override) == 0) {
        output$material_table <- rhandsontable::renderRHandsontable(
          rhandsontable::rhandsontable(data.frame(), rowHeaders = FALSE, overflow = "visible", maxRows = 0) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
          rhot_disable_context_menu()
        )
        return()
      }

      df <- (selected$material_override |>
             filter(material_type_id %in% input$filter_material_type | length(input$filter_material_type) == 0,
                    storage_type_id %in% input$filter_storage_type | length(input$filter_storage_type) == 0) |>
             left_join(selected$acc |> rename_w_prefix("acc."), by = join_by(accession_id == acc.id)) |>
             left_join(db$species |> rename_w_prefix("species."), by = join_by(acc.species_id == species.id))
      )

      df[, "accnr"] <- esbaser::accdb_to_accnr(df[, "accession_id", drop = TRUE])
      df[, "material_type.swe_name"] <- df |> select(material_type_id) |> unlist(use.names = FALSE) |> factor(
        levels = db$material_type |> select(id) |> unlist(use.names = FALSE),
        labels = db$material_type |> select(swe_name) |> unlist(use.names = FALSE))

      material_storage <- db$material_storage |> arrange(sortbyme)
      df[, "material_storage.name"] <- df |> select(storage_type_id) |> unlist(use.names = FALSE) |> factor(
        levels = material_storage |> select(id) |> unlist(use.names = FALSE),
        labels = material_storage |> select(name) |> unlist(use.names = FALSE))

      df <- (df |> select(id, accnr, species.swe_name, material_type.swe_name, amount_original, amount_left, material_storage.name,
                          storage_id, storage_note) |>
             arrange(accnr) |>
             as.data.frame())

      column_names <- c("Id", "Accessions nummer", "Art", "Materialtyp", "Sparad (g, st)", "Mängd kvar (g, st)",
                    "Förvaringsplats", "Lådnummer", "Förv. kommentar")

      output$material_table <- rhandsontable::renderRHandsontable(
        rhandsontable::rhandsontable(df, rowHeaders = FALSE, overflow = "visible", maxRows = nrow(df)) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
        rhandsontable::hot_col(c("id", "accnr", "species.swe_name"), readOnly = TRUE) |>
        rhandsontable::hot_col(c("amount_original", "amount_left"), format = "0") |>
        rhandsontable::hot_col("id", colWidths = 1) |>
        rhot_set_visual_colheaders(column_names) |>
        rhot_disable_context_menu()
      )
      logfine("mod_material.R - render_material_table: finished")
    }

    update_select_inputs_with_stodlistor <- function() {
      logdebug("mod_material.R - update_select_inputs_with_stodlistor: called")

      material_type_vector <- db$material_type |> select(id) |> unlist(use.names = FALSE)
      names(material_type_vector) <- db$material_type |> select(swe_name) |> apply(1, paste_collapse)
      material_type_vector <- material_type_vector[names(material_type_vector) != ""]
      shiny::updateSelectizeInput(session, "filter_material_type", choices = material_type_vector,
                                  selected = NA, server = FALSE)
      shiny::updateSelectizeInput(session, "add_material_type", choices = material_type_vector,
                                  selected = NA, server = FALSE)

      material_storage <- db$material_storage |> arrange(sortbyme)
      material_storage_vector <- db$material_storage |> select(id) |> unlist(use.names = FALSE)
      names(material_storage_vector) <- db$material_storage |> select(name) |> apply(1, paste_collapse)
      material_storage_vector <- material_storage_vector[names(material_storage_vector) != ""]
      shiny::updateSelectizeInput(session, "filter_storage_type", choices = material_storage_vector,
                                  selected = NA, server = FALSE)
      shiny::updateSelectizeInput(session, "change_storage_type", choices = material_storage_vector,
                                  selected = NA, server = FALSE)

      logfine("mod_material.R - update_select_inputs_with_stodlistor: finished")
    }

    handle_material_table_changed <- function(new_table) {
      logdebug("mod_material.R - handle_material_table_changed: called")
      if (is.null(new_table) || nrow(new_table) == 0) {
        selected$material_override <- selected$material # Might not be needed, but safer to clear here and make sure the render in clean
        return()
      }

      new_table <- (
        new_table |>
        left_join(db$material_type |> rename_w_prefix("material_type."), by = join_by(material_type.swe_name)) |>
        left_join(db$material_storage |> rename_w_prefix("material_storage."), by = join_by(material_storage.name))
        )

      for (row in rownames(new_table)) {
        id <- new_table[row, "id"]
        row_mask <- selected$material_override |> select(id) |> unlist(use.names = FALSE) == id
        selected$material_override[row_mask, c(
          "material_type_id", "amount_original", "amount_left",
          "storage_type_id", "storage_id", "storage_note")] <-
        new_table[row, c(
          "material_type.id", "amount_original", "amount_left",
          "material_storage.id", "storage_id", "storage_note")]
      }

      logfine("mod_material.R - handle_material_table_changed: finished")
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(selected$update(), {
      logdebug("mod_provlista.R - observeEvent(selected$update(), {}): called")
      accnrs <- selected$accs_db
      names(accnrs) <- esbaser::accdb_to_accnr(accnrs)
      updateSelectizeInput(session, "add_material_accessionsnummer", choices = c("Alla" = "all", accnrs),
                           selected = "all", server = FALSE)
      render_material_table()
      logfine("mod_provlista.R - observeEvent(selected$update(), {}): finished")
    })

    shiny::observeEvent({
      input$filter_material_type
      input$filter_storage_type
      1
    }, {
      logdebug("mod_provlista.R - observeEvent({input$filter_material_type; input$filter_storage_type; 1}, {}): called")
      render_material_table()
      logfine("mod_provlista.R - observeEvent({input$filter_material_type; input$filter_storage_type; 1}, {}): finished")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$material_table, {
      logdebug("mod_provlista.R - observeEvent(input$material_table, {}): called")
      new_table <- rhandsontable::hot_to_r(input$material_table)
      handle_material_table_changed(new_table)
      logfine("mod_provlista.R - observeEvent(input$material_table, {}): finished")
    })

    shiny::observeEvent(input$add_material, {
      logdebug("mod_provlista.R - observeEvent(input$add_material, {}): called")
      shiny::req(input$add_material_accessionsnummer)
      shiny::req(input$add_material_type)

      accdbs <- input$add_material_accessionsnummer
      if (accdbs == "all") {
        accdbs <- selected$accs_db
      }

      shiny::req(length(accdbs) > 0)

      esbaser::insert_new_material(db$conn, account$id, accdbs, input$add_material_type)
      selected$material <- esbaser::get_material_between(db$conn, selected$acc_min, selected$acc_max)
      selected$material_override <- selected$material
      selected$material_override_backup <- tibble()
      render_material_table()

      selected$update(selected$update() + 1)
      logfine("mod_provlista.R - observeEvent(input$add_material, {}): finished")
    })

    shiny::observeEvent(input$change_storage, {
      logdebug("mod_provlista.R - observeEvent(input$change_storage, {}): called")
      shiny::req(!is.null(selected$material_override) && nrow(selected$material_override) > 0)
      selected$material_override_backup <- selected$material_override

      row_mask <- ((selected$material_override$material_type_id %in% input$filter_material_type | length(input$filter_material_type) == 0) &
                   (selected$material_override$storage_type_id %in% input$filter_storage_type | length(input$filter_storage_type) == 0))

      selected$material_override[row_mask, c("storage_type_id", "storage_id", "storage_note")] <- tibble(
        storage_type_id = as.integer(input$change_storage_type),
        storage_id = as.integer(input$change_storage_id),
        storage_note = input$change_storage_note
      )

      shinyjs::enable("change_storage_undo")

      selected$update(selected$update() + 1)
      logfine("mod_provlista.R - observeEvent(input$change_storage, {}): finished")
    })

    shiny::observeEvent(input$change_storage_undo, {
      logdebug("mod_provlista.R - observeEvent(input$change_storage_undo, {}): called")
      shiny::req(!is.null(selected$material_override_backup) && nrow(selected$material_override_backup) > 0)
      selected$material_override <- selected$material_override_backup
      selected$update(selected$update() + 1)
      shinyjs::disable("change_storage_undo")
      logfine("mod_provlista.R - observeEvent(input$change_storage_undo, {}): finished")
    })

    shiny::observeEvent(input$save_changes, {
      logdebug("mod_provlista.R - observeEvent(input$save_changes, {}): called")
      shiny::req(!is.null(selected$material_override) && nrow(selected$material_override) > 0)
      esbaser::update_material(db$conn, account$id, selected$material_override)
      selected$material <- selected$material_override
      selected$material_override_backup <- tibble()
      selected$update(selected$update() + 1)
      shiny::showNotification("Materialdata sparat", duration = 10, type = "message")
      logfine("mod_provlista.R - observeEvent(input$save_changes, {}): finished")
    })
  })
}
