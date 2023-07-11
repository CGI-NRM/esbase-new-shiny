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
    shiny::wellPanel(
    ),
    rhandsontable::rHandsontableOutput(ns("material_table"))
  )
}

mod_material_server <- function(id, db, selected) {
  shiny::moduleServer(id, function(input, output, session) {
    loginfo("mod_material.R: module server start")

    # ---------- FUNCTIONS ----------
    render_material_table <- function() {
      logdebug("mod_material.R - render_material_table: called")
      df <- (selected$material |> select(
          id, accession_id, material_type_id, amount_original,
          amount_left, storage_id, storage_type_id, storage_note) |>
        filter(material_type_id %in% input$filter_material_type | length(input$filter_material_type) == 0,
               storage_type_id %in% input$filter_storage_type | length(input$filter_storage_type) == 0)
      )

      output$material_table <- rhandsontable::renderRHandsontable(
        rhandsontable::rhandsontable(df, rowHeaders = FALSE, overflow = "visible", maxRows = nrow(df)) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
        rhandsontable::hot_col(c("id", "accession_id"), readOnly = TRUE)
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

      material_storage <- db$material_storage |> arrange(sortbyme)
      material_storage_vector <- db$material_storage |> select(id) |> unlist(use.names = FALSE)
      names(material_storage_vector) <- db$material_storage |> select(name) |> apply(1, paste_collapse)
      material_storage_vector <- material_storage_vector[names(material_storage_vector) != ""]
      shiny::updateSelectizeInput(session, "filter_storage_type", choices = material_storage_vector,
                                  selected = NA, server = FALSE)

      logfine("mod_material.R - update_select_inputs_with_stodlistor: finished")
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(selected$update(), {
      logdebug("mod_provlista.R - observeEvent(selected$update(), {}): called")
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
  })
}
