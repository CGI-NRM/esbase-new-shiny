mod_provlista_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns("provlista"),
             shiny::h3("Provlista"),
             provlist_ui(create_prov_ns("prov1", ns)),
             shiny::div(id = ns("after_provs")),
             shiny::actionButton(inputId = ns("lagg_till_prov"), label = "Lägg till prov"),
             shiny::br(),
             shiny::actionButton(inputId = ns("set_limniska"), label = "Set Limniska Programmet Prover", disabled = TRUE)
  )
}

mod_provlista_server <- function(id, selected_accnrs) {
  shiny::moduleServer(id, function(input, output, session) {
    # ---------- REACTIVE VARIABLES ----------
    # Containing provid_table$dfs which is a list where the keys are the names of the prov, and the values are the coresponding dataframe
    provid_table <- shiny::reactiveValues()
    # A vector of the names of all provs
    provs <- shiny::reactiveVal()
    # A list/reactiveValues where the keys hold vectors of all saved observeEvents, so that they can be deleted
    provs_observe_events <- shiny::reactiveValues()

    # ---------- DEFAULT VALUES ----------
    provid_table$dfs <- list()
    provs(c("prov1"))

    # ---------- FUNCTIONS ----------
    # Create a shorthand for generating the custom-namespaced id:s for prov with a certain name
    prov_io <- function(name, id) {
      paste0(name, "_", id)
    }

    provid_table_cols <- c("accnr", "provid", "aces", "delvikt", "provvikt")
    provid_table_cols_pretty <- c("Acc.nr.", "ProvID", "ACES NR", "Delvikt (g)", "Provvikt (g)")

    # Create a new provid_table dataframe with default/empty values and place in provid_table
    create_provid_table <- function(name) {
      provid_table$dfs[[name]] <- data.frame(
        accnr = selected_accnrs(),
        provid = "",
        aces = "",
        delvikt = as.numeric(NA),
        provvikt = as.numeric(NA))
    }

    handle_provid_table_change <- function(name, new_table) {
      if (is.null(provid_table$dfs[[name]]) || nrow(new_table) != nrow(provid_table$dfs[[name]])) {
        shiny::showNotification(
          paste0(
            "Cannot add rows to this table. Add more rows under 'Biologdata' to increase the number of rows in this table"
          ), type = "warning", duration = 30)
        render_provid_table(name)
        return()
      }
      changed <- FALSE
      for (col in colnames(new_table)) {
        cells_changed <- provid_table$dfs[[name]][col] != new_table[col]
        cells_changed <- cells_changed | xor(is.na(provid_table$dfs[[name]][col]), is.na(new_table[col]))

        rows <- which(cells_changed)
        provid_table$dfs[[name]][rows, col] <- new_table[rows, col]

        if (length(rows)) {
          changed <- TRUE
        }
      }
      if (changed) {
        render_provid_table(name)
      }
    }

    render_provid_table <- function(name) {
      if (is.null(input[[prov_io(name, "homogenat")]]) ||
          is.null(input[[prov_io(name, "analyslab")]])) {
        shiny::showNotification(
          paste0(
            "Cannot render provid_table for ",
            name,
            " due to missing UI input. Please submit a ",
            "ticket to https://github.com/CGI-NRM/esbase-new-shiny/ ",
            "with step steps to produce this issue."
          ), type = "error", duration = 20)
        return()
      }

      cols <- c("accnr", "provid")
      if (input[[prov_io(name, "analyslab")]] == "ACES") {
        cols <- c(cols, "aces")
      }
      if (input[[prov_io(name, "homogenat")]]) {
        cols <- c(cols, "delvikt")
      }
      cols <- c(cols, "provvikt")

      df <- provid_table$dfs[[name]][cols]

      output[[prov_io(name, "provid_table")]] <- rhandsontable::renderRHandsontable({
        hot <- rhandsontable::rhandsontable(df, rowHeaders = FALSE, overflow = "visible", maxRows = nrow(df)) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
        rhandsontable::hot_col("accnr", readOnly = TRUE) |>
        rhandsontable::hot_col("provid", renderer = rhot_renderer_validate_provid_gray_bg_on_read_only) |>
        rhandsontable::hot_col(which(colnames(df) != "provid"), renderer = rhot_renderer_gray_bg_on_read_only) |>
        rhandsontable::hot_row(which(selected_accnrs() == ""), readOnly = TRUE) |>
        rhot_set_visual_colheaders(provid_table_cols_pretty[match(cols, provid_table_cols)])

        hot
      })
    }

    klona_provid_fran_forsta <- function(name) {
      if (!esbaser::provid_validate(provid_table$dfs[[name]][1, "provid"])) {
        shiny::showNotification(
          "Invalid or missing ProvID in first row. Please enter on the form 'Q2022-12345'",
          type = "warning")
        return()
      }

      new_table <- provid_table$dfs[[name]]
      new_table[selected_accnrs() != "", "provid"] <- provid_table$dfs[[name]][1, "provid"]
      handle_provid_table_change(name, new_table)
    }

    sekvens_provid_fran_forsta <- function(name) {
      if (!esbaser::provid_validate(provid_table$dfs[[name]][1, "provid"])) {
        shiny::showNotification(
          "Invalid or missing ProvID in first row. Please enter on the form 'Q2022-12345'",
          type = "warning")
        return()
      }

      parsed <- esbaser::provid_parse(provid_table$dfs[[name]][1, "provid"])
      new_table <- provid_table$dfs[[name]]
      new_table[selected_accnrs() != "", "provid"] <- unlist(
        lapply(
          seq_len(nrow(new_table))[selected_accnrs() != ""],
          function(i) {
            esbaser::provid_sprint(esbaser::provid_add(parsed, i - 1))
          }
        )
      )

      handle_provid_table_change(name, new_table)
    }

    create_prov_section <- function(name) {
      if (name %in% provs()) {
        shiny::showNotification(
          paste0(
            "Cannot add multilpe prov_sections with the same name, '",
            name,
            "', Please submit an issue detailing the steps you took to reach ",
            "this error on https://github.com/CGI-NRM/esbase-new-shiny/"
          ),
          type = "error", duration = 30)
        return()
      }

      prov_ns_current <- create_prov_ns(name, session$ns)

      shiny::insertUI(paste0("#", session$ns("after_provs")), where = "beforeBegin", provlist_ui(prov_ns_current))

      provs(c(provs(), name))

      create_provid_table(name)

      add_new_prov_section_observe_events(name)
    }

    add_new_prov_section_observe_events <- function(name) {
      # This handles the first render aswell when the UI has been renderer, and the input$ has been initialized
      # Except for the initial prov1, where the input already exists and the observeEvent for selected_accnrs()
      #     creates and does the initial render
      o1 <- shiny::observeEvent({
        input[[prov_io(name, "analyslab")]]
        input[[prov_io(name, "homogenat")]]
        1
      }, {
        render_provid_table(name)
      }, ignoreInit = TRUE)

      o2 <- shiny::observeEvent(input[[prov_io(name, "klona_provid_fran_forsta")]], {
        klona_provid_fran_forsta(name)
      })

      o3 <- shiny::observeEvent(input[[prov_io(name, "sekvens_provid_fran_forsta")]], {
        sekvens_provid_fran_forsta(name)
      })

      o4 <- shiny::observeEvent(input[[prov_io(name, "provid_table")]], {
        new_table <- rhandsontable::hot_to_r(input[[prov_io(name, "provid_table")]])
        handle_provid_table_change(name, new_table)
      })

      # Save observe events so that they can be deleted later
      provs_observe_events[[name]] <- c(o1, o2, o3, o4)
    }

    delete_prov_section <- function(name) {
      # NOTE: OBS NOT TESTED/USED YET
      # remove name from provs
      provs(provs()[-which(provs() == name)])

      # remove ui
      # TODO:

      # destroy observers
      for (o in provs_observe_events[[name]]) {
        o$destroy()
      }
    }

    # ---------- ONE-TIME SETUP ----------
    add_new_prov_section_observe_events("prov1")

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$set_limniska, {
      shiny::showNotification("Lägger till Hg, Metall, CLC+BFR, Dioxin och PFAS")
    })

    shiny::observeEvent(input$lagg_till_prov, {
      if (length(provs()) == 0) {
        shiny::showNotification(
          paste0(
            "Could not create a new prov-section, no initial prov_section exists. ",
            "This should not happen, please submit an issue to https://github.com",
            "/CGI-NRM/esbase-new-shiny/."
          ), type = "error", duration = 20)
      }

      new_name <- provs()[length(provs())]
      new_name <- paste0("prov", new_name |> substring(5) |> as.numeric() + 1)
      create_prov_section(new_name)
    })

    shiny::observeEvent(selected_accnrs(), {
      # TODO: If accnr changed, remove row
      # TODO: If length(selected_accnrs()) change, do not clear all data, only add the necessary new rows
      current_dfs <- provid_table$dfs
      for (name in provs()) {
        if (is.null(current_dfs[[name]]) || nrow(current_dfs[[name]]) != length(selected_accnrs())) {
          create_provid_table(name)
          render_provid_table(name)
        } else {
          current_dfs[[name]][, "accnr"] <- selected_accnrs()
          handle_provid_table_change(name, current_dfs[[name]])
        }
      }
    })
  })
}
