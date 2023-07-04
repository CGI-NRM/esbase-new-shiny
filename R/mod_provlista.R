mod_provlista_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("provlista"),
#    shiny::h3("Provlista"),
    shiny::actionButton(inputId = ns("lagg_till_prov"), label = "Lägg till prov"),
    shiny::actionButton(inputId = ns("set_limniska"), label = "Set Limniska Programmet Prover", disabled = TRUE),
    shiny::br(),
    shiny::br(),
    shiny::tabsetPanel(
      type = "tabs",
      id = ns("prov_tabset_panel"),
      provlist_ui(create_prov_ns("prov1", ns), "prov1")
    )
  )
}

mod_provlista_server <- function(id, conn, selected_accnrs, provlista_table) {
  shiny::moduleServer(id, function(input, output, session) {
    loginfo("mod_provlista.R: module server start")
    # ---------- REACTIVE VARIABLES ----------
    # A vector of the names of all provs
    provs <- shiny::reactiveVal()
    # A list/reactiveValues where the keys hold vectors of all saved observeEvents, so that they can be deleted
    provs_observe_events <- shiny::reactiveValues()

    # ---------- DEFAULT VALUES ----------
    provlista_table$dfs <- list()
    provlista_table$metas <- data.frame(
      homogenat = logical(0),
      analyslab = character(0),
      analystyp = character(0),
      analytiker = character(0),
      provtagningsinst = character(0),
      vavnad = character(0)
    )
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
      logdebug("mod_provlista.R - create_provid_table: called")
      num <- length(selected_accnrs())
      provlista_table$dfs[[name]] <- data.frame(
        accnr = selected_accnrs(),
        provid = rep("", num),
        aces = rep("", num),
        delvikt = as.numeric(NA) |> rep(num),
        provvikt = as.numeric(NA) |> rep(num))

      provlista_table$metas[name, "vavnad"] <- ifelse(is.null(input[[prov_io(name, "vavnad")]]),
                                                      "", input[[prov_io(name, "vavnad")]])
      provlista_table$metas[name, "provtagningsinst"] <- ifelse(is.null(input[[prov_io(name, "provtagningsinst")]]),
                                                                "", input[[prov_io(name, "provtagningsinst")]])
      provlista_table$metas[name, "analytiker"] <- ifelse(is.null(input[[prov_io(name, "analytiker")]]),
                                                          "", input[[prov_io(name, "analytiker")]])
      provlista_table$metas[name, "analystyp"] <- ifelse(is.null(input[[prov_io(name, "analystyp")]]),
                                                         "", input[[prov_io(name, "analystyp")]])
      provlista_table$metas[name, "analyslab"] <- ifelse(is.null(input[[prov_io(name, "analyslab")]]),
                                                         "", input[[prov_io(name, "analyslab")]])
      provlista_table$metas[name, "homogenat"] <- ifelse(is.null(input[[prov_io(name, "homogenat")]]),
                                                         FALSE, input[[prov_io(name, "homogenat")]])
    }

    handle_provid_table_change <- function(name, new_table) {
      logdebug("mod_provlista.R - handle_provid_table_change: called")
      if (is.null(provlista_table$dfs[[name]]) || nrow(new_table) != nrow(provlista_table$dfs[[name]])) {
        shiny::showNotification(
          paste0(
            "Cannot add rows to this table. Add more rows under 'Biologdata' to increase the number of rows in this table"
          ), type = "warning", duration = 30)
        render_provid_table(name)
        return()
      }
      changed <- FALSE

      # Clear rows with changed accnrs
      rows_accnr_changed <- provlista_table$dfs[[name]]["accnr"] != new_table["accnr"]
      rows_accnr_changed <- rows_accnr_changed | xor(is.na(provlista_table$dfs[[name]]["accnr"]), is.na(new_table["accnr"]))
      if (any(rows_accnr_changed)) {
        changed <- TRUE

        provlista_table$dfs[[name]][rows_accnr_changed, ] <- data.frame(
          accnr = new_table[rows_accnr_changed, "accnr"],
          provid = "",
          aces = "",
          delvikt = as.numeric(NA),
          provvikt = as.numeric(NA)
        )
      }

      cn <- colnames(new_table)
      for (col in cn[cn != "accnr"]) {
        cells_changed <- provlista_table$dfs[[name]][col] != new_table[col]
        cells_changed <- cells_changed | xor(is.na(provlista_table$dfs[[name]][col]), is.na(new_table[col]))
        cells_changed <- cells_changed & !rows_accnr_changed

        rows <- which(cells_changed)
        provlista_table$dfs[[name]][rows, col] <- new_table[rows, col]

        if (length(rows) > 0) {
          changed <- TRUE
        }
      }

      if (changed) {
        render_provid_table(name)
      }
    }

    render_provid_table <- function(name) {
      logdebug("mod_provlista.R - render_provid_table: called")
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

      df <- provlista_table$dfs[[name]][cols]

      output[[prov_io(name, "provid_table")]] <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(df, rowHeaders = FALSE, overflow = "visible", maxRows = nrow(df)) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
          rhandsontable::hot_col("accnr", readOnly = TRUE) |>
          rhandsontable::hot_col("provid", renderer = rhot_renderer_validate_provid_gray_bg_on_read_only) |>
          rhandsontable::hot_col(which(colnames(df) != "provid"), renderer = rhot_renderer_gray_bg_on_read_only) |>
          rhandsontable::hot_row(which(selected_accnrs() == ""), readOnly = TRUE) |>
          rhot_set_visual_colheaders(provid_table_cols_pretty[match(cols, provid_table_cols)]) |>
          rhot_disable_context_menu()
      })
    }

    klona_provid_fran_forsta <- function(name) {
      logdebug("mod_provlista.R - klona_provid_fran_forsta: called")
      if (!esbaser::provid_validate(provlista_table$dfs[[name]][1, "provid"])) {
        shiny::showNotification(
          "Invalid or missing ProvID in first row. Please enter on the form 'Q2022-12345'",
          type = "warning")
        return()
      }

      new_table <- provlista_table$dfs[[name]]
      new_table[selected_accnrs() != "", "provid"] <- provlista_table$dfs[[name]][1, "provid"]
      handle_provid_table_change(name, new_table)
    }

    sekvens_provid_fran_forsta <- function(name) {
      logdebug("mod_provlista.R - sekvens_provid_fran_forsta: called")
      if (!esbaser::provid_validate(provlista_table$dfs[[name]][1, "provid"])) {
        shiny::showNotification(
          "Invalid or missing ProvID in first row. Please enter on the form 'Q2022-12345'",
          type = "warning")
        return()
      }

      parsed <- esbaser::provid_parse(provlista_table$dfs[[name]][1, "provid"])
      new_table <- provlista_table$dfs[[name]]
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
      logdebug("mod_provlista.R - create_prov_section: called")
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

      shiny::insertTab("prov_tabset_panel", provlist_ui(prov_ns_current, name))

      provs(c(provs(), name))

      create_provid_table(name)

      add_new_prov_section_observe_events(name)
    }

    add_new_prov_section_observe_events <- function(name) {
      logdebug("mod_provlista.R - add_new_prov_section_observe_events: called")
      # Once the vavnad select exists, update it with the options from esbase
      shiny::observeEvent(input[[prov_io(name, "vavnad")]], {
        update_select_inputs_with_stodlistor(name)
      }, once = TRUE)


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

      o5 <- shiny::observeEvent(input[[prov_io(name, "homogenat")]], {
        provlista_table$metas[name, "homogenat"] <- input[[prov_io(name, "homogenat")]]
      })

      o6 <- shiny::observeEvent(input[[prov_io(name, "analyslab")]], {
        provlista_table$metas[name, "analyslab"] <- input[[prov_io(name, "analyslab")]]
      })

      o7 <- shiny::observeEvent(input[[prov_io(name, "analystyp")]], {
        provlista_table$metas[name, "analystyp"] <- input[[prov_io(name, "analystyp")]]
      })

      o8 <- shiny::observeEvent(input[[prov_io(name, "analytiker")]], {
        provlista_table$metas[name, "analytiker"] <- input[[prov_io(name, "analytiker")]]
      })

      o9 <- shiny::observeEvent(input[[prov_io(name, "provtagningsinst")]], {
        provlista_table$metas[name, "provtagningsinst"] <- input[[prov_io(name, "provtagningsinst")]]
      })

      o10 <- shiny::observeEvent(input[[prov_io(name, "vavnad")]], {
        shiny::req(session$userData$stodlistor$material_type_vector)
        shiny::req(input[[prov_io(name, "vavnad")]])

        provlista_table$metas[name, "vavnad"] <- names(
          session$userData$stodlistor$material_type_vector
        )[session$userData$stodlistor$material_type_vector == input[[prov_io(name, "vavnad")]]]
      })

      o11 <- shiny::observeEvent(input[[prov_io(name, "delete_section")]], {
        delete_prov_section(name)
      }, ignoreInit = TRUE)

      # Save observe events so that they can be deleted later
      provs_observe_events[[name]] <- c(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11)
    }

    update_select_inputs_with_stodlistor <- function(name) {
      logdebug("mod_provlista.R - update_select_inputs_with_stodlistor: called")
      # Update vävnad choices from stödlista
      material_type <- esbaser::get_options_material_type()
      session$userData$stodlistor$material_type_vector <- material_type[, "id", drop = TRUE]
      names(session$userData$stodlistor$material_type_vector) <- material_type[, "representation", drop = TRUE]

      shiny::updateSelectizeInput(session, prov_io(name, "vavnad"),
                                  choices = session$userData$stodlistor$material_type_vector,
                                  selected = NA, server = TRUE)
    }

    delete_prov_section <- function(name) {
      logdebug("mod_provlista.R - delete_prov_section: called")

      if (length(provs()) <= 1) {
        shiny::showNotification("There is only one prov left, cannot delete it.", duration = 10)
        return()
      }

      provs(provs()[provs() != name])
      provlista_table$metas <- provlista_table$metas[rownames(provlista_table$metas) != name, ]
      provlista_table$dfs[[name]] <- NULL

      shiny::removeTab(inputId = "prov_tabset_panel", target = session$ns(prov_io(name, "tabpanel")))

      # destroy observers
      for (o in provs_observe_events[[name]]) {
        o$destroy()
      }

      provs_observe_events[[name]] <- NULL
    }

    # ---------- ONE-TIME SETUP ----------
    add_new_prov_section_observe_events("prov1")
    update_select_inputs_with_stodlistor("prov1")

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
      current_dfs <- provlista_table$dfs
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

    shiny::observeEvent(input$prov_tabset_panel, {
      prov <- stringr::str_match(input$prov_tabset_panel,
                                 paste0("(", session$ns(""), ")(?<prov>.*)(_tabpanel)")
      )[, "prov"]
      render_provid_table(prov)
    })
  })
}
