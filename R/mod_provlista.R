mod_provlista_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("provlista"),
    shiny::actionButton(inputId = ns("lagg_till_prov"), label = "Lägg till prov"),
    shiny::actionButton(inputId = ns("set_limniska"), label = "Set Limniska Programmet Prover", disabled = TRUE),
    shiny::br(),
    shiny::br(),
    shiny::tabsetPanel(
      type = "tabs",
      id = ns("prov_tabset_panel"),
      provlist_ui(create_prov_ns("prov1", ns), "prov1")
    ),
    shiny::actionButton(inputId = ns("save"), label = "Spara provberedningar")
  )
}

mod_provlista_server <- function(id, db, account, selected, provlista, provberednings_meta, restore) {
  shiny::moduleServer(id, function(input, output, session) {
    loginfo("mod_provlista.R: module server start")
    # ---------- REACTIVE VARIABLES ----------
    # A vector of the names of all provs
    provs <- shiny::reactiveVal()
    # A list/reactiveValues where the keys hold vectors of all saved observeEvents, so that they can be deleted
    provs_observe_events <- dataHolder()

    # ---------- DEFAULT VALUES ----------
    provlista$dfs <- list()
    provlista$metas <- data.frame(
      homogenat = logical(0),
      analyslab = character(0),
      analystyp = character(0),
      analytiker = character(0),
      provtagningsinst = character(0),
      vavnad = character(0),
      protokollnummer = numeric(0),
      analystypnotis = character(0),
      resultatnotis = character(0)
    )
    provs(c("prov1"))

    # ---------- FUNCTIONS ----------
    # Create a shorthand for generating the custom-namespaced id:s for prov with a certain name
    prov_io <- function(name, id) {
      paste0(name, "_", id)
    }

    provid_table_cols <- c("check", "accnr", "material.amount_original", "material.amount_left",
                           "material_storage.name", "provid", "aces", "delvikt", "provvikt")
    provid_table_cols_pretty <- c("", "Acc.nr.", "Sparad (g)", "Mängd kvar (g, st)",
                                  "Förvaringsplats", "ProvID", "ACES NR", "Delvikt (g)", "Provvikt (g)")
    provid_table_readonly_cols <- c("accnr", "material.amount_original", "material.amount_left", "material_storage.name")
    provid_table_number_columns <- c("delvikt", "provvikt")

    assign_material_id <- function(name) {
      logdebug("mod_provlista.R - assign_material_id: called")
      if (is.null(input[[prov_io(name, "vavnad")]]) || is.null(provlista$dfs[[name]]) || nrow(selected$material) == 0) {
        return()
      }

      mat_vavnad <- selected$material |> filter(material_type_id == input[[prov_io(name, "vavnad")]])
      for (row in seq_len(nrow(provlista$dfs[[name]]))) {
        if (
          !is.na(provlista$dfs[[name]][row, "material_id"]) &&
          (provlista$dfs[[name]][row, "material_id"] %in% mat_vavnad$id)
        ) {
          next
        }
        accdb <- esbaser::accnr_to_accdb(provlista$dfs[[name]][row, "accnr"])
        mat <- mat_vavnad |> filter(accession_id == accdb)

        if (nrow(mat) == 0) {
          provlista$dfs[[name]][row, "material_id"] <- NA
        } else if (nrow(mat) == 1) {
          provlista$dfs[[name]][row, "material_id"] <- mat |> select(id) |> unlist(use.names = FALSE)
        } else if (nrow(mat) > 1) {
          provlista$dfs[[name]][row, "material_id"] <- mat |> select(id) |> first() |> unlist(use.names = FALSE)
        }
      }

      logfine("mod_provlista.R - assign_material_id: finished")
    }

    # Create a new provid_table dataframe with default/empty values and place in provid_table
    create_provid_table <- function(name) {
      logdebug("mod_provlista.R - create_provid_table: called")

      num <- length(selected$accs_db)
      provlista$dfs[[name]] <- tibble(
        accnr = esbaser::accdb_to_accnr(selected$accs_db),
        check = rep(TRUE, num),
        provid = rep("", num),
        aces = rep("", num),
        delvikt = as.numeric(NA) |> rep(num),
        provvikt = as.numeric(NA) |> rep(num),
        material_id = rep(NA, num)
      )

      provlista$metas[name, "vavnad"] <- ifelse(is.null(input[[prov_io(name, "vavnad")]]),
                                                      "", input[[prov_io(name, "vavnad")]])
      provlista$metas[name, "provtagningsinst"] <- ifelse(is.null(input[[prov_io(name, "provtagningsinst")]]),
                                                                "", input[[prov_io(name, "provtagningsinst")]])
      provlista$metas[name, "analytiker"] <- ifelse(is.null(input[[prov_io(name, "analytiker")]]),
                                                          "", input[[prov_io(name, "analytiker")]])
      provlista$metas[name, "analystyp"] <- ifelse(is.null(input[[prov_io(name, "analystyp")]]),
                                                         "", input[[prov_io(name, "analystyp")]])
      provlista$metas[name, "analyslab"] <- ifelse(is.null(input[[prov_io(name, "analyslab")]]),
                                                         "", input[[prov_io(name, "analyslab")]])
      provlista$metas[name, "homogenat"] <- ifelse(is.null(input[[prov_io(name, "homogenat")]]),
                                                         FALSE, input[[prov_io(name, "homogenat")]])
      provlista$metas[name, "analystypnotis"] <- ifelse(is.null(input[[prov_io(name, "analystyp_notis")]]),
                                                        "", input[[prov_io(name, "analystyp_notis")]])
      provlista$metas[name, "resultatnotis"] <- ifelse(is.null(input[[prov_io(name, "resultatnotis")]]),
                                                       "", input[[prov_io(name, "resultatnotis")]])
      logfine("mod_provlista.R - create_provid_table: finished")
    }

    handle_provid_table_change <- function(name, new_table) {
      logdebug("mod_provlista.R - handle_provid_table_change: called")
      if (nrow(provlista$dfs[[name]]) == 0 || nrow(new_table) == 0) {
        return()
      }
      if (is.null(provlista$dfs[[name]]) || nrow(new_table) != nrow(provlista$dfs[[name]])) {
        shiny::showNotification(
          paste0(
            "Kan inte lägga till eller ta bort rader i tabellen."
          ), type = "warning", duration = 30)
        render_provid_table(name)
        return()
      }
      changed <- FALSE

      cn <- colnames(new_table)
      for (col in cn[!(cn %in% provid_table_readonly_cols)]) {
        cells_changed <- provlista$dfs[[name]][col] != new_table[col]
        cells_changed <- cells_changed | xor(is.na(provlista$dfs[[name]][col]), is.na(new_table[col]))

        rows <- which(cells_changed)
        provlista$dfs[[name]][rows, col] <- new_table[rows, col]

        if (length(rows) > 0) {
          changed <- TRUE
        }
      }

      if (changed) {
        #render_provid_table(name)
      }
      logfine("mod_provlista.R - handle_provid_table_change: finished")
    }

    render_provid_table <- function(name) {
      logdebug("mod_provlista.R - render_provid_table: called")
      if (is.null(input[[prov_io(name, "homogenat")]]) ||
          is.null(input[[prov_io(name, "analyslab")]])) {
        return()
      }

      if (is.null(provlista$dfs[[name]]) || nrow(provlista$dfs[[name]]) == 0) {
        output[[prov_io(name, "provid_table")]] <- rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(data.frame(), rowHeaders = FALSE, overflow = "visible", maxRows = 0) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
          rhot_disable_context_menu()
        })
        return()
      }

      if (is.null(selected$material) || nrow(selected$material) == 0) {
        return()
      }

      cols <- c("accnr", "material.amount_original", "material.amount_left", "material_storage.name", "provid")
      if (input[[prov_io(name, "analyslab")]] == "ACES") {
        cols <- c(cols, "aces")
      }
      if (input[[prov_io(name, "homogenat")]]) {
        cols <- c("check", cols, "delvikt")
      }
      cols <- c(cols, "provvikt")

      df_all <- (
        provlista$dfs[[name]] |>
        left_join(selected$material |> rename_w_prefix("material."), by = join_by(material_id == material.id)) |>
        left_join(db$material_storage |> rename_w_prefix("material_storage."),
                  by = join_by(material.storage_type_id == material_storage.id)))
      df <- df_all |> select(all_of(cols))

      output[[prov_io(name, "provid_table")]] <- rhandsontable::renderRHandsontable({
        hot <- (
          rhandsontable::rhandsontable(df, rowHeaders = FALSE, overflow = "visible", maxRows = nrow(df)) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
          rhandsontable::hot_col("provid", renderer = rhot_renderer_validate_provid_gray_bg_on_read_only) |>
          rhandsontable::hot_col(provid_table_readonly_cols[provid_table_readonly_cols %in% colnames(df)], readOnly = TRUE) |>
          rhandsontable::hot_col(provid_table_number_columns[provid_table_number_columns %in% colnames(df)], format = "0") |>
          rhandsontable::hot_col(which(colnames(df) != "provid"), renderer = rhot_renderer_gray_bg_on_read_only) |>
          rhot_set_visual_colheaders(provid_table_cols_pretty[match(cols, provid_table_cols)]) |>
          rhot_disable_context_menu())

        for (row in seq_len(nrow(df))) {
          if (isFALSE(df[row, ] |> select(any_of("check")) |> unlist(use.names = FALSE)) || is.na(df_all[row, "material_id"])) {
            for (col in which(colnames(df) != "check")) {
              hot <- rhandsontable::hot_cell(hot, row, col, readOnly = TRUE)
            }
          }
        }

        hot
      })
      logfine("mod_provlista.R - render_provid_table: finished")
    }

    klona_provid_fran_forsta <- function(name) {
      logdebug("mod_provlista.R - klona_provid_fran_forsta: called")
      if (!esbaser::provid_validate(provlista$dfs[[name]][1, "provid"])) {
        shiny::showNotification(
          "Ogiltigt eller saknat ProvID i första rader. Vänligen skriv i enligt 'Q2022-12345.'",
          type = "warning")
        return()
      }

      new_table <- provlista$dfs[[name]]
      new_table[selected$accs_db != "", "provid"] <- provlista$dfs[[name]][1, "provid"]
      new_table[!new_table$check, "provid"] <- ""
      handle_provid_table_change(name, new_table)
      render_provid_table(name)
      logfine("mod_provlista.R - klona_provid_fran_forsta: finished")
    }

    sekvens_provid_fran_forsta <- function(name) {
      logdebug("mod_provlista.R - sekvens_provid_fran_forsta: called")
      if (!esbaser::provid_validate(provlista$dfs[[name]][1, "provid"])) {
        shiny::showNotification(
          "Ogiltigt eller saknat ProvID i första rader. Vänligen skriv i enligt 'Q2022-12345.'",
          type = "warning")
        return()
      }

      parsed <- esbaser::provid_parse(provlista$dfs[[name]][1, "provid"])
      new_table <- provlista$dfs[[name]]
      new_table[selected$accs_db != "", "provid"] <- unlist(
        lapply(
          seq_len(nrow(new_table))[selected$accs_db != ""],
          function(i) esbaser::provid_sprint(esbaser::provid_add(parsed, i - 1))
        )
      )
      new_table[!new_table$check, "provid"] <- ""

      handle_provid_table_change(name, new_table)
      render_provid_table(name)
      logfine("mod_provlista.R - sekvens_provid_fran_forsta: finished")
    }

    create_prov_section <- function(name, do_create_provid_table = TRUE, before_stodlistor = NULL, after_stodlistor = NULL) {
      logdebug("mod_provlista.R - create_prov_section: called")
      if (name %in% provs()) {
        shiny::showNotification(
          paste0(
            "Kan inte lägga till flera provsektioner med samma namn, '",
            name,
            "', vänligen lägg till en issue på https://github.com/CGI-NRM/esbase-new-shiny/"
          ),
          type = "error", duration = 30)
        return()
      }

      prov_ns_current <- create_prov_ns(name, session$ns)

      shiny::insertTab("prov_tabset_panel", provlist_ui(prov_ns_current, name))

      provs(c(provs(), name))

      if (do_create_provid_table) {
        create_provid_table(name)
        assign_material_id(name)
      }

      add_new_prov_section_observe_events(name)

      # When Ui has been initialized
      uid <- uuid::UUIDgenerate(use.time = TRUE, output = "string")
      i <- prov_io(name, paste0("new_ui_initialized", uid))
      shinyjs::runjs(paste0(
          'Shiny.setInputValue("', session$ns(i), '", "true")'))
      shiny::observeEvent(input[[i]], {
        if (!is.null(before_stodlistor)) {
          before_stodlistor(name)
        }
        update_select_inputs_with_stodlistor(name)
        if (!is.null(after_stodlistor)) {
          uid <- uuid::UUIDgenerate(use.time = TRUE, output = "string")
          j <- prov_io(name, paste0("new_ui_stodlistor_updated", uid))
          shinyjs::runjs(paste0(
              'Shiny.setInputValue("', session$ns(j), '", "true")'))
          shiny::observeEvent(input[[j]], {
            after_stodlistor(name)
          }, once = TRUE)
        }
      }, once = TRUE)
      logfine("mod_provlista.R - create_prov_section: finished")
    }

    add_new_prov_section_observe_events <- function(name) {
      logdebug("mod_provlista.R - add_new_prov_section_observe_events: called")
      # This handles the first render aswell when the UI has been renderer, and the input$ has been initialized
      # Except for the initial prov1, where the input already exists and the observeEvent for selected$update()
      #     creates and does the initial render
      observers <- c(
        shiny::observeEvent({
          input[[prov_io(name, "analyslab")]]
          input[[prov_io(name, "homogenat")]]
          1
        }, {
          render_provid_table(name)
        }, ignoreInit = TRUE),

        shiny::observeEvent(input[[prov_io(name, "klona_provid_fran_forsta")]], {
          klona_provid_fran_forsta(name)
        }),

        shiny::observeEvent(input[[prov_io(name, "sekvens_provid_fran_forsta")]], {
          sekvens_provid_fran_forsta(name)
        }),

        shiny::observeEvent(input[[prov_io(name, "provid_table")]], {
          new_table <- rhandsontable::hot_to_r(input[[prov_io(name, "provid_table")]])
          handle_provid_table_change(name, new_table)
        }),

        shiny::observeEvent(input[[prov_io(name, "homogenat")]], {
          provlista$metas[name, "homogenat"] <- ifelse(is.null(input[[prov_io(name, "homogenat")]]),
                                                             "", input[[prov_io(name, "homogenat")]])
          if (!is.null(provlista$dfs[[name]])) {
            num <- nrow(provlista$dfs[[name]])
            provlista$dfs[[name]]$check <- rep(TRUE, num)
          }
        }),

        shiny::observeEvent(input[[prov_io(name, "analyslab")]], {
          if (isTRUE(input[[prov_io(name, "analyslab")]] == "")) {
            return()
          }

          provlista$metas[name, "analyslab"] <- ifelse(is.null(input[[prov_io(name, "analyslab")]]),
                                                             "", input[[prov_io(name, "analyslab")]])
        }),

        shiny::observeEvent(input[[prov_io(name, "analystyp")]], {
          if (isTRUE(input[[prov_io(name, "analystyp")]] == "")) {
            return()
          }
          provlista$metas[name, "analystyp"] <- ifelse(is.null(input[[prov_io(name, "analystyp")]]),
                                                             "", input[[prov_io(name, "analystyp")]])
        }),

        shiny::observeEvent(input[[prov_io(name, "analystyp_notis")]], {
          provlista$metas[name, "analystypnotis"] <- ifelse(is.null(input[[prov_io(name, "analystyp_notis")]]),
                                                             "", input[[prov_io(name, "analystyp_notis")]])
        }),

        shiny::observeEvent(input[[prov_io(name, "notiser_resultat")]], {
          provlista$metas[name, "resultatnotis"] <- ifelse(is.null(input[[prov_io(name, "notiser_resultat")]]),
                                                           "", input[[prov_io(name, "notiser_resultat")]])
        }),

        shiny::observeEvent(input[[prov_io(name, "analytiker")]], {
          if (isTRUE(input[[prov_io(name, "analytiker")]] == "")) {
            return()
          }
          provlista$metas[name, "analytiker"] <- ifelse(is.null(input[[prov_io(name, "analytiker")]]),
                                                              "", input[[prov_io(name, "analytiker")]])
        }),

        shiny::observeEvent(input[[prov_io(name, "provtagningsinst")]], {
          if (isTRUE(input[[prov_io(name, "provtagningsinst")]] == "")) {
            return()
          }
          provlista$metas[name, "provtagningsinst"] <- ifelse(is.null(input[[prov_io(name, "provtagningsinst")]]),
                                                                    "", input[[prov_io(name, "provtagningsinst")]])
        }),

        shiny::observeEvent(input[[prov_io(name, "vavnad")]], {
          if (isTRUE(input[[prov_io(name, "vavnad")]] == "")) {
            return()
          }
          provlista$metas[name, "vavnad"] <- ifelse(is.null(input[[prov_io(name, "vavnad")]]),
                                                          "", input[[prov_io(name, "vavnad")]])
          assign_material_id(name)
          render_provid_table(name)
        }),

        shiny::observeEvent(input[[prov_io(name, "delete_section")]], {
          delete_prov_section(name)
        }, ignoreInit = TRUE)
      )

      # Save observe events so that they can be deleted later
      provs_observe_events[[name]] <- observers
    }

    update_select_inputs_with_stodlistor <- function(name) {
      logdebug("mod_provlista.R - update_select_inputs_with_stodlistor: called")
      # Update vävnad choices from stödlista

      if (!is.null(selected$material) && nrow(selected$material) > 0) {
        materials <- (selected$material |> select(material_type_id) |> unique() |>
                      left_join(db$material_type |> rename_w_prefix("type."), by = join_by(material_type_id == type.id)) |>
                      filter(!is.na(material_type_id) & type.swe_name != ""))
        material_vector <- materials |> select(material_type_id) |> unlist(use.names = FALSE)
        names(material_vector) <- materials |> select(type.swe_name) |> unlist(use.names = FALSE)
        curr_selected_material <- ifelse(
          is.null(provlista$metas[name, "vavnad", drop = TRUE]) ||
          provlista$metas[name, "vavnad", drop = TRUE] == "",
          NA, provlista$metas[name, "vavnad", drop = TRUE])
        shiny::updateSelectizeInput(session, prov_io(name, "vavnad"), choices = material_vector,
                                    selected = curr_selected_material, server = TRUE)
      } else {
        shiny::updateSelectizeInput(session, prov_io(name, "vavnad"), choices = c(""),
                                    selected = NA, server = TRUE)
      }

      person_vector <- db$person |> select(id) |> unlist(use.names = FALSE)
      names(person_vector) <- db$person |> select(institution, firstname, lastname, town) |> apply(1, paste_collapse)
      person_vector <- person_vector[names(person_vector) != ""]
      curr_selected_analytiker <- ifelse(
        is.null(provlista$metas[name, "analytiker", drop = TRUE]) ||
        provlista$metas[name, "analytiker", drop = TRUE] == "",
        NA, provlista$metas[name, "analytiker", drop = TRUE])
      shiny::updateSelectizeInput(session, prov_io(name, "analytiker"), choices = person_vector,
                                  selected = curr_selected_analytiker, server = TRUE)

      analysis_type_vector <- db$analysis_type |> select(id) |> unlist(use.names = FALSE)
      names(analysis_type_vector) <- db$analysis_type |> select(name) |> apply(1, paste_collapse)
      analysis_type_vector <- analysis_type_vector[names(analysis_type_vector) != ""]
      curr_selected_analystyp <- ifelse(
        is.null(provlista$metas[name, "analystyp", drop = TRUE]) ||
        provlista$metas[name, "analystyp", drop = TRUE] == "",
        NA, provlista$metas[name, "analystyp", drop = TRUE])
      shiny::updateSelectizeInput(session, prov_io(name, "analystyp"), choices = analysis_type_vector,
                                  selected = curr_selected_analystyp, server = TRUE)

      logfine("mod_provlista.R - update_select_inputs_with_stodlistor: finished")
    }

    delete_prov_section <- function(name, force = FALSE) {
      logdebug("mod_provlista.R - delete_prov_section: called")

      if (length(provs()) <= 1 && !force) {
        shiny::showNotification("Det är endast en provsektion kvar, kan inte ta bort den.", duration = 10)
        return()
      }

      provs(provs()[provs() != name])
      provlista$metas <- provlista$metas[rownames(provlista$metas) != name, ]
      provlista$dfs[[name]] <- NULL

      # Wait for update inputs until removing the UI
      shiny::observeEvent(input[[prov_io(name, "provid_table")]], {
        if (is.null(provlista$dfs[[name]])) {
          shiny::removeTab(inputId = "prov_tabset_panel", target = session$ns(prov_io(name, "tabpanel")))
        }
      }, once = TRUE, ignoreInit = TRUE)

      shiny::updateSelectizeInput(session, prov_io(name, "vavnad"), selected = NA)
      shiny::updateSelectizeInput(session, prov_io(name, "provtagningsinst"), selected = NA)
      shiny::updateSelectizeInput(session, prov_io(name, "analystyp"), selected = NA)
      shiny::updateSelectizeInput(session, prov_io(name, "analyslab"), selected = NA)
      shiny::updateSelectizeInput(session, prov_io(name, "analytiker"), selected = NA)
      shiny::updateTextInput(session, prov_io(name, "analystyp_notis"), value = "")
      shiny::updateTextInput(session, prov_io(name, "notiser_resultat"), value = "")
      shiny::updateCheckboxInput(session, prov_io(name, "homogenat"), value = FALSE)
      output[[prov_io(name, "provid_table")]] <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), rowHeaders = FALSE, overflow = "visible", maxRows = 0) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE, allowCustomBorders = FALSE) |>
        rhot_disable_context_menu()
      })

      # destroy observers
      for (o in provs_observe_events[[name]]) {
        o$destroy()
      }

      provs_observe_events[[name]] <- NULL
      logfine("mod_provlista.R - delete_prov_section: finished")
    }

    update_select_from_metas <- function(prov) {
      logdebug("mod_provlista.R - update_select_from_metas: called")
      shiny::updateSelectizeInput(session, prov_io(prov, "vavnad"),
                                  selected = ifelse(
                                    is.null(provlista$metas[prov, "vavnad", drop = TRUE]) ||
                                    provlista$metas[prov, "vavnad", drop = TRUE] == "",
                                    NA,
                                    provlista$metas[prov, "vavnad", drop = TRUE]))
      shiny::updateCheckboxInput(session, prov_io(prov, "homogenat"),
                                 value = ifelse(
                                   is.null(provlista$metas[prov, "homogenat", drop = TRUE]) ||
                                   provlista$metas[prov, "homogenat", drop = TRUE] == "",
                                   FALSE,
                                   provlista$metas[prov, "homogenat", drop = TRUE]))
      shiny::updateSelectizeInput(session, prov_io(prov, "provtagningsinst"),
                                  selected = ifelse(
                                    is.null(provlista$metas[prov, "provtagningsinst", drop = TRUE]) ||
                                    provlista$metas[prov, "provtagningsinst", drop = TRUE] == "",
                                    NA,
                                    provlista$metas[prov, "provtagningsinst", drop = TRUE]))
      shiny::updateSelectizeInput(session, prov_io(prov, "analystyp"),
                                  selected = ifelse(
                                    is.null(provlista$metas[prov, "analystyp", drop = TRUE]) ||
                                    provlista$metas[prov, "analystyp", drop = TRUE] == "",
                                    NA,
                                    provlista$metas[prov, "analystyp", drop = TRUE]))
      shiny::updateSelectizeInput(session, prov_io(prov, "analyslab"),
                                  selected = ifelse(
                                    is.null(provlista$metas[prov, "analyslab", drop = TRUE]) ||
                                    provlista$metas[prov, "analyslab", drop = TRUE] == "",
                                    NA,
                                    provlista$metas[prov, "analyslab", drop = TRUE]))
      shiny::updateSelectizeInput(session, prov_io(prov, "analytiker"),
                                  selected = ifelse(
                                    is.null(provlista$metas[prov, "analytiker", drop = TRUE]) ||
                                    provlista$metas[prov, "analytiker", drop = TRUE] == "",
                                    NA,
                                    provlista$metas[prov, "analytiker", drop = TRUE]))
      shiny::updateTextInput(session, prov_io(prov, "analystyp_notis"),
                             value = ifelse(
                               is.null(provlista$metas[prov, "analystypnotis", drop = TRUE]),
                               "",
                               provlista$metas[prov, "analystypnotis", drop = TRUE]))
      shiny::updateTextInput(session, prov_io(prov, "notiser_resultat"),
                             value = ifelse(
                               is.null(provlista$metas[prov, "resultatnotis", drop = TRUE]),
                               "",
                               provlista$metas[prov, "resultatnotis", drop = TRUE]))
      logfine("mod_provlista.R - update_select_from_metas: finished")
    }

    # ---------- ONE-TIME SETUP ----------
    add_new_prov_section_observe_events("prov1")
    update_select_inputs_with_stodlistor("prov1")

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$set_limniska, {
      shiny::showNotification("Lägger till Hg, Metall, CLC+BFR, Dioxin och PFAS")
    })

    shiny::observeEvent(input$lagg_till_prov, {
      logdebug("mod_provlista.R - observeEvent(input$lagg_till_prov, {}): called")
      if (length(provs()) == 0) {
        shiny::showNotification(
          paste0(
            "Kunde inte skapa en ny prov-sektion. Ingen provsektion att utgå från kunde hittas. ",
            "Detta borde inte hända, vänligen lägg till en issue på https://github.com",
            "/CGI-NRM/esbase-new-shiny/."
          ), type = "error", duration = 20)
      }

      new_name <- provs()[length(provs())]
      new_name <- paste0("prov", new_name |> substring(5) |> as.numeric() + 1)
      create_prov_section(new_name)
      logfine("mod_provlista.R - observeEvent(input$lagg_till_prov, {}): finished")
    })

    shiny::observeEvent(selected$update(), {
      logdebug("mod_provlista.R - observeEvent(selected$update(), {}): called")
      # TODO: If accnr changed, remove row
      # TODO: If length(selected$accs_db) change, do not clear all data, only add the necessary new rows
      current_dfs <- provlista$dfs
      for (name in provs()) {
        if (
          is.null(current_dfs[[name]]) ||
          nrow(current_dfs[[name]]) != length(selected$accs_db) ||
          any(current_dfs[[name]]$accnr != esbaser::accdb_to_accnr(selected$accs_db), na.rm = TRUE)
        ) {
          create_provid_table(name)
          assign_material_id(name)
          render_provid_table(name)
        } else {
          current_dfs[[name]][, "accnr"] <- esbaser::accdb_to_accnr(selected$accs_db)
          handle_provid_table_change(name, current_dfs[[name]])
        }

        assign_material_id(name)
        update_select_inputs_with_stodlistor(name)
      }
      logfine("mod_provlista.R - observeEvent(selected$update(), {}): finished")
    })

    shiny::observeEvent(input$prov_tabset_panel, {
      logdebug("mod_provlista.R - observeEvent(input$prov_tabset_panel, {}): called")
      prov <- stringr::str_match(input$prov_tabset_panel,
                                 paste0("(", session$ns(""), ")(?<prov>.*)(_tabpanel)")
      )[, "prov"]
      render_provid_table(prov)
      update_select_from_metas(prov)
      logfine("mod_provlista.R - observeEvent(input$prov_tabset_panel, {}): finished")
    })

    shiny::observeEvent(input$save, {
      logdebug("mod_provlista.R - observeEvent(input$save, {}): called")
      lapply(
        provs(),
        function(prov) {
          id <- save_provlista(
            db = db,
            account = account,
            selected = selected,
            provlista = provlista,
            provberednings_meta = provberednings_meta,
            prov_name = prov)
          if (!is.null(id)) {
            output[[prov_io(prov, "protokollnummer")]] <- shiny::renderText(paste0("- Protokollnummer: ", as.character(id)))
            provlista$metas[prov, "protokollnummer"] <- id
          }
        })
      logfine("mod_provlista.R - observeEvent(input$save, {}): finished")
    })

    shiny::observeEvent(restore$update(), {
      logdebug("mod_provlista.R - observeEvent(restore$update(), {}): called")
      for (prov in provs()) {
        if (!(prov %in% names(restore$dfs))) {
          delete_prov_section(prov, force = TRUE)
        }
      }

      lapply(
        names(restore$dfs),
        function(prov) {
          provlista$dfs[[prov]] <- restore$dfs[[prov]]
          provlista$metas[prov, ] <- restore$metas[prov, ]

          if (!(prov %in% provs())) {
            create_prov_section(prov,
            do_create_provid_table = FALSE,
            before_stodlistor = function(prov) {
              provlista$dfs[[prov]] <- restore$dfs[[prov]]
              provlista$metas[prov, ] <- restore$metas[prov, ]
            },
            after_stodlistor = function(prov) {
              provlista$dfs[[prov]] <- restore$dfs[[prov]]
              provlista$metas[prov, ] <- restore$metas[prov, ]
              update_select_from_metas(prov)
              if (!is.na(provlista$metas[prov, "protokollnummer"])) {
                output[[prov_io(prov, "protokollnummer")]] <- shiny::renderText(
                  paste0("- Protokollnummer: ", as.character(provlista$metas[prov, "protokollnummer"])))
              }
            })
          } else {
            provlista$dfs[[prov]] <- restore$dfs[[prov]]
            provlista$metas[prov, ] <- restore$metas[prov, ]
            update_select_from_metas(prov)
            if (!is.na(provlista$metas[prov, "protokollnummer"])) {
              output[[prov_io(prov, "protokollnummer")]] <- shiny::renderText(
                paste0("- Protokollnummer: ", as.character(provlista$metas[prov, "protokollnummer"])))
            }
          }
        }
      )

      logfine("mod_provlista.R - observeEvent(restore$update(), {}): finished")
    }, ignoreInit = TRUE)
  })
}
