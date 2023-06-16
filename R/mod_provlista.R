mod_provlista_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns("provlista"),
             shiny::h3("Provlista"),
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = ns("prov1_analystyp"),
                                           label = "Analystyp",
                                           choices = c("", "Hg", "Metall", "CLC+BFR", "Dioxin", "PFAS")
                        )
             ),
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = ns("prov1_analyslab"),
                                           label = "Analyslab",
                                           choices = c("", "ACES", "SLV", "Umeå")
                        )
             ),
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = ns("prov1_analytiker"),
                                           label = "Analytiker",
                                           choices = c("", "Marcus Sundbom", "Marie Aune", "Peter Haglund", "Lutz Ahrens")
                        )
             ),
             shiny::tags$br(),
             shiny::div(
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = ns("prov1_provtagningsinst"),
                                           label = "Provtagnings instrument",
                                           choices = c("", "Metallskalpell/Metallpinsett", "Keramikkniv/Plastpinsett")
                        )
             ),
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = ns("prov1_vavnad"),
                                           label = "Vävnad",
                                           choices = c("", "Muskell", "Lever")
                        )
             ),
             shiny::div(style = "display:inline-block",
                        shiny::checkboxInput(inputId = ns("prov1_homogenat"),
                                             label = "Homogenat",
                                             value = FALSE)
             )),
             shiny::tags$br(),
             rhandsontable::rHandsontableOutput(ns("provid_table")),
             shiny::actionButton(inputId = ns("prov1_klona_provid_fran_forsta"), label = "Kopiera ProvID från första"),
             shiny::actionButton(inputId = ns("prov1_sekvens_provid_fran_forsta"), label = "Sekvens av ProvID från första"),
             shiny::hr(),
             shiny::actionButton(inputId = ns("lagg_till_prov"), label = "Lägg till prov"),
             shiny::br(),
             shiny::actionButton(inputId = ns("set_limniska"), label = "Set Limniska Programmet Prover")
             )
}

mod_provlista_server <- function(id, selected_accnrs) {
  shiny::moduleServer(id, function(input, output, session) {
    provid_table <- shiny::reactiveValues()

    # ---------- FUNCTIONS ----------
    create_provid_table <- function() {
      provid_table$df <- data.frame(
        accnr = selected_accnrs(),
        provid = "",
        aces = "",
        delvikt = as.numeric(NA),
        provvikt = as.numeric(NA))
    }

    handle_provid_table_change <- function(new_table) {
      if (is.null(provid_table$df) || nrow(new_table) != nrow(provid_table$df)) {
        shiny::showNotification(
          paste0(
            "Cannot add rows to this table. Add more rows under 'Biologdata' to increase the number of rows in this table"
          ), type = "warning", duration = 30)
        render_provid_table()
        return()
      }
      changed <- FALSE
      for (col in colnames(new_table)) {
        cells_changed <- provid_table$df[col] != new_table[col]
        cells_changed <- cells_changed | xor(is.na(provid_table$df[col]), is.na(new_table[col]))

        rows <- which(cells_changed)
        provid_table$df[rows, col] <- new_table[rows, col]

        if (length(rows)) {
          changed <- TRUE
        }
      }
      if (changed) {
        render_provid_table()
      }
    }

    render_provid_table <- function() {
      output$provid_table <- rhandsontable::renderRHandsontable({
        cols <- c("accnr", "provid")
        if (input$prov1_homogenat) {
          cols <- c(cols, "delvikt")
        }
        if (input$prov1_analyslab == "ACES") {
          cols <- c(cols, "aces")
        }
        cols <- c(cols, "provvikt")
        df <- provid_table$df[cols]

        hot <- rhandsontable::rhandsontable(df, rowHeaders = FALSE, overflow = "visible") |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE, allowComments = FALSE) |>
        rhandsontable::hot_col("accnr", readOnly = TRUE) |>
        rhandsontable::hot_col("provid", renderer = "
                               function (instance, td, row, col, prop, value, cellProperties) {
                                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                                 if (cellProperties.readOnly) {
                                  td.style.background = 'lightgray';
                                  td.style.color = 'gray';
                                  if (value === null || value === '') {
                                    td.innerText = '-';
                                  }
                                 } else {
                                   re = /^Q[0-9]{4}\\-[0-9]{5}$/;
                                   if (value === null || value.match(re) === null) {
                                     td.style.background = 'red';
                                   } else {
                                     td.style.background = 'white';
                                   }
                                 }
                               }
                               ") |>
        rhandsontable::hot_col(which(colnames(df) != "provid"), renderer = "
                               function (instance, td, row, col, prop, value, cellProperties) {
                                 Handsontable.renderers.TextRenderer.apply(this, arguments);

                                 if (cellProperties.readOnly) {
                                   td.style.background = 'lightgray';
                                   td.style.color = 'gray';
                                   if (value === null || value === '') {
                                     td.innerText = '-';
                                   }
                                 } else {
                                   customRenderer(instance, td, row, col, prop, value, cellProperties)
                                 }
                               }
                               ")

        hot <- rhandsontable::hot_row(hot, which(selected_accnrs() == ""), readOnly = TRUE)

        hot
      })
    }

    klona_provid_fran_forsta <- function() {
      if (!esbaser::provid_validate(provid_table$df[1, "provid"])) {
        shiny::showNotification(
          "Invalid or missing ProvID in first row. Please enter on the form 'Q2022-12345'",
          type = "warning")
        return()
      }

      new_table <- provid_table$df
      new_table[, "provid"] <- provid_table$df[1, "provid"]
      handle_provid_table_change(new_table)
    }

    sekvens_provid_fran_forsta <- function() {
      if (!esbaser::provid_validate(provid_table$df[1, "provid"])) {
        shiny::showNotification(
          "Invalid or missing ProvID in first row. Please enter on the form 'Q2022-12345'",
          type = "warning")
        return()
      }

      parsed <- esbaser::provid_parse(provid_table$df[1, "provid"])
      new_table <- provid_table$df
      new_table[, "provid"] <- unlist(
        lapply(
          seq_len(nrow(new_table)),
          function(i) {
            esbaser::provid_sprint(esbaser::provid_add(parsed, i - 1))
          }
        )
      )

      handle_provid_table_change(new_table)
    }

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$set_limniska, {
      shiny::showNotification("Lägger till Hg, Metall, CLC+BFR, Dioxin och PFAS")
    })

    shiny::observeEvent(input$lagg_till_prov, {
      shiny::showNotification("Lägger till ett prov")
    })

    shiny::observe({
      selected_accnrs()
      current_df <- shiny::isolate(provid_table$df)
      if (is.null(current_df) || nrow(current_df) != length(selected_accnrs())) {
        shiny::isolate(create_provid_table())
        shiny::isolate(render_provid_table())
      } else {
        current_df[, "accnr"] <- selected_accnrs()
        shiny::isolate(handle_provid_table_change(current_df))
      }
    })

    shiny::observeEvent(input$prov1_klona_provid_fran_forsta, {
      klona_provid_fran_forsta()
    })

    shiny::observeEvent(input$prov1_sekvens_provid_fran_forsta, {
      sekvens_provid_fran_forsta()
    })

    shiny::observeEvent(input$provid_table, {
      new_table <- rhandsontable::hot_to_r(input$provid_table)
      handle_provid_table_change(new_table)
    })
  })
}
