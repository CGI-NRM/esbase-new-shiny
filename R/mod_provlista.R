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
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = ns("prov1_provtagningsinst"),
                                           label = "Provtagnings instrument",
                                           choices = c("", "Metallskalpell/Metallpinsett", "Keramikkniv/Plastpinsett")
                        )
             ),
             shiny::div(class = "monospace",
                        DT::DTOutput(outputId = ns("prov1_provid_table"))
             ),
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
    render_provid_table <- function() {
      df <- data.frame(accnr = selected_accnrs(), provid = "", acesnr = "", provvikt = 0)

      colnames(df) <- c("accnr", "provid", "acesnr", "provvikt")

      provid_table$df <- df

# Hantera ACES nr endast om 'prov1' är ACES
# Hantera homogenat med en/samma provid och provvikt samt den extra kolumnen delvikt

      output$prov1_provid_table <- DT::renderDT(
        cbind(
          provid_table$df,
          data.frame(rep(as.character(div(style = "width=100%;")), nrow(provid_table$df)))
        ),
        options = list(
          paging = FALSE,
          processing = FALSE,
          filter = "none",
          dom = "t",
          ordering = FALSE,
          autoWidth = TRUE,
          columnDefs = list(
            list(width = "20px", targets = c(0)),
            list(width = "110px", targets = 1:4)
          )
        ),
        edit = list(target = "column", disable = list(columns = c(0, 1, 5))),
        escape = FALSE,
        selection = "none",
        server = TRUE,
        colnames = c("AccNR", "ProvID", "ACES NR", "Provvikt (g)", "")
      )
      provid_table$proxy <- DT::dataTableProxy("prov1_provid_table")
    }

    update_provid_table <- function() {
      DT::replaceData(provid_table$proxy,
      cbind(
        provid_table$df,
        data.frame(rep(as.character(div(style = "width=100%;")), nrow(provid_table$df)))
      ),
      resetPaging = FALSE)
    }

    handle_provid_table_cell_edit <- function() {
      changed <- FALSE
      for (row in seq_len(nrow(input$prov1_provid_table_cell_edit))) {
        i <- input$prov1_provid_table_cell_edit[row, "row"]
        j <- input$prov1_provid_table_cell_edit[row, "col"]
        v <- input$prov1_provid_table_cell_edit[row, "value"]
        if (j == 0 || is.na(colnames(provid_table$df)[j]) || colnames(provid_table$df)[j] == "accnr") {
          next
        } else if (colnames(provid_table$df)[j] == "provid") {
          valid <- esbaser::accnr_validate(v)
          empty <- v == "" || v == "-"

          if (empty || !valid) {
            v <- ""
          }

          if (v != provid_table$df[i, "provid"]) {
            if (valid) {
              v <- v %>% esbaser::accnr_parse() %>% esbaser::accnr_sprint()
            }
            provid_table$df[i, "provid"] <- v
            changed <- TRUE
          }

          if (!empty && !valid) {
            changed <- TRUE
            shiny::showNotification(
              "Invalid ProvID format. Please enter on the form Q2022/12345 or Q202212345.", duration = 30, type = "error")
          }
        }
      }

      if (changed) {
        update_provid_table()
      }
    }

    klona_provid_fran_forsta <- function() {
      if (esbaser::accnr_validate(provid_table$df[1, "provid"])) {
        first_provid <- esbaser::accnr_parse(provid_table$df[1, "provid"])
        provid_table$df[, "provid"] <- esbaser::accnr_sprint(first_provid)
        update_provid_table()
      } else {
        shiny::showNotification("Invalid ProvID in first position.", duration = 15, type = "error")
      }
    }

    sekvens_provid_fran_forsta <- function() {
      if (esbaser::accnr_validate(provid_table$df[1, "provid"])) {
        first_provid <- esbaser::accnr_parse(provid_table$df[1, "provid"])
        for (row in seq_len(length(selected_accnrs()))) {
          provid_table$df[row, "provid"] <- esbaser::accnr_sprint(esbaser::accnr_add(first_provid, row - 1))
        }
        update_provid_table()
      } else {
        shiny::showNotification("Invalid ProvID in first position.", duration = 15, type = "error")
      }
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
      shiny::isolate(render_provid_table())
    })

    shiny::observeEvent(input$prov1_provid_table_cell_edit, {
      handle_provid_table_cell_edit()
    })

    shiny::observeEvent(input$prov1_klona_provid_fran_forsta, {
      klona_provid_fran_forsta()
    })

    shiny::observeEvent(input$prov1_sekvens_provid_fran_forsta, {
      sekvens_provid_fran_forsta()
    })
  })
}
