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
             DT::DTOutput(outputId = ns("prov1_provid_table")),
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
    shiny::observeEvent(input$set_limniska, {
      shiny::showNotification("Lägger till Hg, Metall, CLC+BFR, Dioxin och PFAS")
    })

    shiny::observeEvent(input$lagg_till_prov, {
      shiny::showNotification("Lägger till ett prov")
    })

    shiny::observe({
      req(selected_accnrs())

      df <- data.frame(accnr = selected_accnrs(), provid = " ", acesnr = " ", provvikt = 0)

      colnames(df) <- c("AccNR", "ProvID", "ACES NR", "Provvikt (g)")

# Hantera ACES nr endast om 'prov1' är ACES
# Hantera homogenat med en/samma provid och provvikt samt den extra kolumnen delvikt

      output$prov1_provid_table <- DT::renderDT(
        df,
        options = list(
          filter = "none",
          dom = "t",
          ordering = FALSE,
          columnDefs = list(list(width = "200px", targets = "_all"))
        ),
        edit = list(target = "column", disable = list(columns = c(0))),
        rownames = FALSE,
        selection = "none",
        server = FALSE
      )
    })

  })
}
