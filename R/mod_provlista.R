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
    # ---------- FUNCTIONS ----------

    output$provid_table <- rhandsontable::renderRHandsontable({ 
      df <- data.frame(
        A = LETTERS[1:11],
        B = 10:20,
        C = rep(TRUE, 11)
      )

      rhandsontable::rhandsontable(df)
    })


    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$set_limniska, {
      shiny::showNotification("Lägger till Hg, Metall, CLC+BFR, Dioxin och PFAS")
    })

    shiny::observeEvent(input$lagg_till_prov, {
      shiny::showNotification("Lägger till ett prov")
    })

#    shiny::observe({
#      selected_accnrs()
#      shiny::isolate(render_provid_table())
#    })
  })
}
