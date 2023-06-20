create_prov_ns <- function(name, ns) {
  function(id) {
    ns(paste0(name, "_", id))
  }
}

# All ids in provlist_ui uses the provided prov_ns function, which is created above.
# It will put it in the same namespace as mod_provlista, but prefix it with provXX_
# Its almost a custom done namespace, but is done this way to place all the functions
# and data in mod_provlista_server
provlist_ui <- function(prov_ns, name) {
  shiny::div(id = prov_ns("div"),
             shiny::h2(name, style = "color: #888888;"),
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = prov_ns("analystyp"),
                                           label = "Analystyp",
                                           choices = c("", "Hg", "Metall", "CLC+BFR", "Dioxin", "PFAS")
                        )
             ),
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = prov_ns("analyslab"),
                                           label = "Analyslab",
                                           choices = c("", "ACES", "SLV", "Umeå")
                        )
             ),
             shiny::div(style = "display:inline-block",
                        shiny::selectInput(inputId = prov_ns("analytiker"),
                                           label = "Analytiker",
                                           choices = c("", "Marcus Sundbom", "Marie Aune", "Peter Haglund", "Lutz Ahrens")
                        )
             ),
             shiny::tags$br(),
             shiny::div(
               shiny::div(style = "display:inline-block",
                          shiny::selectInput(inputId = prov_ns("provtagningsinst"),
                                             label = "Provtagnings instrument",
                                             choices = c("", "Metallskalpell/Metallpinsett", "Keramikkniv/Plastpinsett")
                          )
               ),
               shiny::div(style = "display:inline-block",
                          shiny::selectInput(inputId = prov_ns("vavnad"),
                                             label = "Vävnad",
                                             choices = c("", "Muskell", "Lever")
                          )
               ),
               shiny::div(style = "display:inline-block",
                          shiny::checkboxInput(inputId = prov_ns("homogenat"),
                                               label = "Homogenat",
                                               value = FALSE)
               )),
             shiny::tags$br(),
             rhandsontable::rHandsontableOutput(prov_ns("provid_table")),
             shiny::actionButton(inputId = prov_ns("klona_provid_fran_forsta"), label = "Kopiera ProvID från första"),
             shiny::actionButton(inputId = prov_ns("sekvens_provid_fran_forsta"), label = "Sekvens av ProvID från första"),
             shiny::hr()
             )
}
