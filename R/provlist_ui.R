create_prov_ns <- function(name, ns) {
  logdebug("provlist_ui.R - create_prov_ns: called")
  function(id) {
    ns(paste0(name, "_", id))
  }
}

# All ids in provlist_ui uses the provided prov_ns function, which is created above.
# It will put it in the same namespace as mod_provlista, but prefix it with provXX_
# Its almost a custom done namespace, but is done this way to place all the functions
# and data in mod_provlista_server
provlist_ui <- function(prov_ns, name) {
  logdebug("provlist_ui.R - provlist_ui: called")
  shiny::tabPanel(
    id = prov_ns("tabpanel"),
    title = name,
    value = prov_ns("tabpanel"),
    shiny::wellPanel(
      shiny::span(
        shiny::actionButton(inputId = prov_ns("delete_section"), label = "", icon = shiny::icon("trash"),
                            style = "color: white; background-color: red;"),
        style = "float: right;"),
      shiny::h2(name, style = "color: #888888;"),
      shiny::fluidRow(
        shiny::column(3,
                      shiny::selectizeInput(
                        inputId = prov_ns("vavnad"),
                        label = "Vävnad",
                        choices = c(""),
                        options = list(placeholder = "Vävnad/Material typ", highlight = FALSE)
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        inputId = prov_ns("storage"),
                        label = "Förvaringsplats",
                        choices = c(""),
                        options = list(placeholder = "Förvaringsplats", highlight = FALSE)
                      )
        ),
        shiny::column(3,
                      shiny::checkboxInput(inputId = prov_ns("homogenat"),
                                           label = "Homogenat",
                                           value = FALSE)
        ),
        shiny::column(3)
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(3,
                      shiny::selectInput(inputId = prov_ns("provtagningsinst"),
                                         label = "Provtagnings instrument",
                                         choices = c("", "Metallskalpell/Metallpinsett", "Keramikkniv/Plastpinsett")
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        inputId = prov_ns("analystyp"),
                        label = "Analystyp",
                        options = list(placeholder = "Analystyp", highlight = FALSE),
                        choices = c("")
                      )
        ),
        shiny::column(3,
                      shiny::selectInput(inputId = prov_ns("analyslab"),
                                         label = "Analyslab",
                                         choices = c("", "ACES", "SLV", "Umeå")
                      )
        ),
        shiny::column(3,
                      shiny::selectizeInput(
                        inputId = prov_ns("analytiker"),
                        label = "Analytiker",
                        options = list(placeholder = "Analytiker", highlight = FALSE),
                        choices = c("")
                      )
        ),
      ),
      rhandsontable::rHandsontableOutput(prov_ns("provid_table")),
      shiny::br(),
      shiny::actionButton(inputId = prov_ns("klona_provid_fran_forsta"), label = "Kopiera ProvID från första"),
      shiny::actionButton(inputId = prov_ns("sekvens_provid_fran_forsta"), label = "Sekvens av ProvID från första")
      )
      )
}
