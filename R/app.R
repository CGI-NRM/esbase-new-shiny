# Package names
packages <- c("shiny", "shinyBS", "DT", "remotes", "stringr", "dplyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

remotes::install_github("cgi-nrm/esbaser")

source("mod_biologdata.R")
source("mod_provberedning.R")
source("mod_provlista.R")

ui <- shiny::fluidPage(
  includeCSS("www/style.css"),
  shiny::titlePanel("Esbase New"),
  mod_provberedning_ui("provberedning")
)

server <- function(input, output, session) {
  mod_provberedning_server("provberedning")
}

shiny::shinyApp(ui, server)
