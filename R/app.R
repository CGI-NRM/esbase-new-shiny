# Package names
packages <- c("shiny", "shinyBS", "shinyjs", "DT", "remotes", "stringr", "dplyr",
              "rhandsontable", "htmlwidgets", "tibble", "knitr", "kableExtra", "logging")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

remotes::install_github("cgi-nrm/esbaser")

# Must be loaded to be included in app
library(shinyBS)

# Convenient
library(tibble)
library(dplyr)

# Logging
library(logging)
logReset()
addHandler(writeToConsole)
setLevel("DEBUG")

# Sourcing
source("mod_biologdata.R")
source("mod_provberedning.R")
source("mod_provlista.R")
source("mod_validera.R")

source("provlist_ui.R")
source("report.R")

source("rhandsontable_js.R")
source("kableExtraExtra.R")

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::includeCSS("www/style.css"),
  shiny::titlePanel("Esbase New"),
  mod_provberedning_ui("provberedning")
#  shiny::div(style = "height: 100vh")
)

server <- function(input, output, session) {
  loginfo("app.R: server started")
  conn <- esbaser::connect_to_database()

  # ---------- MODULE SERVERS ----------
  mod_provberedning_server("provberedning", conn)
}

shiny::shinyApp(ui, server)
