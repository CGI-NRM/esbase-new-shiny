# Package names
packages <- c("shiny", "shinyBS", "shinyjs", "DT", "remotes", "stringr", "dplyr", "rlang", "fastmap",
              "rhandsontable", "htmlwidgets", "tibble", "knitr", "kableExtra", "logging", "lubridate")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

if (isFALSE("esbaser" %in% rownames(installed.packages()))) {
  remotes::install_github("cgi-nrm/esbaser")
}

# Must be loaded to be included in app
library(shinyBS)

# Convenient
library(tibble)
library(dplyr)
library(lubridate)

# Logging
library(logging)
logReset()
addHandler(writeToConsole)
setLevel("FINE")

# Usefull funtion to pipe into when not all elements/columns contains data

# Sourcing utils
source("rhandsontable_js.R")
source("kableExtraExtra.R")
source("dataHolder.R") # DataHolder object, replacement for reactiveValues without reactiveness
source("utils.R")

source("provlist_ui.R")
source("report.R")

# Sourcing modules
source("mod_biologdata.R")
source("utils_biologdata.R")
source("mod_provberedning.R")
source("mod_provlista.R")
source("mod_material.R")

# Shiny App
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::includeCSS("www/style.css"),
  shiny::titlePanel("Esbase New"),
  mod_provberedning_ui("provberedning")
)

server <- function(input, output, session) {
  loginfo("app.R - server: server started")

  conn <- tryCatch(
    esbaser::connect_to_database(),
    error = function(cond) {
      logdebug(paste0("Could not connect to database:", cond))
      shiny::showNotification(paste0("Could not connect to database: ", cond), duration = NULL, type = "error")
      return(FALSE)
    }
  )

  if (isFALSE(conn)) {
    loginfo("app.R - server: could not connect to database. Aborting")
    return()
  }
  loginfo("app.R - server: connection made to database")

  shiny::onStop(function() {
    loginfo("app.R - server: stopped")
    esbaser::disconnect_from_database(conn)
  })

  load_start <- Sys.time()
  db <- dataHolder(
    conn = conn,
    locality = esbaser::get_locality(conn),
    country = esbaser::get_country(conn),
    county = esbaser::get_county(conn),
    catalog = esbaser::get_catalog(conn),
    coast = esbaser::get_coast(conn),
    gender = esbaser::get_gender(conn),
    province = esbaser::get_province(conn),
    species = esbaser::get_species(conn),
    material_type = esbaser::get_material_type(conn),
    material_storage = esbaser::get_material_storage(conn),
    project = esbaser::get_project(conn),
    person = esbaser::get_person(conn),
    analysis_type = esbaser::get_analysis_type(conn)
  )
  load_end <- Sys.time()

  logdebug(paste0("app.R - server: loading tables took ", format(load_end - load_start)))

  # ---------- MODULE SERVERS ----------
  mod_provberedning_server("provberedning", db)
}

shiny::shinyApp(ui, server)
