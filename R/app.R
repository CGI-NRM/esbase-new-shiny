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
library(tidyr)
library(lubridate)

# Logging
library(logging)
logReset()
addHandler(writeToConsole)
setLevel("FINER")

# Usefull funtion to pipe into when not all elements/columns contains data

# Sourcing utils
source("rhandsontable_js.R")
source("kableExtraExtra.R")
source("dataHolder.R") # DataHolder object, replacement for reactiveValues without reactiveness
source("utils.R")

source("provlist_ui.R")
source("report.R")
source("utils_report.R")

# Sourcing modules
source("mod_biologdata.R")
source("utils_biologdata.R")
source("mod_provberedning.R")
source("mod_provlista.R")
source("utils_provlista.R")
source("mod_material.R")
source("mod_login.R")

db_password <- readLines("/run/secrets/db_password")
db_username <- Sys.getenv()["DB_USERNAME"]
db_host <- Sys.getenv()["DB_HOST"]
db_dbname <- Sys.getenv()["DB_DBNAME"]

# Logins
if (!dir.exists("active_sessions/")) {
  dir.create("active_sessions")
}

# Saved provberedningar
if (!dir.exists("provberedningar/")) {
  dir.create("provberedningar")
}

# Shiny App
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::includeScript("www/js.cookie.js"),
  shiny::includeCSS("www/style.css"),
  shiny::titlePanel("Esbase New"),
  mod_login_ui("login")
)

server <- function(input, output, session) {
  loginfo("app.R - server: server started")

  conn <- tryCatch(
    esbaser::connect_to_database(host = db_host, username = db_username, password = db_password, dbname = db_dbname),
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

  account <- dataHolder(
    id = 0 # Will be set by mod_login
  )

  # ---------- MODULE SERVERS ----------
  mod_login_server("login", db = db, account = account)
}

shiny::shinyApp(ui, server)
