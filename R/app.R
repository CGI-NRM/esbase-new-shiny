library(shiny)
library(shinyBS)
library(DT)
library(esbaser)

source("mod_biologdata.R")
source("mod_provberedning.R")
source("mod_provlista.R")

ui <- shiny::fluidPage(
               shiny::titlePanel("Esbase New"),
               mod_provberedning_ui("provberedning")
)

server <- function(input, output, session) {
  mod_provberedning_server("provberedning")
}

shiny::shinyApp(ui, server)
