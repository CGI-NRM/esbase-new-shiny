library(shiny)
library(shinyBS)
library(DT)
library(esbaser)

source("mod_biologdata.R")

ui <- shiny::fluidPage(
               shiny::titlePanel("Esbase New"),
               mod_biologdata_ui("biologdata")
)

server <- function(input, output, session) {
  output$data_loaded <- shiny::renderText(
                                 esbaser::load_data("Hello here is the file")
                               )

  mod_biologdata_server("biologdata")
}

shiny::shinyApp(ui, server)
