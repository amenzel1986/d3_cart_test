shinyUI(
  tagList(
    tags$head(
      tags$link(
        href = "http://timelyportfolio.github.io/rCharts_rpart/css/treestyle.css"
        ,rel = "stylesheet"
      )
    ),
  fluidPage( 
  fluidRow(
#    column(3, selectInput("dataset", "Choose a Dataset:", 
#                           choices = c("rock", "pressure", "cars"))),
    column(3, uiOutput("selectDV")),
    column(6, uiOutput("selectPreds")),
    column(3, sliderInput("minsplit", "Select Minimum Split Size", 2, 20, 5))),
  h5("Interactive Classification and Regression Tree for mtcars Dataset"),
  fluidRow(
    #tableOutput("test"),
    #        tableOutput("rpk.text"),
            column(12, showOutput("plot", "dimple")),
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ))
            
)))
