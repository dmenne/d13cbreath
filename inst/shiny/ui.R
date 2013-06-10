shinyUI(pageWithSidebar(
  headerPanel("13C Breath Test"),
  sidebarPanel(
    tags$head(
      tags$style(type='text/css', "select[multiple], select[size] {height: 320px;width:200px;}")
    ),
    conditionalPanel(
      condition= "input.tabs=='Splom'",
      selectInput("parComb", "2 or more parameters (multiple: CTRL)", 
                  selected=parComb$Pair[c(8,10,14)],
                  choices = parComb$Pair,multiple=TRUE),
      sliderInput("plotQuantile","Plot quantile %, for same parameters, e.g.t50",            
                  0,10,1)
    )
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Splom", plotOutput("splomPlot", height="auto")),
      tabPanel("Curves",plotOutput("curvePlot", height="auto")),
      id="tabs"
  ))
))
