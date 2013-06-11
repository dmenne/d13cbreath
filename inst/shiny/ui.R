methodParameters = 
  c("BluckCoward t50/tlag", "BluckCowardPop t50/tlag",
    "Maes t50/tlag","MaesPop t50/tlag","ExpBeta k/m","ExpBeta k/beta")

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
    ),  
    conditionalPanel(
      condition=  "input.tabs=='Bewertung'",
      selectInput("par2D","Welche Parameterpaare sollen dargestellt werden",
                  choices=methodParameters),
      checkboxInput("greenData", "Grün", value = TRUE),
      checkboxInput("blueData", "Blau", value = FALSE),
      checkboxInput("redData", "Rot", value = FALSE),
      checkboxInput("orangeData", "Orange", value = FALSE),
      sliderInput("outlierFak","Outlier-Bereich", 0,maxOutlier,1,0.5),
      checkboxInput("showPoints","Datenpunkte anzeigen",value=FALSE),
      selectInput("kde.package","Auswertemethode",choices=c("ash","ks"))
    )
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Bewertung", plotOutput("decisionPlot", height="auto")),
      tabPanel("Curves",plotOutput("curvePlot", height="auto")),
      tabPanel("Splom", plotOutput("splomPlot", height="auto")),
      id="tabs"
  ))
))

