shinyUI(pageWithSidebar(
  headerPanel("13C Atemtest"),
  sidebarPanel(
    tags$head(
      tags$style(type='text/css', 
        "select[multiple], select[size] {height: 320px;width:200px;}
        h1 {font-size: 20px; margin: -10px 0 -10px 0;} ")
    ),
    conditionalPanel(
      condition= "input.tabs=='Multiple'",
      selectInput("parComb", "2 or more parameters (multiple: CTRL)", 
                  selected=parComb$Pair[c(8,10,14)],
                  choices = parComb$Pair,multiple=TRUE),
      sliderInput("plotQuantile","Outlier entfernen",            
                  0,10,1)
    ),  
    
    conditionalPanel(
      condition=  "input.tabs=='Bewertung'",
      selectInput("par2D","Parameterpaar",  choices=methodParameters)
    ),
      
    conditionalPanel(
      condition =  helpMethodConditions[[1]],
      helpText(helpTexts[[1]])
    ),
    conditionalPanel(
      condition =  helpMethodConditions[[2]],
      helpText(helpTexts[[2]])
    ),
    conditionalPanel(
      condition =  helpMethodConditions[[3]],
      helpText(helpTexts[[3]])
    ),
    conditionalPanel(
      condition =  helpMethodConditions[[4]],
      helpText(helpTexts[[4]])
    ),
    conditionalPanel(
      condition =  helpMethodConditions[[5]],
      helpText(helpTexts[[5]])
    ),
    conditionalPanel(
      condition =  helpMethodConditions[[6]],
      helpText(helpTexts[[6]])
    ),
    conditionalPanel(
      condition =  helpMethodConditions[[7]],
      helpText(helpTexts[[7]])
    ),
    
    conditionalPanel(
      condition=  "input.tabs!='Multiple'",
      HTML("<hr>"),      
      helpText("Auswahl der Aufnahmen oder Patienten, deren Daten angezeigt werden sollten.
               Die Farbmarker können in GastroBase2 mit der rechten Maustaste gesetzt werden."),
      checkboxInput("greenData", "Grün", value = TRUE),
      checkboxInput("blueData", "Blau", value = FALSE),
      checkboxInput("redData", "Rot", value = FALSE),
      checkboxInput("orangeData", "Orange", value = FALSE)
      ),
    conditionalPanel(
      condition=  "input.tabs=='Bewertung'",
      selectInput("kde.package", "Interpolations-Methode",choices=c("ash","ks")),
      sliderInput("outlierFak", "Outlier-Bereich", 0,maxOutlier,0,0.5),
      helpText("Die Graphik zeigt die Verteilung von allen Atemtestwerten in der Datenbank."),
      checkboxInput("showPoints","Datenpunkte anzeigen",value=FALSE)
    )  
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Bewertung", plotOutput("decisionPlot", height="auto"),
               helpText(C13HelpText[[1]]),helpText(C13HelpText[[2]]),
               helpText(C13HelpText[[3]])),
      tabPanel("PDR Verläufe",plotOutput("curvePlot", height="auto")),
      tabPanel("Multiple", plotOutput("splomPlot", height="auto")),    
      id="tabs"
  ))
))

