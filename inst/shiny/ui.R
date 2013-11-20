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
      
    lapply(1:length(helpMethodConditions), function(i){
      conditionalPanel(condition=helpMethodConditions[[i]],
                       helpText(HTML(helpTexts[[i]])))
                       
    }),
    conditionalPanel(
      condition=  "input.tabs=='Bewertung'",
      HTML("<hr>")      
    ),
    conditionalPanel(
      condition=  "input.tabs!='Multiple'",
      helpText(HTML("Auswahl der Aufnahmen oder Patienten, deren Daten angezeigt werden sollten.
          Die Farbmarker können in <i>GastroBase2</i> mit der rechten Maustaste gesetzt werden; 
          alternativ können in <i>GastroBase2</i> mit diesem 
          <img src='menu.png' alt='Gastrobase2 menu button'>
          Kurzmenü die letzten vier geladenenen Aufnahmen markiert werden.")),
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
               helpText(HTML(C13HelpText[[1]])),#
               helpText(HTML(C13HelpText[[2]])),
               helpText(HTML(C13HelpText[[3]]))),
      tabPanel("PDR Verläufe",plotOutput("curvePlot", height="auto")),
      tabPanel("Multiple", plotOutput("splomPlot", height="auto")),    
      id="tabs"
  ))
))

