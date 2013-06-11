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
      condition= "input.tabs=='Multiple'",
      selectInput("parComb", "2 or more parameters (multiple: CTRL)", 
                  selected=parComb$Pair[c(8,10,14)],
                  choices = parComb$Pair,multiple=TRUE),
      sliderInput("plotQuantile","Outlier entfernen",            
                  0,10,1)
    ),  
    
    conditionalPanel(
      condition=  "input.tabs=='Bewertung'",
      selectInput("par2D","Welche Parameterpaare sollen dargestellt werden",
                  choices=methodParameters),
      selectInput("kde.package","Interpolations-Methode",choices=c("ash","ks")),
      sliderInput("outlierFak","Outlier-Bereich", 0,maxOutlier,1,0.5),
      helpText("Die Graphik ergibt sich als Verteilung von allen Datenpunkten in der Datenbank."),
      checkboxInput("showPoints","Datenpunkte anzeigen",value=FALSE),
      HTML("<hr>")
    ),
    
    conditionalPanel(
      condition=  "input.tabs!='Multiple'",
      helpText("Auswahl der Aufnahmen oder Patienten, die angezeigt werden sollten.
               Die Farbmarker können in GastroBase2 mit der rechten Maustaste gesetzt werden."),
      checkboxInput("greenData", "Grün", value = TRUE),
      checkboxInput("blueData", "Blau", value = FALSE),
      checkboxInput("redData", "Rot", value = FALSE),
      checkboxInput("orangeData", "Orange", value = FALSE),
      HTML("<hr>"),
      helpText("Eine 13C-Atemtest-Aufnahme wird häufig durch die Kombination von zwei 
Parametern charakterisiert. Einer der beiden ist üblicherweise ein Wert für die 
Halbwertszeit t50 oder t1/2, wobei je nach Methode um den Faktor 3 unterschiedliche Werte 
herauskommen können. Der andere ist ein Parameter, der als 'Lag' interpretiert
wird, dessen Bedeutung jedoch sehr diffus ist."), 
      helpText("Im dunkelgrünen Bereich liegen 50% der gemessenen Atemtestwerte. Man kann annehmen,
dass die 'mittlere Mehrheit' eher normal ist; diese Annahme ist vermutlich falsch, aber
eine bessere ist mangels Gold-Standard nicht vorhanen. Im hellgrünen und dunkelgrünen 
Bereich zusammen liegen 75%, mit orange 95%, und mit rot 99% der Atemtestwerte."),
      helpText("Wenn ein Patient mit mehreren Aufnahmen markiert wurde, zeigen weiße 
Pfeile an, wie sich die Werte über die Zeit entwickelt haben.")
    )
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Bewertung", plotOutput("decisionPlot", height="auto")),
      tabPanel("PDR Verläufe",plotOutput("curvePlot", height="auto")),
      tabPanel("Multiple", plotOutput("splomPlot", height="auto")),
      id="tabs"
  ))
))

