shinyUI(pageWithSidebar(
  headerPanel("13C Breath Test"),
  sidebarPanel(
    tags$head(
      tags$style(type='text/css', "select[multiple], select[size] {height: 400px;width:200px;}")
    ),
    selectInput("parComb", "Parameters (multiple: CTRL)", 
                choices = parComb$Pair,multiple=TRUE)
  ),
  mainPanel(
    plotOutput("splomPlot")
  )
))

