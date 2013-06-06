library(lattice)
library(reshape2)
library(stringr)

#parc = parComb[6:10,"Pair"]

shinyServer(function(input, output) {
  output$splomPlot <- renderPlot({
    p = PlotPairs(input$parComb)
    if (!is.null(p)) print(p)
  },height=700)
  
})
