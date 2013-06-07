
shinyServer(function(input, output,session) {
  output$splomPlot <- renderPlot({
    p = PlotPairs(input$parComb,input$plotQuantile)
    if (!is.null(p)) print(p)
  },height=700)
  
  output$curvePlot <- renderPlot({
    p = PlotCurves()
    if (!is.null(p)) print(p)
  },height=500)
  
  observe({
    selectedTab <<-  input$tabs
    print(selectedTab)
  })
  observe({
    updateTabsetPanel(session, "tabs", selected = selectedTab)
  })
  
})


