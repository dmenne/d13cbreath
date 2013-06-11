shinyServer(function(input, output,session) {
  output$splomPlot <- renderPlot({
    p = PlotPairs(input$parComb,input$plotQuantile)
    if (!is.null(p)) print(p)
  },height=700)
  
  output$curvePlot <- renderPlot({
    p = PlotCurves(colorsToShow())
    if (!is.null(p)) print(p)
  },height=500)

  colorsToShow <- reactive({
    showColors = NULL
    if (input$greenData)  showColors=c(showColors,"green")
    if (input$blueData)   showColors=c(showColors,"blue")
    if (input$orangeData) showColors=c(showColors,"orange")
    if (input$redData)    showColors=c(showColors,"red")
    showColors
  })
  
  output$decisionPlot <- renderPlot({
    pd = str_match(input$par2D,"(\\w*) +(\\w*)/(\\w*)")
    if (length(pd)!=4) return(NULL);
    DecisionPlot(con, pars,
                 method=pd[2], 
                 parameters = pd[3:4],
                 showColors = colorsToShow(),
                 showPoints= input$showPoints,
                 kde.package = input$kde.package,
                 outlierFak = maxOutlier- input$outlierFak)
    
  },height=700)
  
  #  observe({
#    selectedTab <<-  input$tabs
#  })
#  observe({
#    updateTabsetPanel(session, "tabs", selected = selectedTab)
#  })
  
})


