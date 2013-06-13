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

  #https://gist.github.com/jcheng5/5616777
  #https://groups.google.com/forum/?fromgroups#!searchin/shiny-discuss/multiple/shiny-discuss/fm0yERuNkIA/eiD19ePqcK0J
  
  output$decisionPlot <- renderPlot({
    pd = str_match(input$par2D,"(\\w*) +(\\w*)/(\\w*) *(\\w*)")  
    if (length(pd)!=5) return(NULL);
    if (pd[5] == ""){
      methods = pd[2]
      parameters = pd[3:4]
    } else {
      methods = c(pd[2], pd[4])
      parameters = c(pd[3], pd[5])
    }   
    DecisionPlot(con, pars,
                 methods = methods, 
                 parameters = parameters,
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

