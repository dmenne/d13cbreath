#' @title Runs Shiny app to display data
#' @description Start the Shiny App
#' @export
RunShiny = function(){
  shiny::runApp(system.file('shiny', package='D13CBreath'))
}
