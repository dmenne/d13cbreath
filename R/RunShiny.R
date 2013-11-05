#' @title Runs Shiny app to display data
#' @description Start the Shiny App
#' @examples
#' \dontrun{
#' # This example creates a database in the temporary directory and
#' # fills it with simulated data, the displays the Shiny page.
#' library(D13CBreath)
#' # The databasePath is passed to global.R in shiny
#' databasePath = CreateSimulatedBreathTestDatabase()
#' RunShiny()
#' }
#' @export
RunShiny = function(){
  shiny::runApp(system.file('shiny', package='D13CBreath'),port=8100)
}
