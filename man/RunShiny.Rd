\name{RunShiny}
\alias{RunShiny}
\title{Runs Shiny app to display data}
\usage{
RunShiny()
}
\description{
Start the Shiny App
}
\examples{
\dontrun{
# This example creates a database in the temporary directory and
# fills it with simulated data, the displays the Shiny page.
library(D13CBreath)
# The databasePath is passed to global.R in shiny
databasePath = CreateSimulatedBreathTestDatabase()
RunShiny()
}
}

