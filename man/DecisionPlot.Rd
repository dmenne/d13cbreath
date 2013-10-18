\name{DecisionPlot}
\alias{DecisionPlot}
\title{DecisionPlot: 2D contour plot of breath-test variables}
\usage{
  DecisionPlot(con = NULL,
    pars = dbGetQuery(con, "SELECT * from BreathTestParameter")[, -1],
    methods = "Maes", parameters = c("t50", "tlag"),
    prob = c(0.01, 0.05, 0.25, 0.5),
    main = "Breath test parameters",
    kde.package = c("ash", "ks"), brewerPalette = "RdYlGn",
    outlierFak = 3, showColors = NULL, showPoints = TRUE,
    showDateLabels = TRUE)
}
\arguments{
  \item{con}{connection to sqlite database; if missing,
  uses default database.}

  \item{pars}{all parameters in the form as read from table
  \code{BreathTestParameters}.}

  \item{methods}{if one entry is given, e.g \code{ExpBeta,
  BluckCoward, WN, MaesPop}, it is combined with both
  \code{parameters}; if two are given, \code{method} and
  \code{parameters} are paired.}

  \item{parameters}{two parameter names, e.g.
  \code{c("t50","tlag")}.}

  \item{prob}{quantile probabilities; color for these is
  determined by Brewer palette, default \code{RdYlGn}.}

  \item{main}{title for graph}

  \item{kde.package}{package to use for HDI 2D kernel
  smoothing}

  \item{brewerPalette}{color palette name from
  \code{RBrewerColor} to use}

  \item{outlierFak}{the multiples of standard deviations to
  use for outlier removal. Note outliers removed from
  computation. It cannot be ruled out the data point you
  are focussing on has been removed. If in doubt, try
  higher values, i.e. wider ranges to include.}

  \item{showColors}{e.g.
  \code{c("green","blue","red","orange")} to show all
  marked items.}

  \item{showPoints}{should data points contributing to
  decision be shown?}

  \item{showDateLabels}{If TRUE, the record date of the
  \code{showPoints}s records are added as label.}
}
\value{
  None; as a side effect, a plot using standard graphics is
  generated. Note that the image may contain aliasing lines
  when Cairo is avaialable on your system.
  \url{https://groups.google.com/forum/?fromgroups#!topic/shiny-discuss/ysac9FqOQLI}
}
\description{
  Creates a 2D percentile plot of a pair of breath test
  variables. By default, the green central regions contains
  50% of the data points, determined using a
  highest-density-interval (HDI) method.
}
\examples{
databasePath = CreateSimulatedBreathTestDatabase()
con = OpenSqliteConnection(databasePath)
opt = par(mfrow=c(2,2))
DecisionPlot(con, showColors="red", main="Method ash")
# DecisionPlot(con, showColors=c("orange","blue"),main= "Method ks", kde.package="ks")
DecisionPlot(con, method="BluckCowardPop",showColors=c("red"), main= "BluckCoward Population" )
# For Wagner-Nelson, there is no lag, we must use it from a different source
DecisionPlot(con, method=c("MaesPop","Maes"),parameters=c("t50","t50"),
 showColors=c("green","blue"),showPoints=FALSE,
   main="No points, Spectral", brewerPalette = "Spectral")
par(opt)
dbDisconnect(con)
}
\author{
  Dieter Menne, \email{dieter.menne@menne-biomed.de}
}

