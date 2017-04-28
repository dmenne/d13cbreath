[![Travis-CI Build Status](https://travis-ci.org/dmenne/d13cbreath.svg?branch=master)](https://travis-ci.org/dmenne/d13cbreath)
[![Coverage Status](https://coveralls.io/repos/github/dmenne/d13cbreath/badge.svg?branch=master)](https://coveralls.io/github/dmenne/d13cbreath?branch=master)

D13CBreath: Processing C13 breath test data
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

## This package is mostly obsolete 

It has been replaced by packages [dmenne/breathtestcore](https://github.com/dmenne/breathtestcore), [dmenne/breathteststan](https://github.com/dmenne/breathteststan) and [dmenne/breathtestshiny](https://github.com/dmenne/breathtestshiny). Use this package for legacy applications only.

## What it does

* Reads several formats of 13C data: IRIS/Wagner (partially supported), BreathID
* Creates sample data and writes sample SQLite database; default database is in `<HOME>/Gastrobase2/Gastrobase2.sqlite`.
* Fits Beta-Exponential nonlinear function using `nls`.
* Fits Wagner-Nelson, with terminal slope estimated from Bluck-Coward fit.
* Computes population fit with `nlme` using all data in database
* See documentation of function `bluckCoward` for examples, including how to use
  `nlme` for stuborn or highly incomplete cases.
* For additional examples, see the folder `tests/testthat` of the source package
* [A comparison of results with nls, nlme](http://menne-biomed.de/blog/de/breath-test-stan) and Bayesian [Stan](http://www.mc-stan.org).
* See the example in the documentation of `t50BluckCoward` for a comparison with published data. Most cases agree with those published here, but there are some exceptions; possible a typo in the published table?

The software is being developed in cooperation with the ETH and Department of Gastroenterology of the University Hospital of Zürich, Switzerland. Thanks to Andreas Steingötter, Benjamin Misselwitz, Mark Fox and Werner Schwizer. And to Kirill Müller for supporting the RSqlite package.

## How to install
To install the functions, use

```
devtools::install_github("d13cbreath","dmenne")
```

## Using shiny to display the results

The Shiny package is the test bed for new ways to display the data. To run
it with a sample database, use:

```
library(D13CBreath)
# remove the following line if there is a database in the default location
# <HOME>/Gastrobase2/Gastrobase2sqlite
databasePath = CreateSimulatedBreathTestDatabase()
RunShiny()
```

These function were developed under Windows 7 and have not been tested under Linux.

## Known problems
The coupling between computation and database storage is rather tight because of requirements of the first installation. Some refactoring must be done to allow using the computational part without data from the database.


__Reference__: 

* Bluck LJC and Coward WA (2006) Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy. Physiol. Meas. 27 279-89

* Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate breath test for gastric emptying. J. Breath Res. 3, http://iopscience.iop.org/1752-7163/3/3/034002/

* Bluck, LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian hierarchical methods to interpret  the 13C-octanoic acid breath  test for gastric emptying. Digestion 83_96-107.
