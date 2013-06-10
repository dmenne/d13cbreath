D13CBreath: Processing C13 breath test data
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

The software is being developed in cooperation with the ETH and Department of Gastroenterology of the University Hospital of Zürich, Switzerland.  
Thanks to Andreas Steingötter and Werner Schwizer.

__!! The package is under active development, and functions may change any time. Be warned !!__

* Reads several formats of 13C data: IRIS/Wagner (partially supported), BreathID
* Creates sample data and writes sample SQLite database; default database is in `<HOME>/Gastrobase2/Gastrobase2.sqlite`.
* Fits Beta-Exponential nonlinear function using `nls`.
* Fits Wagner-Nelson, with terminal slope estimated from Bluck/Coward fit.
* Computes population fit with `nlme` using all data in database
* See documentation of function `bluckCoward` for examples, including how to use
  `nlme` for stuborn or highly incomplete cases.
* For additional examples, see the folder `inst/tests` of the source package
* [A comparison of results with nls, nlme](https://bitbucket.org/dmenne/d13cbreath/downloads/BreathTestBayes.html) and the Bayesian [Stan](http://www.mc-stan.org).
* See the documenation of `t50BluckCoward` for a comparison with published data. Most cases agree with those published here, but there are some exceptions; possible a typo in the published table?


To install the functions, use
```
devtools::install_bitbucket("d13cbreath","dmenne")
```

The Shiny package is the test bed for new ways to display the data. To run
it with a sample database, use:
```
library(D13CBreath)
# remove the following line if there is a database in the default location
# <HOME>/Gastrobase/Gastrobase2sqlite
databasePath = CreateSimulatedBreathTestDatabase()
RunShiny()
```


These function were developed under Windows 7 and have not been tested under Linux.

__Reference__: 

* Bluck LJC and Coward WA (2006) Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy. Physiol. Meas. 27 279-89

* Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate breath test for gastric emptying. J. Breath Res. 3, http://iopscience.iop.org/1752-7163/3/3/034002/

* "Bluck, LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian hierarchical methods to interpret  the 13C-octanoic acid breath  test for gastric emptying. Digestion 83_96-107.
