D13CBreath: Processing C13 breath test data
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

This software was developed in cooperation with the ETH and Department of Gastroenterology of the University Hospital of Zürich, Switzerland.  
Thanks to Andreas Steingötter and Werner Schwizer.

* Reads several formats of 13C data: IRIS/Wagner (partially supported), BreathID
* Creates sample data and writes sample SQLite database; default database is in `<HOME>/Gastrobase2/Gastrobase2.sqlite`.
* Fits Bluck/Coward, Ghoos self-correcting formula using `nls`.
* Fits Wagner-Nelson, with terminal slope estimated from Bluck/Coward fit.
* Computes population fit with `nlme` using all data in database
* See documentation of function `bluckCoward` for examples, including how to use
  `nlme` for stuborn or highly incomplete cases.
* For additional examples, see the folder `inst/tests` of the source package

To install the functions, use
```
devtools::install_bitbucket("d13cbreath","dmenne")
```

These function were developed under Windows 7 and have not been tested under Linux.

__Reference__: 

* Bluck LJC and Coward WA (2006) Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy. Physiol. Meas. 27 279-89

* Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate breath test for gastric emptying. J. Breath Res. 3, http://iopscience.iop.org/1752-7163/3/3/034002/