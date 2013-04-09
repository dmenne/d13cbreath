D13CBreath: Processing C13 breath test data
===========================================

Dieter Menne
Menne Biomed Consulting Tübingen, Germany 
http://www.menne-biomed.de

dieter.menne at menne-biomed.de 

This software was developed in cooperation with the ETH and Department of Gastroenterology of the University Hospital of Zürich, Switzerland (Andreas Steingötter, Werner Schwizer)

* Reads several formats of 13C data (IRIS/Wagner, BreathID)
* IRIS format only partially implemented
* Create sample data, and sqlite database, and writes sample SQLite database
* Fits Bluck/Coward, Ghoos self-correcting formula using nls.
* Computes population fit with nlme using all data in database
* See documentation of bluckCoward for examples, including how to use
  nlme for stuborn or highly incomplete cases.

Reference: Bluck L J C and Coward W A 2006 Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy Physiol. Meas. 27 279-89
