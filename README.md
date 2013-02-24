D13CBreath: Processing C13 breath test data
===========================================
dieter.menne@menne-biomed.de

* Reads several formats of <sup>13</sup>C data (IRIS/Wagner, BreathID)
* IRIS format only partially implemented
* Create sample data, and sqlite database, and writes sample SQLite database
* Fits Bluck/Coward, Ghoos self-correcting formula using nls.
* See documentation of bluckCoward for examples, including how to use
  nlme for stuborn or highly incomplete cases.

Reference: Bluck L J C and Coward W A 2006 Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy Physiol. Meas. 27 279-89
