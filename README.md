# PIwebAPI4R

OsiSoft PI Web API for R language
Is a R script to read and write data to OsiSoft PI Data Archive

It's created as a need to easily fetch, clean and join PI data into R DataFrames for further manipulation, analysisi and model building.
Main reference is PI Tag name that is preserved as column name in data frame.

Input could be PI Tag name or array of PI Tag names

Timestamps of PI Tags (UTC) are recalculated to your system time.

Supported read methods are Interpolated, Average, Compressed, Summary

Supported summary  methods are: 
Average,Total,Minimum,Maximum,Range,StdDev,PopulationStdDev,Count,PercentGood

// TODO ....finish this docu...
