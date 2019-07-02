# PIwebAPI4R

OsiSoft PI Web API for R language
Is a R script to read and write data to OsiSoft PI Data Archive

It's created as a need to easily fetch, clean and join PI data into R DataFrames for further manipulation, analysis and model building.
Main reference is PI Tag name that is preserved as column name in data frame.

Input could be PI Tag name or array of PI Tag names

Timestamps of PI Tags (UTC) are recalculated to your system time.

Supported read methods:
- Interpolated 
- Compressed  
- Summary

Supported write methods:
- write single PI tag value with timestamp

Supported summary  methods: 
- Average 
- Total
- Minimum 
- Maximum
- Range 
- StdDev 
- PopulationStdDev
- Count
- PercentGood

Helper Functions:
- Join data (by timestamp) defined by array of PI Tags
- Save data frames to CSV in bulk defined by array of PI Tags
- Load data frames to CSV in bulk defined by array of PI Tags


Needed packages:
- jsonlite
- httr
- lubridate
- dplyr

## Example of usage
#### Define statics
PIServer <- "yourPIserver"  
base_url <- "https://pivision/piwebapi"  # PUT YOUR BASE URL FOR PI WEB API   
startTime="1-Apr-2018"  
endTime="1-Feb-2019"  
interval="5m"  

#### Get average values for Pi Point array in a batch. 
It returns DataFrames named as PI Tag Names in PITagArray.
```R
batchPIgetValues(PITagArray, StartTime = startTime, EndTime = endTime, Interval = interval, summaryType = "Average")
```

#### Get archive raw data (compressed data). 
It returns DataFrames named as PI Tag Names in PITagArray:
```R
batchPIgetCompressed(PIpointsArray, StartTime = startTime, EndTime = endTime, maxCount = 2000)
```

#### Join data by timestamp to data frame:
```R
JoinedDataFrame <- joinData(PIpointsArray)
```

#### Save data to individual .csv from PIpointsArray:
```R
saveDataCSV(PIpointsArray)
```

SetUp WebAPI max return values by request, default value can be overriden in configuration

// TODO ....finish this docu...

Feel free to contribute ;)
