# Sergej Horvat
# 27.11.2018.
# version 0.2
# Connecting R to PI system via PI WEB API
# Include this source to your project "source("../PiWebApi4R.R")"
##### OsiSoft PI WEB API 4 R ####


# Library dependancy
if (!require("jsonlite")) install.packages("jsonlite")
library("jsonlite")

if (!require("httr")) install.packages("httr")
library("httr")

if (!require("lubridate")) install.packages("lubridate")
library("lubridate")

if (!require("dplyr")) install.packages("dplyr")
library("dplyr")

# Static variables
PIServer <- "yourPIserver"
base_url <- "https://pivision/piwebapi"  # PUT YOUR BASE URL FOR PI WEB API
#startTime <- "10-Sep-2018"
#endTime <- "20-Sep-2018"
#interval <- "10m"
#timeStamp <- "" # now
PercentGood <- 80
piPointName <- NULL
pointWebId <- NULL
body <- NULL
HTTP_verbose <- TRUE


# Enumeration of valid summary types for function that uses summayType
summaryType = factor(x=c("Average",
                         "Total",
                         "Minimum",
                         "Maximum",
                         "Range",
                         "StdDev",
                         "PopulationStdDev",
                         "Count",
                         "PercentGood"
                )
                ,levels=c("Average",
                          "Total",
                          "Minimum",
                          "Maximum",
                          "Range",
                          "StdDev",
                          "PopulationStdDev",
                          "Count",
                          "PercentGood"
                )
)




#### HELPER FUNCTIONS ####

# Validate user input and exit gracefully if summaryType is not supported/enumerated
isValidSummaryType <-function(avrgType){
    isValid=FALSE
    for(j in 1:nlevels(summaryType)){
        
        if(avrgType==levels(summaryType)[j]){
            isValid=TRUE
        }    
        
    }
    return(isValid)
}

# Get PI Point WebId function
getPIPointWebId <- function(piDataArchive, piPointName){
    cat("getPIPointWebId called","\n")
    piPointName <<- piPointName
    path = paste(c("\\\\",piDataArchive,"\\",piPointName))
    compl_url= paste(c("/points?path=" , path), collapse="")
    x = getHttpRequest(compl_url)
    x$WebId
}




# getHttpRequest function implementation
getHttpRequest <- function(compl_url){
    cat("getHttpRequest function call.","\n")
    url <- paste(c(base_url,compl_url), collapse = "")
    w <- GET(url,
             authenticate(user=":",
                          password=":",
                          "gssnegotiate"),
             verbose = HTTP_verbose
    )
    if (w$status_code == 200){
        x <- content(w,"text")
        x <- fromJSON(x)
    }else{
        cat("Error, HTTP response <> 200", "\n")
        return(NULL)
    }
}

# POST Http request for GET method
postHttpRequest <- function(compl_url,body){
    cat("postHttpRequest function call.","\n")
    url <- paste(c(base_url,compl_url), collapse = "")
    postResponse <- POST(url = url,
                         body = body,
                         add_headers("X-Requested-With"="message/http"),
                         encode="json",
                         content_type_json(),
                         authenticate(user=":",
                                      password=":",
                                      "gssnegotiate"),
                         verbose()  # enables debugging message in console only
    )
    # return response from WEB API service
    postResponse
}


# Convert UTC timestamps to local time
convertTimeZone <- function(timeValues){
    # Convert char to POSIX date time format, PI data is in UTC time format
    timeValues$Timestamp <-as.POSIXct(strptime(timeValues$Timestamp, 
                                               format="%Y-%m-%dT%H:%M:%S",
                                               tz="UTC"
    )
    )
    # Get system local timezone
    timezone <- Sys.timezone(location = TRUE)
    # Change timestamps from UTC to Local time zone
    timeValues$Timestamp <- with_tz(timeValues$Timestamp,tz=timezone)
    # Return converted data timestamps
    timeValues
}

# Bind data by point names from point name data frame, 
# point names are preserved in column names
joinData <- function(PIpointsArray){
    cat("joinData called","\n")
    cat("PIpointArray class:", class(PIpointsArray),"\n")
    
    
    # Cast passed value to array so it can be itterated
    #PIpointsArray <- as.data.frame(PIpointsArray)
    
    # Create initial data frame from first element in array to join to.
    pointsDataFrame <- data.frame()
    pointsDataFrame <- eval(parse(text = PIpointsArray[1]))
    
    # Check the first column transposed to cat
    #cat("PointsDataFrame to use for initial joining: ",t(pointsDataFrame[,1]), "\n" )
    
    cat("PiPointArray class: ", class(PIpointsArray), "\n")
    cat("Join data ", PIpointsArray[1], "\n")
    for (df in 2 : length(PIpointsArray)){
        # ///TODO anti_join to see what data is being discarded
        cat("Join data for function:", df, "\n")
        cat("Data Type:", class(pointsDataFrame),"\n")
        if(!exists(PIpointsArray[df])){
            cat("JoinData PI point array loop - object not found: ", PIpointsArray[df]," skipping joining!" )
            break
        }else{
            cat("Joining data", (text=PIpointsArray[df]),"\n")
            pointsDataFrame <- inner_join(pointsDataFrame, eval(parse(text=paste0("`",PIpointsArray[df],"`"))),by="Timestamp")
        }
    }
    return(pointsDataFrame)
}

# Save imported data to .csv
saveDataCSV <- function(PIpointsArray){
    cat("saveData function called","\n")
    for(point in PIpointsArray){
        cat("For point " , point , " in PipointArray write: ", paste0(point,".csv") ,"\n")
        # Use ' for strings with special characters that can invoke an error in tag names
        write.csv2(eval(parse(text=paste0("`",point,"`"))),file = paste0(point,".csv"), append = FALSE, quote = FALSE, row.names = FALSE, col.names = TUE)
        
    }
}

# Load imported data from .csv (file name to object name)
loadDataCSV <- function(PIpointsArray){
    cat("loadData function called","\n")
    for(point in 1:length(PIpointsArray)){
        tagFile <- paste(c(getwd(),"/",PIpointsArray[point],".csv"),collapse = "")
        cat("current working directory and file: " ,tagFile,"\n")
        if(file.exists(tagFile)){
            cat("File exists!!!!","\n","eval point:",PIpointsArray[point],"\n")
            # Create R object with point name from tagFile csv in Global Environment
            do.call("<<-",list(PIpointsArray[point],
                               read.csv2(file = tagFile,
                                         header = TRUE, 
                                         stringsAsFactors = FALSE,
                                         colClasses = c("Timestamp"="POSIXct",
                                                        "numeric"))))

        }else{
            cat("File does not exists!!!!","\n")
        }
        
    }
}

#### CURRENT VALUE ####
# NOT IMPLEMENTED

# Get points value for time  # NOT IMPLEMENTED
getCurrentValue <- function(pointWebId,dateTime){
    cat("getSingleValue called","\n")
    campl_url <- paste(c("/streams/",as.character(pointWebId),
                         "/value?selectedFields=Timestamp;Value"
                         ), collapse ="")
    cat(compl_url,"\n")
    x = getHttpRequest(compl_url)
    
    # // TODO create cleaning and collapse functions
}




##### INTERPOLATED #####

# Get interpolated values for WebId # NOT USED AS STANDALONE FUNCTION
getInterpolatedValues <- function(startTime, endTime, interval, WebId){
    cat("getInterpolatedValues called","\n")
    compl_url <- paste(c("/streamsets/interpolated?starttime=",as.character(startTime),"&endtime=",as.character(endTime),"&interval=", as.character(interval),"&webId=", WebId), collapse ="")
    cat(compl_url,"\n")
    x = getHttpRequest(compl_url)
    # //TODO check what has getHTTPrequest function has returned - if null 
}


# Get interpolated values from PI TAG, collapse and set timezone function
getInterpolated <- function(piDataArchive=PIServer, piPointName, StartTime=startTime, EndTime=endTime, interval=interval){
    cat("GetInterpolated return pointWebID value: ", pointWebId,"\n")
    if(is.null(pointWebId)) {
        cat("Breaking GetInterpolated function no web id for PI point!\n")
    }else{
        
        interpolatedValues <- getInterpolatedValues(StartTime,EndTime,interval,pointWebId)
        interpolatedValues <- collapseInterpolatedValues(interpolatedValues)
        interpolatedValues <- convertTimeZone(interpolatedValues)
    }
    # //TODO check request complete duration
    # //TODO check if all values are fetched, is there some values missing or removed by collapse function - implement in collapse function
    # //TODO Create fetch data summary
    #Return values:
    interpolatedValues
}

# End user callable function
getInterpolatedPointsValues <- function(PIpointsArray,StartTime=as.character(startTime),EndTime=as.character(endTime),Interval=as.character(interval)){
    # Timestamp and values for each point
    PIpointsArray <- as.array(PIpointsArray)
    #cat(PIpointsArray,"\n")
    cat("getInterpolatedPointsVealues passed parameter list is class: ", class(PIpointsArray),"\n")
    
    # Loop in Point array for every point get value and assign 
    # global variable and name in by PI point name 
    # so it can be acces by model coeficient names
    for(point in PIpointsArray){
        pointWebId <<-getPIPointWebId(piDataArchive=PIServer, point)
        cat("Point from array: ",point, "\n","PointWebId: ",pointWebId,"\n")
        if(is.null(pointWebId)){
            cat("No valid PI Poit Web Id - skipping point")
        }else{
            pointValue <- getInterpolated(PIServer,point,StartTime,EndTime,Interval)
            # Execute a function call (<<- is a operator!(parent frame)) for every point name in list create variable and assign values from pointValue
            do.call("<<-",list(point,pointValue))
        }
    }
}

# Collapse interpolated valus from original structure and extract PI name into Value column
collapseInterpolatedValues <- function(timeValues){
    timeValues <- as.data.frame(timeValues$Items$Items)
    # Check for "Bad" , "No Good Data For Calculation" in columns: Good = TRUE & Questionable=FALSE  
    timeValues <- timeValues[which((timeValues$Good==TRUE)&(timeValues$Questionable==FALSE)),]
    # Get name from parent environment to name Value column by PiPointName
    colnames(timeValues)[2] <- piPointName
    # Convert from Json values from character to double variable type
    timeValues[,piPointName] <- as.double(timeValues[,piPointName])
    #Return values:
    timeValues[,1:2]
}




##### AVERAGE #####

# Get average values filtered from WebId # BATCH GET ALL! # DEPRICIATED # DELETE
getAverageValues <- function(startTime, endTime, interval, WebId, summaryType="Average",PercentGood=PercentGood){
    cat("getAverageValues function call.","\n")
    timezone <- Sys.timezone(location = TRUE)
    compl_url <- paste(c("/streams/", WebId,"/summary?startTime=", startTime,"&endtime=",endTime,"&summaryDuration=", interval, "&summaryType=",summaryType,"&timeZone=",timezone), collapse ="")
    cat(compl_url,"\n")
    x = getHttpRequest(compl_url)
    # //TODO check what has getHTTPrequest function has returned - if null 
}

# Get average values from PI TAG, collapse and set timezone function
getAverage <- function(piDataArchive, piPointName, StartTime=startTime, EndTime=endTime, interval=interval){
    cat("GetAverage return pointWebID value: ", pointWebId,"\n")
    if(is.null(pointWebId)) {
        cat("Breaking getAverage function no web id for PI point!\n")
    }else{
        averageValues <- getAverageValues(StartTime,EndTime,interval,pointWebId)
        averageValues <- collapseAverageValues(averageValues)
        averageValues <- convertTimeZone(averageValues)
        cat("Point: ", piPointName," class: ", class(averageValues), "\n")
    }
    # //TODO check request complete duration
    # //TODO check if all values are fetched, is there some values missing or removed by collapse function - implement in collapse function
    # //TODO Create fetch data summary
    #Return values:
    averageValues
}

# Collapse averaged valus from original structure and extract PI name into Value column
collapseAverageValues <- function(timeValues){
    timeValues <- as.data.frame(timeValues$Items$Value)
    # Check for "Bad" , "No Good Data For Calculation" in columns: Good = TRUE & Questionable=FALSE  
    timeValues <- timeValues[which((timeValues$Good==TRUE)&(timeValues$Questionable==FALSE)),]
    # Get name from parent environment to name Value column by PiPointName
    colnames(timeValues)[2] <- piPointName
    # Convert from Json values from character to double variable type
    timeValues[,piPointName] <- as.double(timeValues[,piPointName])
    #Return values:
    timeValues[,1:2]
    
}



# Get PiPoint array values
# 1 st function in call
# //TODO add calculation type
getAveragePointsValues <- function(PIpointsArray,StartTime=startTime,EndTime=endTime,Interval=interval){
    # Timestamp and values for each point
    PIpointsArray <- as.array(PIpointsArray)
    #cat(PIpointsArray,"\n")
    cat("getAveragePointsValues passed parameter list is class: ", class(PIpointsArray),"\n")
    
    # Loop in Point array for every point get value and assign 
    # global variable and name in by PI point name 
    # so it can be acces by model coeficient names
    for(point in PIpointsArray){
        pointWebId <<-getPIPointWebId(piDataArchive=PIServer, point)
        cat("Point from array: ",point, "\n","PointWebId: ",pointWebId,"\n")
        if(is.null(pointWebId)){
            cat("No valid PI Poit Web Id - skipping point")
        }else{
            pointValue <- getAverage(PIServer,point,StartTime,EndTime,Interval)
            # Execute a function call (<<- is a operator!(parent frame)) for every point name in list create variable and assign values from pointValue
            do.call("<<-",list(point,pointValue))
        }
    }
}




#### BATCH COMPRESSED ####

batchPIgetCompressed <- function(PIpointsArray, StartTime=startTime, EndTime=endTime, boundaryType="inside",maxCount=2000){
    cat("getRecorded batchPIgetcompressed!" , "\n")
    
    # Empty list for request body conectent in Json
    body <- list()
    
    # Create body request from all points in PIpointArray - so all of them will have same timestamp for relative PI time!
    for (point in PIpointsArray) {
        cat("batchPIgetCompressed points in Array: " , point , "\n" )
        
        # For get request every PI point webID is needed
        pointWebId <<-getPIPointWebId(piDataArchive=PIServer, point)
        body[[as.character(point)]] <- list("Method"="GET",
                                            "Resource"=paste(c(base_url,
                                                               "/streams/",pointWebId,
                                                               "/recorded?starttime=",StartTime,
                                                               "&endtime=",EndTime,
                                                               "&boundaryType=",boundaryType,
                                                               "&maxCount=",maxCount
                                            ),
                                            collapse =""
                                            )
        )
    }
    
    # Check JSON request body in pretty way ;)
    cat("Return body:","\n",
        toJSON(body, pretty = TRUE),
        "\n"
    )
    
    # Batch url:
    batch_url <- c("/batch/")
    
    # Send POST request with created body list
    batch_response <<- postHttpRequest(batch_url,body)
    
    # Status: 207: A dictionary of response information corresponding to the batched requests.
    if (batch_response$status_code == 207){
        cat("Batch request was sucessfull status code: 207","\n")
        batch_response_content <- content(batch_response, type="text",encoding = "UTF-8")
        batch_response_content <- fromJSON(batch_response_content,flatten = TRUE)
        #batch_response_content <<- fromJSON(batch_response_content,flatten = TRUE)
        collapseBatchCompressedValues(batch_response_content,PIpointsArray)
    }
    
}

# Collapse returned values from batch compressed request
collapseBatchCompressedValues <- function(batch_response_content,PIpointsArray){
    cat("collapseBatchCompressedValues called!", "\n")
    for (point in PIpointsArray){
        cat("Point: ", point , "\n")
        
        # Unlist content from response to Items for every PItag
        pointValue <- batch_response_content[[point]][["Content"]][["Items"]]
        
        # Check for "Bad" , "No Good Data For Calculation" in columns: Good = TRUE & Questionable=FALSE 
        pointValue <- pointValue[which(pointValue$Good==TRUE & pointValue$Questionable==FALSE),]
        
        # Collapse table only to timestamp and value columns
        pointValue <- pointValue[,1:2]
        
        # Give common col names for Timestampo and PI tag columns, for easy joining of data tables
        colnames(pointValue) <- c("Timestamp",paste(toString(point),"compressed",sep="."))
        
        # Convert Time Zone of Timestamp colum
        pointValue <- convertTimeZone(pointValue)
        
        # Create R variable by point name with pointValues from previous step
        do.call("<<-",list(point,pointValue))
        
    }
}




#### BATCH SUMMARY ####

# BATCH POST request, sends multiple requests (It will be used only for GET request!)
batchPIgetValues <- function(PIpointsArray, StartTime=startTime, EndTime=endTime, Interval=interval,summaryType="Average" ,PercentGood=80){
    cat("batchPIgetValues called", "\n")
    
    # Validate summary type
    if(isValidSummaryType(summaryType)){
        cat("Good average type","\n")
    }else{
        cat("Not Good summary type! aborting", "\n")
        # break all batchPIgetValues function
        break
        #("Not Good summary type!")
    }
    
    # Create request URL to be concatinated on base url on POST call 
    compl_url <- c("/batch")
    
    # Empty list for request body conectent in Json
    body <- list()
    
    # Create body request from all points in PIpointArray - so all of them will have same timestamp for relative PI time!
    for (point in PIpointsArray) {
        cat("batchPIgetAverageValues points in Array: " , point , "\n" )
        
        # For get request every PI point webID is needed
        pointWebId <<-getPIPointWebId(piDataArchive=PIServer, point)
        body[[as.character(point)]] <- list("Method"="GET",
                                            "Resource"=paste(c(base_url,
                                                               "/streams/",pointWebId,
                                                               "/summary?startTime=",StartTime,
                                                               "&endtime=",EndTime,
                                                               "&summaryDuration=",Interval,
                                                               "&summaryType=",summaryType
                                            ),
                                            collapse =""
                                            )
        )
    }
    
    # Check JSON request body in pretty way ;)
    cat("Return body:","\n",
        toJSON(body, pretty = TRUE),
        "\n"
    )
    
    # Batch url:
    batch_url <- c("/batch/")
    
    # Send POST request with created body list
    batch_response <<- postHttpRequest(batch_url,body)
    
    # Status: 207: A dictionary of response information corresponding to the batched requests.
    if (batch_response$status_code == 207){
        cat("Batch request was sucessfull status code: 207","\n")
        batch_response_content <- content(batch_response, "text",encoding = "UTF-8")
        batch_response_content <- fromJSON(txt = batch_response_content,flatten = TRUE)

        # Uncomment line below to set response as global variable to check data
        #batch_response_content_global <<- fromJSON(batch_response_content,flatten = TRUE)
        
        collapseBatchValues(batch_response_content,PIpointsArray,summaryType,Interval)
    }else{
        cat("Batch request ERROR status code: ",batch_response$status_code ,"\n")
    }
}


# Collapse returned values from batch request
collapseBatchValues <- function(batch_response_content, PIpointsArray, summaryType,Interval=""){
    cat("collapseBatchValues called!", "\n")
    for (point in PIpointsArray){
        cat("Point: ", point , "\n")
        
        # Unlist content from response to Items for every PItag
        pointValue <- batch_response_content[[point]][["Content"]][["Items"]]
        
        # Check for "Bad" , "No Good Data For Calculation" in columns: Good = TRUE & Questionable=FALSE 
        pointValue <- pointValue[which(pointValue$Value.Good==TRUE & pointValue$Value.Questionable==FALSE),]
        
        # Collapse table only to timestamp and value columns
        pointValue <- pointValue[,2:3]
        pointValue
        # Give common col names for Timestampo and PI tag columns, for easy joining of data tables
        colnames(pointValue) <- c("Timestamp",paste(toString(point),toString(summaryType),toString(Interval),sep="."))
        
        # Convert Time Zone of Timestamp colum
        pointValue <- convertTimeZone(pointValue)
        
        # Test see structure
        str(pointValue)
        
        # Convert data values from character to float
        options(digits = 10)
        pointValue[,2] <- as.numeric(pointValue[,2])
        pointValue[,2] <- round(pointValue[,2], digits = 5)
        
        # Create R variable by point name with pointValues from previous step
        do.call("<<-",list(point,pointValue))
        
    }
}




#### WRITE TO PI ####

# POST data to PI point by piPointWebId
postPIpointValue <- function(point, piPointValue, timeStamp=""){
    cat("postPIpointValue called", "\n")
    
    # Get PI Point Web ID
    pointWebId <<-getPIPointWebId(piDataArchive=PIServer, point)
    cat("postPIpointValue called getPIPointWebId with result: ",pointWebId, "\n")
    cat("postPIpointValue point: ", point, "\tvalue: ",piPointValue, "\n")
    if(!is.null(pointWebId) && !is.null(piPointValue)){
        # Create compl_url and body for POST request
        compl_url <- paste(c("/streams/",pointWebId,"/value"),collapse = "")
        cat("compl_url: ",compl_url, "\n")
        body <-list(
            "Timestamp"=timeStamp, # check for timestap proper format! Cast if needed!
            "Value"=piPointValue
        )
        
        # send compl_url and body to POST http function and expect response
        postResponse <- postHttpRequest(compl_url,body)
        
        if(postResponse$status_code == 202){
            cat("postPIpointValue status code 202: The value was accepted for a potential buffered update.","\n")
        }
        else if(postResponse$status_code == 204){
            cat("postPIpointValue status code 204: The value was accepted for a potential buffered update.","\n")
        }
        else if(postResponse$status_code == 400){
            cat("postPIpointValue status code 400: The request was malformed.","\n")
        }
        else if(postResponse$status_code == 409){
            cat("postPIpointValue status code 409: The attribute or data reference does not support this operation, or the specified units are incompatible.","\n")
        }else{
            cat("postPIpointValue status code error: ",postResponse$status_code,"\n")
        }
        
    }else{
        cat("PIpoint or PIpointValue are invalid. Error posting to PI point.")
    }
}

# //TODO Post function to multiple values for same point 
# https://pivision/piwebapi/help/controllers/stream/actions/updatevalues
