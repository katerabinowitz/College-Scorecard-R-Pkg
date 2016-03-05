library(httr)

#' Get data using the web api and specifying a query. You can use the page parameter to specify how much data to get. Each page will return
#' 20 observations.If page = "All", all observations satisfying the specified query will be returned
#'
#' @param endpoint
#' @param format
#' @param fieldParams
#' @param optionParams
#' @param apiVersionString
#' @param page
#' @return data.frame
#' @examples
#' GetData("school.degrees_awarded.predominant=2,3", "_fields=id,school.name,2013.student.size")
#' GetData("school.degrees_awarded.predominant=2,3", "_fields=id,school.name,2013.student.size", page = "All")
#' @export
GetData <- function(endpoint = "schools", format = "json", fieldParams, optionParams="", apiVersionString = "v1", page = 0){
  apiKey <- "Insert_valid_api_key"
  urlPath = "https://api.data.gov/ed/collegescorecard"
  queryUrl <- paste(urlPath, apiVersionString, paste(paste(paste(endpoint, format, sep = "."), fieldParams, sep = "?"), 
                                                     optionParams, sep = "&"), sep = "/")
  #Add apiKey
  queryUrl <- paste(queryUrl, "&api_key=", apiKey, sep = "")
  
  #Helper function to get pages
  getPages <- function(p = page){
    queryUrl <- paste(queryUrl, "&_page=", p, sep = "")
    res <- GET(queryUrl)
  }
  
  #Helper function to convert json response to data.frame
  toDataFrame <- function(jsonData, parsed = TRUE){
    if(!parsed)
    {
      jsonData <- content(jsonData, as = "parsed")
    }
    #this for loop corrects for the following:
    # 1. replaces all NULLS with NAs so the unlist does not delete them
    # 2. alphabetizes the lists because they were not all in the same order 
    # which created misplaced data in some of the data frame columns
    for(i in 1:(length(jsonData$results)))
    {
      jsonData$results[[i]][sapply(jsonData$results[[i]], is.null)] <- NA
      jsonData$results[[i]]<-jsonData$results[[i]][order(names(jsonData$results[[i]]))]
    }
    DF  <-  as.data.frame(t(matrix(unlist(jsonData$results), nrow=length(unlist(jsonData$results[1])))))
    colnames(DF)<-names(jsonData$results[[1]])
    DF
  }
  
  #To get a single page
  result <- getPages(0)
  result <- content(result, as = "parsed")
  DF <- toDataFrame(result)
  
  #To get all pages
  if(page == "All")
  {
    recordsLeft <- result$metadata$total - 20
    pages <- seq(ceiling(recordsLeft /20))
    result <- lapply(pages, getPages)
    temp <- lapply(result, toDataFrame, parsed = FALSE)
    DF <- do.call(rbind, temp)
  }
  
  DF
}

#' Get a list of all the developer friendly names for variables in a specified category. The available categories are 
#' root, school, academics, admissions, student, cost, aid, repayment, completion, earnings  
#' This function uses the data dictionry to retrieve this information
#'
#' @param catgoryName
#' @return character vector
#' @examples
#' schoolVars <- GetVariableNamesForCategory("school")
#' @export
GetVariableNamesForCategory <- function(categoryName){
  #Make the path to the data dicitonary be global var or environment var. 
  dataDict <- read.csv("https://raw.githubusercontent.com/katerabinowitz/College-Scorecard-R-Pkg/master/data/CollegeScorecardDataDictionary-09-08-2015.csv",
                       stringsAsFactors=FALSE)
  categoryVars <- subset(dataDict, dev.category==categoryName, developer.friendly.name)
  categoryVars <- categoryVars[categoryVars != ""]
}
### CR Question: is the GetVariableNamesForCategory function necessary? I may be missing something but it looks like
### a shorter duplication of GetAllDataInCategory


#UNDER CONSTRUCTION
#Another limitation to this method is that it works only for one year 
# CR note: I think the single year is okay, we have to work within the limitations of the API
GetAllDataInCategory <- function(categoryName, year){
  #Make the path to the data dicitonary be global var or environment var. 
  dataDict <- read.csv("https://raw.githubusercontent.com/katerabinowitz/College-Scorecard-R-Pkg/master/data/CollegeScorecardDataDictionary-09-08-2015.csv",
                       stringsAsFactors=FALSE)
  categoryVars <- subset(dataDict, dev.category==categoryName, developer.friendly.name)
  categoryVars <- categoryVars[categoryVars != ""]
  queryList <- paste("fields=", paste(lapply(categoryVars, function(x) paste(year, ".", categoryName, ".", x, sep = "")), collapse = ","), sep = "")
  
  #tv <- GetData(fieldParams = queryList)
  
}

### To add list:
# 1. the school category of variables do not take year in the API, and the ID variables do not take year or category
# so add and if then else to the queryList building

# 2. if the API fails return API fail message to user