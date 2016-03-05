library(httr)

#LIMITATION - does not work when NULLs are present
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
  dataDict <- read.csv("C:/Research/Data Analytics/Courses/Stanford/Data Mining and Appliations/Paradigms for Computing with Data/Final Project/CollegeScorecard_Raw_Data/CollegeScorecard_Raw_Data/CollegeScorecardDataDictionary-09-12-2015.csv",
                       stringsAsFactors=FALSE)
  categoryVars <- subset(dataDict, dev.category==categoryName, developer.friendly.name)
  categoryVars <- categoryVars[categoryVars != ""]
}


#UNDER CONSTRUCTION
#Currently this function returns the list of variables in the specified category with the year and category concatinated
#in the format <year>.<category>.<variable name> that can be passed as parameters to GetData. The GetData method doesn't
#work because NULL is not being converted to NA
#Another limitation to this method is that it works only for one year.
GetAllDataInCategory <- function(categoryName, year){
  #Make the path to the data dicitonary be global var or environment var. 
  dataDict <- read.csv("C:/Research/Data Analytics/Courses/Stanford/Data Mining and Appliations/Paradigms for Computing with Data/Final Project/CollegeScorecard_Raw_Data/CollegeScorecard_Raw_Data/CollegeScorecardDataDictionary-09-12-2015.csv",
                       stringsAsFactors=FALSE)
  categoryVars <- subset(dataDict, dev.category==categoryName, developer.friendly.name)
  categoryVars <- categoryVars[categoryVars != ""]
  queryList <- paste("fields=", paste(lapply(categoryVars, function(x) paste(year, ".", categoryName, ".", x, sep = "")), collapse = ","), sep = "")
  
  #tv <- GetData(fieldParams = queryList)
  
}