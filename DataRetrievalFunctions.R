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
GetData <- function(apiKey,endpoint = "schools", format = "json", fieldParams, optionParams="", apiVersionString = "v1", page = 0){
  urlPath = "https://api.data.gov/ed/collegescorecard"
  queryUrl <- paste(urlPath, apiVersionString, paste(paste(paste(endpoint, format, sep = "."), fieldParams, sep = "?"), 
                                                     optionParams, sep = "&"), sep = "/")
  #Add apiKey
  queryUrl <- paste(queryUrl, "&api_key=", apiKey, sep = "")
  
  #Helper function to get pages
  getPages <- function(p = page){
    queryUrl <- paste(queryUrl, "&_page=", p, sep = "")
    res <- GET(queryUrl)
    if (res$status_code==414) {
      stop ("Error code 414: Please request fewer variables")
    } else
    {res<-res}
  }
  
  #Helper function to convert json response to data.frame
  toDataFrame <- function(jsonData, parsed = TRUE){
    if (exists("errors",where=jsonData)==TRUE) {
      errorList<-(matrix(unlist(result$errors), nrow=length(unlist(result$errors[1]))))
      stop (paste("\n Your API request has resulted in the following error:",errorList[3,],sep="\n"))
    }
    if(!parsed)
    {
      jsonData <- content(jsonData, as = "parsed")
    }
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

GetAllDataInCategory <- function(apiKey,categoryName, year){
  #Make the path to the data dicitonary be global var or environment var. 
  if (!(categoryName %in% c("academics","admissions","aid","completion","cost","earnings","repayment","root",
                            "school","student"))) {
    stop ("Incorrect categoryName. Please choose from the following: 'academics','admissions','aid','completion,'cost',earnings','repayment','root','school', or 'student'. Consult data dictionary for further detail.")
  }
  if (year<1996 | year>2013) {
    stop("Incorrect year selection. Data is available for 1996 through 2013.")
  }
  dataDict <- read.csv("https://raw.githubusercontent.com/katerabinowitz/College-Scorecard-R-Pkg/master/data/CollegeScorecardDataDictionary-09-08-2015.csv",
                       stringsAsFactors=FALSE)
  categoryVars <- subset(dataDict, dev.category==categoryName, developer.friendly.name)
  categoryVars <- categoryVars[categoryVars != ""]
  if (categoryName=="root") {
  queryList <- paste("fields=", paste(lapply(categoryVars, 
                                             function(x) paste(x, sep = "")), collapse = ","), sep = "")
  }
  else if (categoryName=="school") {
    queryList <- paste("fields=id,", paste(lapply(categoryVars, 
                                               function(x) paste(categoryName, ".", x, sep = "")), collapse = ","), sep = "")  
  }
  else {
    queryList <- paste("fields=id,school.name,", paste(lapply(categoryVars, 
                                               function(x) paste(year, ".", categoryName, ".", x, sep = "")), collapse = ","), sep = "")
  }
  DFcat <- GetData(apiKey=apiKey,fieldParams = queryList)
  DFcat
}

### To add list:
# 1. Having trouble adding latitude and longitude. It is in the root so should not require dev.category
# or year, but API returns field not found for all iterations I've tried
# 2. The Academics and Completion sections have more parameters than the API allows for
# With Academics it can split 2 or 3 queries and then rbind, but I'm not sure what to do with
# completion, which has over 1000 variables (!) 