library(httr)

#Temporary
#GLOBAL PATH TO DICTIONARY 
#PATH_DICT <- "https://raw.githubusercontent.com/katerabinowitz/College-Scorecard-R-Pkg/master/data/CollegeScorecardDataDictionary-09-08-2015.csv"
PATH_DICT <- "C:/Research/Data Analytics/Courses/Stanford/Data Mining and Appliations/Paradigms for Computing with Data/Final Project/CollegeScorecard_Raw_Data/CollegeScorecard_Raw_Data/CollegeScorecardDataDictionary-09-12-2015.csv"

#' Saves the key to a file that later will be used in APIs that require the key to access the data. To update the key,
#' just call the same method again with a new key value 
#'
#' @param apiKey
#' @examples
#' SetKey("some key")
SetAPIKey <- function(apiKey) 
{
  #TEST - will save the key to a file named key.rda in the current directory. This works as long as this method is called from 
  #the same directory as the GetData method or other methods that require a key. Once we have the package structure, it will be 
  #easy to save the key to a known location
  save(apiKey, file = "key.rda")
  
  #To use when we have the package structure in place
  #save(key, file = paste(system.file("<directory_in_package_to_keep_key>", package = "<our_package_name>"), file, sep = "/"))
}

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
#' GetData(fieldParams = "school.degrees_awarded.predominant=2,3", optionParams = "_fields=id,school.name,2013.student.size")
#' GetData(fieldParams = "school.degrees_awarded.predominant=2,3", optionParams = "_fields=id,school.name,2013.student.size", page = "All")
#' @export
GetData <- function(apiKey,endpoint = "schools", format = "json", fieldParams, optionParams="", apiVersionString = "v1", page = 0){
  
  if(missing(apiKey)){
    #If statement below will work once we have the package structure in place
    #if (file_test("-f", system.file("<directory_in_package_to_keep_key>", package = "<our_package_name>"))) {
    #  load(system.file("<directory_in_package_to_keep_key>", package = "<our_package_name>"))
    #}
    
    #Temporary until we get the package structure in place
    if (file_test("-f", "key.rda")) {
      load("key.rda")
    }
    else{
      stop("An API Key is required to access the CollegeScoreCard API. You may obtain a key from https://api.data.gov/signup")
    }
  }
  
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

#' Get all data from a specified category. The available categories are 
#' root, school, academics, admissions, student, cost, aid, repayment, completion, earnings  
#'
#' @param apiKey
#' @param catgoryName
#' @param year
#' @return character vector
#' @examples
#' costData <- GetAllDataInCategory(categoryName = "cost", year = 2013)
#' schoolData <- GetAllDataInCategory(categoryName = "school")
#' earningsData <- GetAllDataInCategory(categoryName = "earnings", year = c(2010, 2013))
#' earningsData <- GetAllDataInCategory(categoryName = "earnings", year = c(2010, 2013), pattern = "6_yrs_after_entry.mean", addParams = "school.state")
#' @export
GetAllDataInCategory <- function(apiKey,categoryName, year, pattern = "", addParams = "id,school.name"){
  isYearValid <- function(value){
    isValid <- all(unlist(lapply(value, function(x) !(x<1996 | x>2013))))
    isValid
  }
   
  if (!(categoryName %in% c("academics","admissions","aid","completion","cost","earnings","repayment","root",
                            "school","student"))) {
    stop ("Incorrect categoryName. Please choose from the following: 'academics','admissions','aid','completion,'cost',earnings','repayment','root','school', or 'student'. Consult data dictionary for further detail.")
  }
  
  if(!missing(year) && (categoryName=="root" || categoryName=="school")){
    if (!isYearValid(year)) {
      stop("Incorrect year selection. Data is available for 1996 through 2013.")
      }
  }
  
  dataDict <- read.csv(PATH_DICT, stringsAsFactors=FALSE)

  categoryVars <- subset(dataDict, dev.category==categoryName, developer.friendly.name)

  categoryVars <- categoryVars[categoryVars != ""]
  categoryVars <- grep(pattern, categoryVars, value = TRUE)
  if (categoryName=="root") {

  queryList <- paste("fields=", paste(lapply(categoryVars, 

                                             function(x) paste(x, sep = "")), collapse = ","), sep = "")

  }

  else if (categoryName=="school") {

    queryList <- paste("fields=id,", paste(lapply(categoryVars, 

                                               function(x) paste(categoryName, ".", x, sep = "")), collapse = ","), sep = "")  

  }

  else {
      queryList <- paste("fields=", addParams, ",", paste(lapply(categoryVars, 
                                                                 function(x) paste(lapply(year, function(x) paste(x, ".", categoryName, sep = "")), ".", x, sep = "", collapse = ",")), collapse = ","), sep = "")
      
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
# 3. Make the path to the data dicitonary be a global variable in the package