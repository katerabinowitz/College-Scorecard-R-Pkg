library(httr)


#https://api.data.gov/ed/collegescorecard/v1/schools.json?school.degrees_awarded.predominant=2,3&_fields=id,school.name,2013.student.size
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
  apiKey <- "NSLINN8oOZPKvLToNsyR4pbYKD3BeW8PgcxBK0m3"
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