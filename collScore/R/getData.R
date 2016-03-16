#' Get data using the web api and specifying a query. You can use the page parameter to specify how much data to get. Each page will return
#' 20 observations.If page = "All", all observations satisfying the specified query will be returned
#'
#' @param endpoint Schools
#' @param apiKey Key required to access data through API
#' @param format Specifies the json format
#' @param fieldParams Fields to be included in output
#' @param optionParams Parameters to optionally include
#' @param apiVersionString Version of the API used
#' @param page Number of pages to return
#' @return data.frame
#' @examples
#' \dontrun{getData(fieldParams = "school.degrees_awarded.predominant=2,3", 
#' optionParams = "_fields=id,school.name,2013.student.size")}
#' \dontrun{getData(fieldParams = "school.degrees_awarded.predominant=2,3", 
#'   optionParams = "_fields=id,school.name,2013.student.size", page = "All")}
#' @export
getData <- function(apiKey,endpoint = "schools", format = "json", fieldParams, optionParams="", apiVersionString = "v1", page = 0){
  ##
  ## Start ilianav code
  ##
  if(missing(apiKey)){
    apiKey <- getAPIKey()
  }
  
  urlPath = "https://api.data.gov/ed/collegescorecard"
  queryUrl <- paste(urlPath, apiVersionString, paste(paste(paste(endpoint, format, sep = "."), fieldParams, sep = "?"), 
                                                     optionParams, sep = "&"), sep = "/")
  #Add apiKey
  queryUrl <- paste(queryUrl, "&api_key=", apiKey, sep = "")
  queryUrl <- gsub("&&", replacement = "&",x = queryUrl)
  
  #Helper function to get pages
  getPages <- function(p = page){
    queryUrl <- paste(queryUrl, "&_page=", p, sep = "")
    res <- httr::GET(queryUrl)
    if (res$status_code==414) {
      stop ("Error code 414: Please request fewer variables")
    }
    else if (res$status_code==429) {
      stop ("Error code 429: Too many requests! You have exceeded your rate limit. 
            Try again later or contact us at https://api.data.gov/contact/ for assistance")
    }
    else {
      res<-res
    }
  }
  ##
  ## End ilianav code
  ##
  
  #Helper function to convert json response to data.frame
  ##
  ## Start cathrynr code
  ##
  toDataFrame <- function(jsonData, parsed = TRUE){
    if (exists("errors",where=jsonData)==TRUE) {
      errorList<-(matrix(unlist(result$errors), nrow=length(unlist(result$errors[1]))))
      stop (paste("\n Your API request has resulted in the following error:",errorList[3,],sep="\n"))
    }
    if(!parsed)
    {
      jsonData <- httr::content(jsonData, as = "parsed")
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
  ##
  ## End cathrynr code
  ##
  
  ##
  ## Start ilianav code
  ##
  #To get a single page
  result <- getPages(0)
  result <- httr::content(result, as = "parsed")
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
  ##
  ## End ilianav code
  ##
  
  DF
}