#' Before calling this function an apiKey needs to be obtained from https://api.data.gov/signup"
#' and set using setAPIKey
#' This method loads the saved apiKey from key.rda 
#'
#' @return the saved key
#' @export
getAPIKey <- function() 
{
  if (file_test("-f", system.file("extdata/key.rda", package = "collScore"))) {
    key <- load(system.file("extdata/key.rda", package = "collScore"))
  }
  else{
    stop("An API Key is required to access the CollegeScoreCard API. 
         You may obtain a key from https://api.data.gov/signup")
  }
  get(key)
}