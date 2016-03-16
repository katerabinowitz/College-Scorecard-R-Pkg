#' Before calling this function an apiKey needs to be obtained from https://api.data.gov/signup"
#' This method saves the apiKey to a file that later will be used in APIs requiring the key to access the data. To update the key,
#' just call the same method again with a new key value 
#'
#' @param apiKey Key obtained from https://api.data.gov/signup
#' @examples
#' \dontrun{setAPIKey("some key")}
#' @export
#' 
##
## Start ilianav code
##
setAPIKey <- function(apiKey) 
{
  save(apiKey, file = paste(system.file("extdata", package = "collScore"), "key.rda", sep = "/"))
}
##
## End ilianav code
##