#' Returns the VARIABLE.NAME corresponding to the developer.friendly.name. When using the REST API, only the developer.friendly.name works when 
#' querying the data. However, when using the data included in the package, only the VARIABLE.NAME is available in the dataset. This
#' functions provides an easy way to get the developer.friendly.name when only the VARIABLE.NAME is known.
#'
#' @param devFriendlyName developer.friendly.name as per data dictionary
#' @return character
#' @examples
#' convertDevNameToVarName("8_yr_completion.low_income")
#' @export
convertDevNameToVarName <- function(devFriendlyName){  
  data(dataDict,  envir = environment())
  varName <- dataDict$VARIABLE.NAME[dataDict$developer.friendly.name == devFriendlyName]
  varName
} 