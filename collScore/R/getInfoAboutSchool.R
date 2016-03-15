#' Returns information about a given school
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schoolName Name of school
#' @examples
#' data(scorecard13)
#' getInfoAboutSchool(,scorecard13, schoolName = 
#' c("University of Massachusetts-Lowell", "Stanford University"))
#' @export
getInfoAboutSchool <- function(apiKey, dataset, schoolName) {
  
  data(dataDict,  envir = environment())
  
  val <- scorecard13$CONTROL[scorecard13$INSTNM == schoolName]
  type <- varName <- dataDict$LABEL[dataDict$VARIABLE.NAME == "CONTROL" & dataDict$VALUE == val]
  type
}