#' Returns information about a given school
#'
#' @param schoolName Name of school
#' @examples
#' data(scorecard13)
#' getInfoAboutSchool("University of Massachusetts-Lowell")
#' @export
getInfoAboutSchool <- function(schoolName, dataset){
  data(dataDict,  envir = environment())
  val <- scorecard13$CONTROL[scorecard13$INSTNM == schoolName]
  type <- varName <- dataDict$LABEL[dataDict$VARIABLE.NAME == "CONTROL" & dataDict$VALUE == val]
  type
}