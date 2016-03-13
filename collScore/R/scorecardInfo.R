#' Returns the variable name, definitions, source and any notes from the College Scorecard data dictionary
#'
#' @param variable Provide the variable(s) to return their definition and characteristics
#' @examples
#' scorecardInfo(c("CITY","INSTNM","UNITID"))
#' @export
##
## Start cathrynr code
##
scorecardInfo<-function(variable) {
  data(dataDict,  envir = environment())
  varDF<-as.data.frame(variable)
  colnames(varDF)<-"variable"
  varSeek<-merge(x = varDF, y = dataDict[ , c("VARIABLE.NAME","NAME.OF.DATA.ELEMENT", "SOURCE","NOTES")], 
            by.x="variable", by.y = "VARIABLE.NAME", all.x=TRUE)
  return(varSeek)
}
##
## End cathrynr code
##