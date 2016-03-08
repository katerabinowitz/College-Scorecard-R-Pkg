#' Returns the variable name, definitions, source and any notes from the College Scorecard data dictionary
#'
#' @param variable
#' @examples
#' scorecardInfo(c("CITY","INSTNM","UNITID"))
#' 
scorecardInfo<-function(variable) {
  dataDict<-read.csv("https://raw.githubusercontent.com/katerabinowitz/College-Scorecard-R-Pkg/master/data/CollegeScorecardDataDictionary-09-08-2015.csv",
                     stringsAsFactors=FALSE)
  varDF<-as.data.frame(variable)
  colnames(varDF)<-"variable"
  varSeek<-merge(x = varDF, y = dataDict[ , c("VARIABLE.NAME","NAME.OF.DATA.ELEMENT", "SOURCE","NOTES")], 
            by.x="variable", by.y = "VARIABLE.NAME", all.x=TRUE)
  return(varSeek)
}


