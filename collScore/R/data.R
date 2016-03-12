#' colleges
#'
#' Dataframe of all the colleges included in the College Scorecard, along with attributes such as geography,
#' religious affliation, predominant degree awarded, and more. Users are encouraged to use this dataset for
#' producing vectors of schools for use in other functions. 
#'
#' @name colleges
#'
#' @docType data
#'
#' @format A dataframe of 7,804 observations and 19 variables
#'
#' @keywords datasets
#'
#' @examples
#' data(colleges)
#' subset(colleges,colleges$HBCU==1)[c(2)]
#' 
#' @source \url{https://collegescorecard.ed.gov/data/documentation/}
"colleges"

#' Data Dictionary
#'
#' Provides all variables and their definitions within the College Scorecard data.
#'
#' @docType data
#'
#' @format A dataframe of 1,953 observations and 9 variables
#'
#' @keywords datasets
#' 
#' @examples
#' data(dataDict)
#' scorecardInfo(c("CITY","INSTNM","UNITID"))
#'
#' @source \url{https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary-09-08-2015.csv}
"dataDict"


#' 2013 Scorecard
#'
#' 2013 College Scorecard data including all available variables and subset to colleges that
#' predominantly offer an Bachelor's Degree. 
#' Refer to dataDict for a data dictionary of fields.
#'
#' @name scorecard13
#' 
#' @docType data
#'
#' @format A dataframe of 2,133 observations and 1,729 variables
#'
#' @keywords datasets
#'
#' @examples
#' data(scorecard13)
#' top5Degrees(,scorecard13,c("Stanford University"))
#'
#' @source \url{https://collegescorecard.ed.gov/data/documentation/}
"scorecard13"