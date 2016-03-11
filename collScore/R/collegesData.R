#' Colleges
#'
#' Dataframe of all the colleges included in the College Scorecard, along with attributes such as geography,
#' religious affliation, predominant degree awarded, and more. Users are encouraged to use this dataset for
#' producing vectors of schools for use in other functions. 
#'
#' @docType data
#'
#' @usage data(colleges)
#'
#' @format A dataframe
#'
#' @keywords colleges
#'
#' @source U.S. Department of Education https://collegescorecard.ed.gov/data/documentation/
#'
#' @examples
#' data(colleges)
#' subset(colleges,colleges$HBCU==1)[c(2)]
