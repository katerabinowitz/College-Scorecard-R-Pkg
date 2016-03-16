#' Get all data from a specified category. The available categories are 
#' root, school, academics, admissions, student, cost, aid, repayment, completion, earnings  
#'
#' @param apiKey Key used for interacting with the API
#' @param dataset If not using API please provide dataset name here
#' @param categoryName The category of data for retrieval. Options include: academics, admission, aid, cost, earnings, repayment,
#' root, school, or student. Please note that the categories aid and completion have too many variables for a single API call so
#' it is required that you specify a pattern. 
#' @param year The year(s) for data retrieval.
#' @param pattern Common text in name of parameters of interest
#' @param addParams Parameters outside of category to include in data retrieval
#' @return character vector
#' @examples
#' \dontrun{costData <- getAllDataInCategory(categoryName = "cost", year = 2013)}
#' \dontrun{schoolData <- getAllDataInCategory(categoryName = "school")}
#' \dontrun{earningsData <- getAllDataInCategory(categoryName = "earnings", 
#' year = c(2010, 2013))}
#' \dontrun{earningsData <- getAllDataInCategory(categoryName = "earnings", 
#'   year = c(2010, 2013), pattern = "6_yrs_after_entry.mean", addParams = "school.state")}
#' @export
getAllDataInCategory <- function(apiKey, dataset, categoryName, year, pattern = "", addParams = "id,school.name"){
  isYearValid <- function(value){
    isValid <- all(unlist(lapply(value, function(x) !(x<1996 | x>2013))))
    isValid
  }
  
  if (!(categoryName %in% c("academics","admissions","aid","completion","cost","earnings","repayment","root",
                            "school","student"))) {
    stop ("Incorrect categoryName. Please choose from the following: 'academics','admissions','aid','completion,'cost',earnings','repayment','root','school', or 'student'. Consult data dictionary for further detail.")
  }
  
  if(!missing(year) && !(categoryName=="root" || categoryName=="school")){
    if (!isYearValid(year)) {
      stop("Incorrect year selection. Data is available for 1996 through 2013.")
    }
  }
  
  ##
  ## Start cathrynr code
  ##
  data(dataDict,  envir = environment())
  
  categoryVars <- subset(dataDict, dataDict$dev.category==categoryName)[c("developer.friendly.name")]
  
  categoryVars <- categoryVars[categoryVars != ""]
  ##
  ## End cathrynr code
  ##
  categoryVars <- grep(pattern, categoryVars, value = TRUE)
  
  if (missing(apiKey)) {
    # dataset included with package uses only varaible names so the developer name 
    # needs to be converted to variable name using the data dictionary
    varNames <- unlist(lapply(categoryVars, convertDevNameToVarName))
    
    if (categoryName !="school") {
      categoryVars <- lapply(categoryVars, function(x) paste(year, ".", categoryName, ".", x, sep = ""))
    }
    if(any(grepl(addParams, unlist(categoryVars)))){
      categoryVars[grepl("school.name|name", categoryVars)] <- "school.name"
      selectVars <- varNames
      colNames <- unlist(categoryVars)
    }
    else {
      selectVars <- c(varNames, convertDevNameToVarName(addParams))
      colNames <- c(unlist(categoryVars), addParams)
    }
    
    DFcat <- subset(dataset, select = selectVars)
    colnames(DFcat) <- colNames
  }
  else {
    ##
    ## Start cathrynr code
    ##
    if (categoryName=="root") {
      queryList <- paste("fields=", paste(lapply(categoryVars, 
                                                 function(x) paste(x, sep = "")), collapse = ","), sep = "")
      }
    else if (categoryName=="school") {
      queryList <- paste("fields=id,", paste(lapply(categoryVars, 
                                                    function(x) paste(categoryName, ".", x, sep = "")), collapse = ","), sep = "")  
      }
    else {
      queryList <- paste("fields=", addParams, ",", paste(lapply(categoryVars, 
                                                                 function(x) paste(lapply(year, function(x) paste(x, ".", categoryName, sep = "")),
                                                                                   ".", x, sep = "", collapse = ",")), collapse = ","), sep = "")
      }
    ##
    ## End cathrynr code
    ##
    DFcat <- getData(apiKey=apiKey,fieldParams = queryList)
    }
  DFcat
}