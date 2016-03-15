#..................Helper Functions.............................................................

#' Returns the VARIABLE.NAME corresponding to the developer.friendly.name. When using the REST API, only the developer.friendly.name works when 
#' querying the data. However, when using the data included in the package, only the VARIABLE.NAME is available in the dataset. This
#' functions provides an easy way to get the developer.friendly.name when only the VARIABLE.NAME is known.
#'
#' @param devFriendlyName developer.friendly.name as per data dictionary
#' @return character
#' convertDevNameToVarName("8_yr_completion.low_income")
convertDevNameToVarName <- function(devFriendlyName){  
  data(dataDict,  envir = environment())
  devFriendlyName <- gsub("school.", replacement = "", x = devFriendlyName)
  varName <- dataDict$VARIABLE.NAME[dataDict$developer.friendly.name == devFriendlyName]
  varName
} 

#' Helper function to get data based on school name if data for the specified school is available
#'
#' @param schoolName Name of school
#' @param dataDf Dataframe to get data from
#' @return data.frame
getDataPerSchool <- function(schoolName, dataDf) {
  schoolData <- subset(dataDf, dataDf$school.name == schoolName)
  if(nrow(schoolData) == 0){
    message(paste("No data is available for the selected school", schoolName, 
                  "therefore data for this school will not be displayed", sep = " "))
  }
  else {
    schoolData
  }
}

#' Splits a string and returns the last element
#'
#' @param x String to split
#' @return character
getLastElement <- function(x) {
  res <- unlist(strsplit(x, ".", fixed = TRUE))
  res[length(res)]
}

#' Subsets data by year
#'
#' @param year Year to subset by
#' @param dataDf Data to subset from
#' @return character
getByYear <- function(year, dataDf) {
  dataByYear <- subset(dataDf, year == year)
  if(all(is.na(dataByYear$value))) {
    message(paste("There is no data available for the selected year", as.character(year)))
  }
  else {
    dataByYear
  }
}

