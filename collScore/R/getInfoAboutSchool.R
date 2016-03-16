#' Returns information about the requested school(s). The information is returned in a table with
#' columns for name of the school, location, type (private, public, etc, ), religious affiliation,
#' predominant degrees awarded, highest degree awarded, man only or women only
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schoolNames Names of schools
#' @examples
#' data(scorecard13)
#' getInfoAboutSchool(,scorecard13, schoolNames = "University of Massachusetts-Lowell")
#' getInfoAboutSchool(,scorecard13, schoolNames = c("Judson College",
#' "Birmingham Southern College", "University of Alaska Fairbanks"))
#' @export
getInfoAboutSchool <- function(apiKey, dataset, schoolNames) {
  
  # Populate column based on the column name and data obtained from the data dictionary
  createColumn <- function(varName) {
    val <- selectedData[selectedData$school.name == schoolNames, varName]
    colVal <- dataDict[which(dataDict$VARIABLE.NAME == varName) + suppressWarnings(as.numeric(val)), "LABEL"]
    #selectedData$PREDDEG <- colVal
    selectedData[, varName] <<- colVal
  }
  
  data(dataDict,  envir = environment())
  schoolData <- getAllDataInCategory(apiKey, dataset = dataset, categoryName = "school", addParams = "name")
  
  colNames <- unlist(lapply(colnames(schoolData), convertDevNameToVarName))
  colNames <- gsub("INSTNM", "school.name", x = colNames, fixed = TRUE)
  colnames(schoolData) <- colNames
  temp <- lapply(schoolNames, getDataPerSchool, dataDf = schoolData)
  
  if(!is.null(temp[[1]])) {
    # Create Location column by combining CITY and STATE
    selectedSchools <- do.call(rbind, temp)
    selectedData <- subset(selectedSchools, select = c("school.name", "CITY", "STABBR", "PREDDEG", "HIGHDEG", "CONTROL", "MENONLY", "WOMENONLY", "RELAFFIL"))
    selectedData$Location <- paste(selectedData$CITY, selectedData$STABBR, sep = ",")
    selectedData$CITY <- NULL
    selectedData$STABBR <- NULL
    
    # Get available values for religious affiliation from data dictionary and
    # create a column using the label for those values
    relAff <- dataDict[c(which(dataDict$VARIABLE.NAME == "RELAFFIL"):which(dataDict$VARIABLE.NAME == "ADM_RATE")), c("VALUE", "LABEL")] 
    val <- suppressWarnings(as.numeric(selectedData$RELAFFIL[selectedData$school.name == schoolNames]))
    val[is.na(val)] <- 0
    colVal <- relAff[relAff$VALUE == val, "LABEL"]
    selectedData$Religion <- colVal
    selectedData$RELAFFIL <- NULL
    
    # Create the rest of the columns
    varNames <- c("PREDDEG", "CONTROL", "HIGHDEG", "MENONLY", "WOMENONLY")
    lapply(varNames, createColumn)
    
    
    selectedData <- selectedData[c(1, 7, 4, 2, 3, 8, 6, 5)]
    colnames(selectedData) <- c("School Name", "Location", "Type of Institution", "Predominant Degree Awarded", 
                                "Highest Degree Awarder", "Religious Affiliation", "Women Only", "Men Only")
    selectedData
  }
}