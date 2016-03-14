#' Output a plot using ggplot2 displaying earnings per school for female and male students for either 6 or 10 years 
#' after entry to a college/university. If specifying multiple years, please follow the format c(year1, year2, year3). 
#' When multiple years are specified, a plot will be displayed for each year. If schoolNames is not specified,
#' data for all schools will be returned. If used with the data provided with this pavkage, this API will produce no
#' results since no data is available for variables in the earnings group
#'  
#' @param apiKey If using API please provide saved API key value here
#' @param dataset If not using API please provide dataset name here
#' @param year Year of college scorecard data
#' @param schoolNames The names of schoold to be compared
#' @param yearsAfterCompletion Available values are 6 and 10
#' @return ggplot2 object
#' @examples
#' \dontrun{femaleMaleEarnings(apiKey = ak, year = 2003, 
#' schoolNames = c("University of Massachusetts-Lowell", "Massachusetts Institute of Technology"))}
#' \dontrun{femaleMaleEarnings(apiKey = ak, year = c(2003, 2005, 2007, 2009, 2011), 
#' schoolNames = c("University of Massachusetts-Lowell", "Massachusetts Institute of Technology"))}
#' @export
femaleMaleEarnings <- function(apiKey, dataset, year, schoolNames, yearsAfterCompletion = 6) {
  
  #..................Helper Functions.............................................................
  # Helper function to get data based on school name if data for the specified school is available
  getDataPerSchool <- function(schoolName) {
    schoolData <- subset(meltedData, meltedData$school.name == schoolName)
    if(nrow(schoolData) == 0){
      message(paste("No data is available for the selected school", schoolName, "therefore data for this school will not be displayed", sep = " "))
    }
    else {
      schoolData
    }
  }
  
  # Splits a string and returns the last element
  getLast <- function(x) {
    res <- unlist(strsplit(x, ".", fixed = TRUE))
    res[length(res)]
  }
  
  # Subset the data by year if data is available for the supplied year and plot the data
  doPlotbyYear <- function(x) {
    plot1 <- NULL
    dataByYear <- subset(meltedData, year == x)
    if(all(is.na(dataByYear$value))) {
      warning(paste("There is no data available for the selected year", as.character(x)))
    }
    else {
      plot1 <- ggplot2::ggplot(data = dataByYear, ggplot2::aes(x = dataByYear$sex, y = dataByYear$value, fill=dataByYear$school.name)) + 
        ggplot2::geom_bar(stat = 'identity', position=ggplot2::position_dodge()) + 
        ggplot2::scale_fill_brewer(palette = "Pastel1") +
        ggplot2::ggtitle(paste("Earnings", yearsAfterCompletion, "Years After Enrollment for Female and Male Students", sep = " ")) +
        ggplot2::labs(x="Sex", y="Earnings ($)", fill='School Name') 
    }
    plot1
  }
  #...............................................................................................
  
  addParams <- "school.name"

  earningsData <- getAllDataInCategory(apiKey, dataset = dataset, categoryName = "earnings", year = year, 
                                       pattern = "mean_earnings.female|mean_earnings.male", addParams = addParams)
  meltedData <- reshape2::melt(earningsData, id.vars = addParams)
  
  temp <- lapply(schoolNames, getDataPerSchool)
  if(!is.null(temp[[1]])) {
    meltedData <- do.call(rbind, temp)
    columnYear <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[1]))
    columtnSex <- unlist(lapply(as.character(meltedData$variable), getLast))
    yearsAfterEntry <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[3]))
    meltedData$year <- columnYear
    meltedData$sex <- columtnSex
    meltedData$yearsAfterEntry <- yearsAfterEntry
    meltedData$variable <- NULL
  
    yearsAE <- meltedData$yearsAfterEntry %in% grep(as.character(yearsAfterCompletion), meltedData$yearsAfterEntry, value = TRUE)
    meltedData <- subset(meltedData, yearsAE)
  
    if ("PrivacySuppressed" %in% meltedData$value) {
      message("Data has been withheld for privacy reasons.")
    }
  
    colnames(meltedData)[colnames(meltedData) == addParams] <- "state"
    meltedData$value <- as.numeric(meltedData$value)
    meltedData <- meltedData[with(meltedData, order(sex)), ]
  
    myPlots <-  Filter(Negate(is.null), lapply(year, doPlotbyYear))
    if(length(myPlots) > 1) {
      numCols <- ceiling(length(myPlots)/2)
      multiplot(plotlist = myPlots, cols = numCols, byrow=TRUE)
    }
    else {
      myPlots
    }
  }
  else {
    message("No data is available for requested school(s)")
  }
}