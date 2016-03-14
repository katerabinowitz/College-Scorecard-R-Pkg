#' Plots repayment comparison based on income for specified school(s). Here repayment is the fraction of borrowers successfully repaying their loan
#' x years after entering repayment. If schoolNames is not specified, data for all schools will be returned
#'  
#' @param apiKey If using API please provide saved API key value here
#' @param dataset If not using API please provide dataset name here
#' @param year Year of college scorecard data
#' @param schoolNames The names of schoold to be compared
#' @param repaymentYears Years after entering repayment
#' @return ggplot2 object
#' @examples
#' data(scorecard13)
#' repaymentRateByIncome(dataset = scorecard13, year = 2013, 
#' schoolNames = c("University of Massachusetts-Lowell",
#' "Massachusetts Institute of Technology", "Drake University"))
#' @export
repaymentRateByIncome <- function(apiKey, dataset, year = 2013, schoolNames, repaymentYears = 3) {
  
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
      message(paste("There is no data available for the selected year", as.character(x)))
    }
    else {
      plot1 <- ggplot2::ggplot(data = dataByYear, ggplot2::aes(x = dataByYear$income, y = dataByYear$value, fill=dataByYear$school.name)) + 
        ggplot2::geom_bar(stat = 'identity', position=ggplot2::position_dodge()) +
        ggplot2::scale_fill_brewer(palette = "Pastel1") +
        ggplot2::ggtitle(paste(repaymentYears, "Years Repayment Rate", "For Different Income Brackets", sep = " ")) +
        ggplot2::labs(x="Income Bracket", y="Repayment Rate (%)", fill='School Name') 
    }
    plot1
  }
  #...............................................................................................
  
  addParams <- "school.name"
  
  repaymentData <- getAllDataInCategory(apiKey, dataset = dataset, categoryName = "repayment", 
                                        year = year, pattern = "suppressed.income", addParams = addParams)
  meltedData <- reshape2::melt(repaymentData, id.vars = addParams)
  
  temp <- lapply(schoolNames, getDataPerSchool)
  if(!is.null(temp[[1]])) {
    meltedData <- do.call(rbind, temp)
    columnYear <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[1]))
    columnIncome <- unlist(lapply(as.character(meltedData$variable), getLast))
    columnRate <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[3]))
    meltedData$year <- columnYear
    meltedData$income <- columnIncome
    meltedData$repaymentRate <- columnRate
    meltedData$variable <- NULL
    repaymentR <- meltedData$repaymentRate %in% grep(as.character(repaymentYears), meltedData$repaymentRate, value = TRUE)
    meltedData <- subset(meltedData, repaymentR)
    meltedData$value <- as.numeric(meltedData$value)
    meltedData <- meltedData[with(meltedData, order(income)), ]
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