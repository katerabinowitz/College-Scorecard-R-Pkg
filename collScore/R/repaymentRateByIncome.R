#' Plots repayment comparison based on income for specified school(s). Here repayment is the fraction of borrowers successfully repaying their loan
#' x years after entering repayment. If schoolNames is not specified, data for all schools will be returned
#'  
#' @param apiKey If use API please provide saved API key value here
#' @param dataset If not using API please provide dataset name here
#' @param year Year of college scorecard data
#' @param schoolNames The names of schoold to be compared
#' @param repaymentYears Years after entering repayment
#' @return ggplot2 object
#' @examples
#' data(scorecard13)
#' repaymentRateByIncome(dataset = scorecard13, year = 2013, 
#' schoolNames = c("University of Massachusetts-Lowell","Massachusetts Institute of Technology", "Drake University"))
#' @export
repaymentRateByIncome <- function(apiKey, dataset, year = 2013, schoolNames, repaymentYears = 3) {
  
  if (missing(apiKey)) {
    addParams <- "name"
  }
  else {
    addParams <- "school.name"
  }
  
  
  repaymentData <- getAllDataInCategory(dataset = dataset, categoryName = "repayment", year = year, pattern = "suppressed.income", addParams = addParams)
  meltedData <- reshape2::melt(repaymentData, id.vars = addParams)
  
  if(length(schoolNames) > 1) {
    temp <- lapply(schoolNames, function(x) subset(meltedData, meltedData$name == x))
    meltedData <- do.call(rbind, temp)
  }
  
  helpF <- function(x) {
    res <- unlist(strsplit(x, ".", fixed = TRUE))
    res[length(res)]
  }
  
  columnYear <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[1]))
  columnIncome <- unlist(lapply(as.character(meltedData$variable), helpF))
  columnRate <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[3]))
  
  meltedData$year <- columnYear
  meltedData$income <- columnIncome
  meltedData$repaymentRate <- columnRate
  meltedData$variable <- NULL
  
  repaymentR <- meltedData$repaymentRate %in% grep(as.character(repaymentYears), meltedData$repaymentRate, value = TRUE)
  meltedData <- subset(meltedData, repaymentR)

  #colnames(meltedData)[colnames(meltedData) == field] <- addParams
  meltedData$value <- as.numeric(meltedData$value)
  meltedData <- meltedData[with(meltedData, order(income)), ]
  
  doPlotbyYear <- function(x) {
    plot1 <- NULL
    dataByYear <- subset(meltedData, year == x)
    if(all(is.na(dataByYear$value))) {
      warning(paste("There is no data available for the selected year", as.character(x)))
    }
    else {
      plot1 <- ggplot2::ggplot(data = dataByYear, ggplot2::aes(x = dataByYear$income, y = dataByYear$value, fill=dataByYear$name)) + 
        ggplot2::geom_bar(stat = 'identity', position=ggplot2::position_dodge()) + 
        ggplot2::ggtitle(paste("Repayment Rate", repaymentYears, "Years For Different Income Brackets", sep = " "))
    }
    plot1
  }
  
  myPlots <-  Filter(Negate(is.null), lapply(year, doPlotbyYear))
  if(length(myPlots) > 1) {
    numCols <- ceiling(length(myPlots)/2)
    multiplot(plotlist = myPlots, cols = numCols, byrow=TRUE)
  }
  else
    myPlots
}