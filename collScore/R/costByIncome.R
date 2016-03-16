#' Plots comparison of the average net price for different shcools based on student income level. The income levels are as follows:
#' Bracket 1: $0 - $30000 
#' Bracket 2: $30001 - $48000
#' Bracket 3: $48001 - $75000
#' Bracket 4: $75001 - $110000
#' Bracket 5: $110001-plus
#'  
#' @param apiKey If using API please provide saved API key value here
#' @param dataset If not using API please provide dataset name here
#' @param year Year of college scorecard data
#' @param schoolNames The names of schoold to be compared
#' @param type Type of institution. CUrrently the available options are: public, private
#' @return ggplot2 object
#' @examples
#' data(scorecard13)
#' costByIncome(dataset = scorecard13, year = 2013, schoolNames = 
#' c("University of Massachusetts-Lowell", "University of Arizona", "Alabama A & M University"))
#' costByIncome(dataset = scorecard13, year = 2013, schoolNames = c("Massachusetts Institute of 
#' Technology","Drake University", "Stanford University"), type = "private")
#' @export
#'
##
## Start ilianav code
##
costByIncome <- function(apiKey, dataset, year = 2013, schoolNames, type = "public") {
  
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
        ggplot2::ggtitle(paste("Cost of a", chartr("p", "P", type), "Institution for Different Income Brackets", sep = " ")) +
        ggplot2::labs(x="Income Bracket", y="Cost ($)", fill='School Name') 
    }
    plot1
  }
  
  addParams <- "school.name"
  
  repaymentData <- getAllDataInCategory(apiKey, dataset = dataset, categoryName = "cost", 
                                        year = year, paste("net_price.", as.character(type), ".by_income_level", sep = ""), addParams = addParams)
  meltedData <- reshape2::melt(repaymentData, id.vars = addParams)
  
  temp <- lapply(schoolNames, getDataPerSchool, dataDf = meltedData)
  if(!is.null(temp[[1]])) {
    meltedData <- do.call(rbind, temp)
    columnYear <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[1]))
    columnType <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[4]))
    columnIncome <- unlist(lapply(as.character(meltedData$variable), getLastElement))
    meltedData$year <- columnYear
    meltedData$income <- columnIncome
    meltedData$type <- columnType
    meltedData$variable <- NULL
    incomeSelected <- meltedData$income %in% grep("0-30000|30001-48000|48001-75000|75001-110000|110001-plus", meltedData$income, value = TRUE)
    meltedData <- subset(meltedData, incomeSelected)
    meltedData$income <- factor(meltedData$income, levels = c("0-30000", "30001-48000", "48001-75000", "75001-110000", "110001-plus"))
    meltedData$value <- suppressWarnings(as.numeric(meltedData$value))
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
##
## End ilianav code
##