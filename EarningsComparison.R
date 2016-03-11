library(reshape2)
library(ggplot2) 
library(scales)

#' Output a plot using ggplot2 displaying earnings per state for female and male students for either 6 or 10 years after entry to a college/university
#' If specifying multiple years, please follow the format c(year1, year2, year3). When multiple years are specified, a plot will be displayed
#' for each year. 
#'  
#'
#' @param year
#' @param addParams
#' @param offline
#' @param yearsAfterCompletion
#' @return ggplot2 object
#' @examples
#' earningsComparisonMaleFemale(2003, addParams = "state", offline = TRUE)
#' earningsComparisonMaleFemale(c(2003, 2005, 2007, 2009, 2011), addParams = "school.state", offline = FALSE)
#' @export
earningsComparisonMaleFemale <- function(year, addParams, offline, yearsAfterCompletion = 6) {
  #This method expects that the coumn names in the data frame are the developer friendly names from the data dictionary
  earningsData <- GetAllDataInCategory(categoryName = "earnings", year = year, pattern = "mean_earnings.female|mean_earnings.male", addParams = addParams, offline = offline)
  meltedData <- melt(earningsData, id.vars = "school.state")
  
  helpF <- function(x) {
    res <- unlist(strsplit(x, ".", fixed = TRUE))
    res[length(res)]
  }
  
  columnYear <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[1]))
  columtnSex <- unlist(lapply(as.character(meltedData$variable), helpF))
  yearsAfterEntry <- unlist(lapply(as.character(meltedData$variable), function(x) unlist(strsplit(x, ".", fixed = TRUE))[3]))
  
  meltedData$year <- columnYear
  meltedData$sex <- columtnSex
  meltedData$yearsAfterEntry <- yearsAfterEntry
  meltedData$variable <- NULL
  
  yearsAE <- meltedData$yearsAfterEntry %in% grep(as.character(yearsAfterCompletion), meltedData$yearsAfterEntry, value = TRUE)
  meltedData <- subset(meltedData, yearsAE)
  
  if ("PrivacySuppressed" %in% meltedData$value) {
    warning("Data has been withheld for privacy reasons.")
  }
  
  colnames(meltedData)[colnames(meltedData) == addParams] <- "state"
  meltedData$value <- as.numeric(meltedData$value)
  meltedData <- meltedData[with(meltedData, order(sex)), ]
  

  doPlotbyYear <- function(x) {
    plot1 <- NULL
    dataByYear <- subset(meltedData, year == x)
    if(all(is.na(dataByYear$value))) {
      warning(paste("There is no data available for the selected year", as.character(x)))
    }
    else {
      plot1 <- ggplot(data = dataByYear, aes_q(y=as.name(names(dataByYear)[2]), x=as.name(names(dataByYear)[1]), fill = as.name(names(dataByYear)[4]))) + 
        geom_bar(stat = 'identity') + 
        coord_flip() +
        ggtitle(paste("Earnings Data Collected", yearsAfterCompletion, "Years After Enrollment for Female and Male Students Based on State", sep = " "))
    }
    plot1
  #plot1 + scale_x_discrete(labels = comma)
  }
  
  myPlots <-  Filter(Negate(is.null), lapply(year, doPlotbyYear))
  if(length(myPlots) > 1) {
    numCols <- ceiling(length(myPlots)/2)
    multiplot(plotlist = myPlots, cols = numCols, byrow=TRUE)
  }
  else
    myPlots
}

#' This function has been taken from Cookbook for R (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
#' The function has been modified so plots <- plotlist
#'  
#' Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- plotlist
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}