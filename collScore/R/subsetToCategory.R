#' Subsets included College Scorecard data to specific category and 
#' prepares for data visualization. 
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @param category The category of data for retrieval. Options include: academics, 
#' admission, aid, completion, cost, earnings, repayment, root, school, or student.
#' @param year Year of college scorecard data
#' @examples 
#' subsetToCategory("aid",,scorecard13,c("Hampshire College","Amherst College"))
#' @export
#' 

subsetToCategory<-function (category,apiKey,dataset,schools,year) {
  data(dataDict)
  catVars<-subset(dataDict,dataDict$dev.category==category) 
  variables<-catVars$VARIABLE.NAME
  variables<-c(variables, c="INSTNM")
  
  if (missing(apiKey)) {
    col.num <- which(colnames(dataset) %in% variables)
    catData <- dataset[,sort(c(col.num))]
  }
  
  else {
    catData <- GetAllDataInCategory(categoryName = category, year = year, addParams = INSTNM)
  }
  
  catData <- subset(catData, catData$INSTNM %in% schools)
  meltData<-reshape2::melt(catData, id.vars="INSTNM")
  namedData<-merge(x = meltData, y = dataDict[ , c("developer.friendly.name", "VARIABLE.NAME")], 
                   by.x="variable", by.y = "VARIABLE.NAME", all.x=TRUE)
}
