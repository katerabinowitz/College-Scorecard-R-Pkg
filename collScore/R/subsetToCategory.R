#' Subsets included College Scorecard data to specific category and 
#' prepares for data visualization. 
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @param categoryName The category of data for retrieval. Options include: academics, 
#' admission, aid, completion, cost, earnings, repayment, root, school, or student. 
#' @examples 
#' subsetToCategory("aid",,scorecard13,c("Hampshire College","Amherst College"))
#' @export
#' 

subsetToCategory<-function (categoryName,apiKey,dataset,schools) {
  catVars<-subset(dataDict,dataDict$dev.category==categoryName) 
  variables<-catVars$VARIABLE.NAME
  variables<-c(variables, c="INSTNM")
  
  if (missing(apiKey)) {
    col.num <- which(colnames(dataset) %in% variables)
    catData <- dataset[,sort(c(col.num))]
    catData <- subset(catData, catData$INSTNM %in% schools)
  }
  
  else {
    ##ENTER API RETRIEVAL HERE##
    #col.num <- which(colnames(apiData) %in% variables)
    #catData <- subset(catData, catData$INSTNM %in% schools)
  }
  
  meltData<-melt(catData, id.vars="INSTNM")
  namedData<-merge(x = meltData, y = dataDict[ , c("developer.friendly.name", "VARIABLE.NAME")], 
                   by.x="variable", by.y = "VARIABLE.NAME", all.x=TRUE)
}
