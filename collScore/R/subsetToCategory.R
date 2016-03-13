#' Subsets included College Scorecard data to specific category and 
#' prepares for data visualization. 
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @param category The category of data for retrieval. Options include: academics, 
#' admission, aid, completion, cost, earnings, repayment, root, school, or student.
#' @param year Year of college scorecard data
#' @param grepl This is a text field that further subsets the API request call from overall 
#' category to fields within that category that match the text pattern indicated.
#' @examples 
#' data(scorecard13)
#' subsetToCategory("aid",,scorecard13,c("Hampshire College","Amherst College"))
#' @export
#' 
##
## Start cathrynr code
##
subsetToCategory<-function (category,apiKey,dataset,schools,year=2013,grepl="") {
  data(dataDict,  envir = environment())
  catVars<-subset(dataDict,dataDict$dev.category==category) 
  variables<-catVars$VARIABLE.NAME
  
  if (missing(apiKey)) {
    variables<-c(variables, c="INSTNM")
    col.num <- which(colnames(dataset) %in% variables)
    catData <- dataset[,sort(c(col.num))]
    catData <- subset(catData, catData$INSTNM %in% schools)
    meltData<-reshape2::melt(catData, id.vars="INSTNM")
    namedData<-merge(x = meltData, y = dataDict[ , c("developer.friendly.name", "VARIABLE.NAME")], 
                     by.x="variable", by.y = "VARIABLE.NAME", all.x=TRUE)
  }
  
  else {
    catData <- getAllDataInCategory(apiKey, categoryName = category, year = year,
                                    addParams = "school.name",pattern=grepl)
    request<-subset(catData,catData$school.name %in% schools)
    namedData<-suppressWarnings(reshape2::melt(request, id.vars="school.name"))
    colnames(namedData)<-c("INSTNM","developer.friendly.name","value")
    namedData$developer.friendly.name<-gsub(year,"",namedData$developer.friendly.name)
    namedData$developer.friendly.name<-gsub(category,"",namedData$developer.friendly.name)
  }
  namedData
}
##
## End cathrynr code
##