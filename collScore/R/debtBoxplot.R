#' Returns a box plot of the 10th, 25th, 50th, 75th, and 90th percentiles of student debt by school. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.  
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @examples
#' debtBoxplot(,scorecard13,c("New York University","Cornell University"))
#' @export
#'

subsetToCategory<-function (category,apiKey,dataset,schools) {
  dataDict<-data(dataDict)
  catVars<-subset(dataDict,dataDict$dev.category==category) 
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

debtBoxplot<-function(apiKey,dataset,schools) {
  debtPer<-subsetToCategory("aid",apiKey,dataset,schools)
  debtPer<-subset(debtPer,(grepl("cumulative_debt.",debtPer$developer.friendly.name) & 
                             grepl("percentile",debtPer$developer.friendly.name)) | 
                    grepl("median_debt_suppressed.overall",debtPer$developer.friendly.name))
  debtPer$var<-gsub("cumulative_debt","P",debtPer$developer.friendly.name)
  debtPer$var<-gsub("_percentile","",debtPer$var)
  debtPer<-debtPer[c("variable","value","INSTNM")]
  debtPer$value<-as.numeric(debtPer$value)
  debtPerPlot<-dcast(debtPer, INSTNM~variable)
  ggplot(debtPerPlot, aes(INSTNM)) +
    geom_boxplot(fill = "white", colour = "#3366FF",
                 aes(ymin = CUML_DEBT_P10, lower = CUML_DEBT_P25, middle=DEBT_MDN_SUPP ,upper = CUML_DEBT_P75, ymax = CUML_DEBT_P90),
                 stat = "identity") +
    scale_colour_brewer(palette = "Set1") +
    labs(x="",y="Debt ($)") 
}
