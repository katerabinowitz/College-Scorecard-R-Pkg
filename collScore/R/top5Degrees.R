#' Returns the top five degrees for each school selected and their proportion. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @examples
#' top5 <- top5Degrees(,scorecard13,c("Stanford University"))
#' @export

top5Degrees<-function(apiKey,dataset,schools) {
  Degrees.Named<-subsetToCategory("academics",apiKey,dataset,schools)
  Degrees.Named<-subset(Degrees.Named,grepl("program_percentage",Degrees.Named$developer.friendly.name))
  
  Degrees.Named$Degree<-gsub("program_percentage.","",Degrees.Named$developer.friendly.name)
  Degrees.Named$Degree<-gsub("_"," ",Degrees.Named$Degree)
  Degrees.Named$Degree<-paste0(toupper(substr(Degrees.Named$Degree, 1, 1)), substr(Degrees.Named$Degree, 2, nchar(Degrees.Named$Degree)))
  
  Degrees.Named<-Degrees.Named[order(Degrees.Named$INSTNM,Degrees.Named$value, decreasing=TRUE),]
  
  degreesD <- data.table(Degrees.Named)
  setkey(degreesD,INSTNM)
  degreesD<-degreesD[,lapply(.SD,function(x) head(x,5)),by = key(degreesD)]
  degreesD<-subset(degreesD,degreesD$INSTNM %in% schools)
  degreesD$percentofGrads<-(as.numeric(degreesD$value)*100)
  degreesD<-as.data.frame(degreesD)[c(1,5:6)]
  
  degreesD
}