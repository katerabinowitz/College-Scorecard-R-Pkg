#' Returns the top five degrees for each school selected and their proportion. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @examples
#' data(scorecard13)
#' top5 <- top5Degrees(,scorecard13,c("Stanford University","University of Southern California"))
#' @export

top5Degrees<-function(apiKey,dataset,schools) {
  Degrees.Named<-subsetToCategory("academics",apiKey,dataset,schools)
  Degrees.Named<-subset(Degrees.Named,grepl("program_percentage",Degrees.Named$developer.friendly.name))
  
  Degrees.Named$Degree<-gsub("program_percentage.","",Degrees.Named$developer.friendly.name)
  Degrees.Named$Degree<-gsub("_"," ",Degrees.Named$Degree)
  Degrees.Named$Degree<-paste0(toupper(substr(Degrees.Named$Degree, 1, 1)), 
                               substr(Degrees.Named$Degree, 2, nchar(Degrees.Named$Degree)))

  Degrees.Named <- transform(Degrees.Named[order(Degrees.Named$value, decreasing=TRUE), ], 
                             INSTNM=as.factor(Degrees.Named$INSTNM))
  degreesD <- plyr::ddply(Degrees.Named, plyr::.(Degrees.Named$INSTNM), head, n=5)
  
  degreesD<-subset(degreesD,degreesD$INSTNM %in% schools)
  degreesD$percentofGrads<-(as.numeric(degreesD$value)*100)
  degreesD<-as.data.frame(degreesD)[c(2,5:6)]
  
  degreesD
}