#' Returns a bar chart of family income based on the student type (aided, dependent, independent).
#' If using API please provide apiKey and leave dataset field blank. If using a dataset, please leave 
#' apiKey field blank. Dataset must use the same naming conventions are the College Scorecard. 
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @param bygroup Populate with aided, dependent or independent to view family income for those student subsets.
#' @examples
#' studentIncomeBy(,scorecard13,c("Boston University","Northeastern University"),"dependent")
#' @export
#' 
studentIncomeBy<-function(apiKey,dataset,schools,bygroup) {
  if (!(bygroup %in% c("aided","dependent","independent"))) {
    stop("Incorrect bygroup. Please kept bygroup empty or select one of the following: aided, dependent, or independent")
  }
  sIncome<-subsetToCategory("student",apiKey,dataset,schools)
  sIncome<-subset(sIncome,grepl("share_",sIncome$developer.friendly.name) & 
                    (grepl("income.",sIncome$developer.friendly.name)))
  sIncome$incomeShare<-(as.numeric(sIncome$value)*100)
  sIncome<-subset(sIncome,!(is.na(sIncome$incomeShare)))
  
  if (bygroup=="aided") {
    sIncomePlot<-subset(sIncome,!(grepl("share_dependent",sIncome$developer.friendly.name) 
                                  | grepl("share_independent",sIncome$developer.friendly.name)))
  }
  if (bygroup=="dependent") {
    sIncomePlot<-subset(sIncome,grepl("share_dependent",sIncome$developer.friendly.name))
  }
  if (bygroup=="independent") {
    sIncomePlot<-subset(sIncome,grepl("share_independent",sIncome$developer.friendly.name))
  }
  sIncomePlot$byGroup<-gsub("^.*\\.","$",sIncomePlot$developer.friendly.name)
  sIncomePlot$byGroup<-gsub("_","-",sIncomePlot$byGroup)
  sIncomePlot$byGroup<-ifelse(sIncomePlot$byGroup=="$110001plus","$110001+",sIncomePlot$byGroup)
  sIncomePlot$byGroup <- factor(sIncomePlot$byGroup,levels=c("$0-30000","$300001-48000",
                                                             "$48001-75000","$75001-110000",
                                                             "$110001+"),ordered=TRUE)
  
  ggplot(data=sIncomePlot, aes(x=INSTNM, y=incomeShare, fill=byGroup)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_brewer(palette = "Set1") +
    labs(x="",y="Percent",fill=" Family Income") 
}