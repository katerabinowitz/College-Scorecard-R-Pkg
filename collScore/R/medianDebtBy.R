#' Returns a bar chart for median debt levels for selected demographic groups by school. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard. Leave bygroup field blank if only interested
#' overall median. 
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @param bygroup Leave this blank to see median debt overall, otherwise populate with any of the following bygroups to see
#' median debt by category: completion, income, gender, Pell, dependence, firstGen
#' @examples 
#' data(scorecard13)
#' medianDebtBy(,scorecard13,c("Hampshire College","Amherst College"),"income",)
#' @export
#' 
##
## Start cathrynr code
##
medianDebtBy<-function(apiKey,dataset,schools,bygroup="",year="2013") {
  if (!(bygroup %in% c("","completion","income","dependence","Pell","gender","firstGen"))) {
    stop("Incorrect bygroup. Please kept bygroup empty or select one of the following: completion, income, dependence, Pell, gender, or firstGen")
  }
  medDebt<-subsetToCategory("aid",apiKey,dataset,schools,year)
  medDebt<-subset(medDebt,grepl("median_debt",medDebt$developer.friendly.name) & 
                    !(grepl(".number.",medDebt$developer.friendly.name)))
  
  if (missing(bygroup)) {
    medDebtPlot<-subset(medDebt,medDebt$developer.friendly.name=="median_debt_suppressed.overall")
    ggplot2::ggplot(data=medDebtPlot, ggplot2::aes(x=medDebtPlot$INSTNM, y=medDebtPlot$value)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::scale_colour_brewer(palette = "Set1") +
      ggplot2::labs(x="School",y="Median Debt") 
  }
  else {
    if (bygroup=="completion") {
      medDebtPlot<-subset(medDebt,grepl("complete",medDebt$developer.friendly.name) & 
                            !(grepl("monthly",medDebt$developer.friendly.name) |
                                grepl("press",medDebt$developer.friendly.name)))
      medDebtPlot$developer.friendly.name<-gsub(".overall","",medDebtPlot$developer.friendly.name)
    }
    if (bygroup=="income") {
      medDebtPlot<-subset(medDebt,grepl(".income.",medDebt$developer.friendly.name))
    }
    if (bygroup=="dependence") {
      medDebtPlot<-subset(medDebt,grepl("dependent",medDebt$developer.friendly.name))
    }
    if (bygroup=="Pell") {
      medDebtPlot<-subset(medDebt,grepl("pell",medDebt$developer.friendly.name))
    }
    if (bygroup=="gender") {
      medDebtPlot<-subset(medDebt,grepl("male_",medDebt$developer.friendly.name))
    }
    if (bygroup=="firstGen") {
      medDebtPlot<-subset(medDebt,grepl("first_generation",medDebt$developer.friendly.name))
    }
    medDebtPlot$byGroup<-gsub("median_debt.","",medDebtPlot$developer.friendly.name)
    medDebtPlot$byGroup<-gsub("_"," ",medDebtPlot$byGroup)
    medDebtPlot$byGroup<-gsub("\\.","",medDebtPlot$byGroup)
    medDebtPlot$byGroup<-paste0(toupper(substr(medDebtPlot$byGroup, 1, 1)), substr(medDebtPlot$byGroup, 2, nchar(medDebtPlot$byGroup)))
    
    medDebtPlot$byGroup<-ifelse(medDebtPlot$byGroup=="Income0 30000","Income $0-30000",
                                ifelse(medDebtPlot$byGroup=="Income30001 75000",
                                       "Income $30001-75000",
                                       ifelse(medDebtPlot$byGroup=="Incomegreater than 75000",
                                              "Income $75000+",
                                              medDebtPlot$byGroup)))
    
    medDebtPlot$medDebt<-suppressWarnings(as.numeric(medDebtPlot$value))
    
    ggplot2::ggplot(data=medDebtPlot, ggplot2::aes(x=medDebtPlot$INSTNM, y=medDebtPlot$medDebt,
                                                   fill=medDebtPlot$byGroup)) +
      ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge()) +
      ggplot2::scale_colour_brewer(palette = "Set1") +
      ggplot2::labs(x="",y="Median Debt ($)",fill="") 
  }
}
##
## End cathrynr code
##