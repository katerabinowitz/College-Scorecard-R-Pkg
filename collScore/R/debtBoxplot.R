#' Returns a box plot of the 10th, 25th, 50th, 75th, and 90th percentiles of student debt by school. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.  
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @examples
#' data(scorecard13)
#' debtBoxplot(,scorecard13,c("New York University","Cornell University"))
#' @export
#'
debtBoxplot<-function(apiKey,dataset,schools) {
  debtPer<-subsetToCategory("aid",apiKey,dataset,schools)
  debtPer<-subset(debtPer,(grepl("cumulative_debt.",debtPer$developer.friendly.name) & 
                             grepl("percentile",debtPer$developer.friendly.name)) | 
                    grepl("median_debt_suppressed.overall",debtPer$developer.friendly.name))
  debtPer$var<-gsub("cumulative_debt","P",debtPer$developer.friendly.name)
  debtPer$var<-gsub("_percentile","",debtPer$var)
  debtPer<-debtPer[c("variable","value","INSTNM")]
  debtPer$value<-as.numeric(debtPer$value)
  debtPerPlot<-reshape2::dcast(debtPer, INSTNM~variable)
  ggplot2::ggplot(debtPerPlot, ggplot2::aes(debtPerPlot$INSTNM)) +
    ggplot2::geom_boxplot(fill = "white", colour = "#3366FF",stat = "identity") +
    ggplot2::aes(ymin = debtPerPlot$CUML_DEBT_P10, lower = debtPerPlot$CUML_DEBT_P25, 
                 middle=debtPerPlot$DEBT_MDN_SUPP, upper = debtPerPlot$CUML_DEBT_P75, 
                 ymax = debtPerPlot$CUML_DEBT_P90) +
    ggplot2::scale_colour_brewer(palette = "Set1") +
    ggplot2::labs(x="",y="Debt ($)") 
}
