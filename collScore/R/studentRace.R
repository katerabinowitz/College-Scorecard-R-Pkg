#' Returns a horizontal stacked bar chart of the racial makeup of the selected schools. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @examples
#' data(scorecard13)
#' studentRace(,scorecard13,c("Yale University","Harvard University"))
#' @export
##
## Start cathrynr code
##
studentRace<-function(apiKey,dataset,schools) {
  race<-subsetToCategory("student",apiKey,dataset,schools)
  race<-subset(race,grepl("demographics.race_ethnicity",race$developer.friendly.name))
  race$Proportion<-suppressWarnings((as.numeric(race$value))*100)
  race<-subset(race,!(is.na(race$Proportion)))
  race$Race<-gsub("demographics.race_ethnicity.","",race$developer.friendly.name)
  race$Race<-gsub("_"," ",race$Race)
  race$Race<-paste0(toupper(substr(race$Race, 1, 1)), 
                    substr(race$Race, 2, nchar(race$Race)))
  
  ggplot2::ggplot(ggplot2::aes(y=race$Proportion, x=race$INSTNM, fill = factor(race$Race)), data = race) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::labs(x="",y="Proportion (%)",fill="Race") 
}
##
## End cathrynr code
##