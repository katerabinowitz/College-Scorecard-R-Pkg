#' Returns a horizontal stacked bar chart of the racial makeup of the selected schools. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @examples
#' studentRace(,scorecard13,c("Yale University","Harvard University"))
#' @export
studentRace<-function(apiKey,dataset,schools) {
  race<-subsetToCategory("student",apiKey,dataset,schools)
  race<-subset(race,grepl("demographics.race_ethnicity",race$developer.friendly.name))
  race$Proportion<-(as.numeric(race$value))*100
  race<-subset(race,!(is.na(race$Proportion)))
  race$Race<-gsub("demographics.race_ethnicity.","",race$developer.friendly.name)
  race$Race<-gsub("_"," ",race$Race)
  race$Race<-paste0(toupper(substr(race$Race, 1, 1)), 
                    substr(race$Race, 2, nchar(race$Race)))
  
  ggplot(aes(y=Proportion, x=INSTNM, fill = factor(Race)), data = race) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    scale_fill_brewer(palette = "Set3") +
    labs(x="",y="Proportion (%)",fill="Race") 
}