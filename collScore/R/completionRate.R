#' Returns a bar chart for median debt levels for selected demographic groups by school. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard. Leave bygroup field blank if only interested
#' overall median. 
#'
#' @param apiKey If use API please provide saved API key value here 
#' @param dataset If not using API please provide dataset name here
#' @param schools The schools you are retrieving data for
#' @param bygroup Leave this blank to see completion rates overall, otherwise populate with 'race' to see completion by race
#' @examples
#' data(scorecard13)
#' completionRate(,scorecard13,c("University of Chicago","Northwestern University"),"race")
#' @export
#' 
completionRate<-function(apiKey,dataset,schools,bygroup="") {
  if (! (bygroup %in% c("","race"))) {
    stop("Incorrect bygroup. Please keep bygroup empty or select race.")
  }
  compRate<-subsetToCategory("completion",apiKey,dataset,schools)
  compRate<-subset(compRate,grepl("completion_rate",compRate$developer.friendly.name))
  
  compRate$rate<-suppressWarnings((as.numeric(compRate$value))*100)
  
  compRate<-subset(compRate,!(is.na(compRate$rate)))
  
  if (bygroup=="") {
    compRatePlot<-subset(compRate,grepl("150nt",compRate$developer.friendly.name) &
                           (!grepl("pool",compRate$developer.friendly.name)))
    ggplot2::ggplot(data=compRatePlot, aes(x=INSTNM, y=rate)) +
      geom_bar(stat="identity") +
      scale_colour_brewer(palette = "Set1") +
      labs(x="",y="Completion Rate (%)") 
  }
  else if (bygroup=="race"){
    compRatePlot<-subset(compRate,!(grepl("150nt",compRate$developer.friendly.name)))
    
    compRatePlot$bygroup<-gsub("^.*_","",compRatePlot$developer.friendly.name)
    compRatePlot$bygroup<-gsub("\\."," ",compRatePlot$bygroup)
    compRatePlot$bygroup<-paste0(toupper(substr(compRatePlot$bygroup, 1, 1)), 
                                 substr(compRatePlot$bygroup, 2, nchar(compRatePlot$bygroup)))
    
    ggplot2::ggplot(data=compRatePlot, ggplot2::aes(x=bygroup, y=rate, fill=INSTNM)) +
      ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge()) +
      ggplot2::scale_fill_brewer(palette = "Pastel1") +
      ggplot2::labs(x="",y="Completion Rate (%)",fill="School") 
  }
}   
