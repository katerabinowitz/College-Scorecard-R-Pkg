setwd("/Users/katerabinowitz/Documents/StanfordGrad/STATS290/Package")
CS2013<-read.csv("MERGED2013_PP.csv", stringsAsFactors=FALSE,strip.white=TRUE) 

library(reshape2)
library(data.table)
library(ggplot2)

subsetToCategory<-function (category,apiKey,dataset,schools) {
  dataDict<-read.csv("https://raw.githubusercontent.com/katerabinowitz/College-Scorecard-R-Pkg/master/data/CollegeScorecardDataDictionary-09-08-2015.csv",
                     stringsAsFactors=FALSE)
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
    col.num <- which(colnames(dataset) %in% variables)
    catData <- subset(catData, catData$INSTNM %in% schools)
  }
  
  meltData<-melt(catData, id.vars="INSTNM")
  namedData<-merge(x = meltData, y = dataDict[ , c("developer.friendly.name", "VARIABLE.NAME")], 
                       by.x="variable", by.y = "VARIABLE.NAME", all.x=TRUE)
}

#' Returns the top five degrees for each school selected and their proportion
#'
#' @param apiKey
#' @param dataset
#' @param school
#' @examples
#' top5 <- top5Degrees(,CS2013,c("Stanford University","Harvard University"))
#' @export

top5Degrees<-function(apiKey,dataset,schools) {
  
  Degrees.Named<-subsetToCategory("academic",apiKey,dataset,schools)
  Degrees.Named<-subset(Degrees.Named,grepl("program_percentage",Degrees.Named$developer.friendly.name))
  
  Degrees.Named$Degree<-gsub("program_percentage.","",Degrees.Named$developer.friendly.name)
  Degrees.Named$Degree<-gsub("_"," ",Degrees.Named$Degree)
  Degrees.Named$Degree<-paste0(toupper(substr(Degrees.Named$Degree, 1, 1)), substr(Degrees.Named$Degree, 2, nchar(Degrees.Named$Degree)))

  Degrees.Named<-Degrees.Named[order(Degrees.Named$INSTNM,Degrees.Named$value, decreasing=TRUE),]

  degreesD <- data.table(Degrees.Named)
  setkey(degreesD,INSTNM)
  degreesD<-degreesD[,lapply(.SD,function(x) head(x,5)),by = key(degreesD)]
  degreesD<-subset(degreesD,degreesD$INSTNM=="Boston College" | degreesD$INSTNM=="Boston University")
  degreesD$percentofGrads<-(as.numeric(degreesD$value)*100)
  degreesD<-as.data.frame(degreesD)[c(1,5:6)]

  degreesD
}

### Academic - Programs Offered ###
out<-subsetToCategory("student",,CS2013,c("Boston University","Boston College"))

### Students - Racial Makeup ###

#' Returns a horizontal stacked bar chart of the racial makeup of the selected schools
#'
#' @param apiKey
#' @param dataset
#' @param schools
#' @examples
#' studentRace(,CS2013,c("Stanford University","Harvard University",))
#' @export
studentRace<-function(apiKey,dataset,schools) {
  race<-subsetToCategory("student",apiKey,dataset,schools)
  race<-subset(race,grepl("demographics.race_ethnicity",race$developer.friendly.name) &
                 !(grepl("prior_2009",race$developer.friendly.name) | grepl("_2000",race$developer.friendly.name)))
  race$Race<-gsub("demographics.race_ethnicity.","",race$developer.friendly.name)
  race$Race<-gsub("_"," ",race$Race)
  race$Race<-paste0(toupper(substr(race$Race, 1, 1)), substr(race$Race, 2, nchar(race$Race)))
  race$Proportion<-as.numeric(race$value)
  
  ggplot(aes(y=Proportion, x=INSTNM, fill = factor(Race)), data = race) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    scale_colour_brewer(palette = "Set1") +
    labs(x="School",y="Proportion (%)") +
}

### Student Family Income Rates ###

