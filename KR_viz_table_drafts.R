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

#' Returns the top five degrees for each school selected and their proportion. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.
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

### Students - Racial Makeup ###

#' Returns a horizontal stacked bar chart of the racial makeup of the selected schools. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.
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

### Median Debt By Groups ###

#' Returns a bar chart for median debt levels for selected demographic groups by school. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard. Leave bygroup field blank if only interested
#' overall median. 
#'
#' @param apiKey
#' @param dataset
#' @param schools
#' @param bygroup
#' @examples
#' medianDebtBy(,CS2013,c("Stanford University","Harvard University"),"gender")
#' @export
#' 
medianDebtBy<-function(apiKey,dataset,schools,bygroup="") {
  if (!(bygroup %in% c("","completion","income","dependence","Pell","gender","firstGen"))) {
    stop("Incorrect bygroup. Please kept bygroup empty or select one of the following: completion, income, dependence, Pell, gender, or firstGen")
  }
  medDebt<-subsetToCategory("aid",apiKey,dataset,schools)
  medDebt<-subset(medDebt,grepl("median_debt",medDebt$developer.friendly.name) & !(grepl(".number.",medDebt$developer.friendly.name)))
  
  if (missing(bygroup)) {
    medDebtPlot<-subset(medDebt,medDebt$developer.friendly.name=="median_debt_suppressed.overall")
    ggplot(data=medDebtPlot, aes(x=INSTNM, y=value)) +
      geom_bar(stat="identity") +
      scale_colour_brewer(palette = "Set1") +
      labs(x="School",y="Median Debt") 
  }
  else {
  if (bygroup=="completion") {
    medDebtPlot<-subset(medDebt,grepl("complete",medDebt$developer.friendly.name))
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
  medDebtPlot$byGroup<-paste0(toupper(substr(medDebtPlot$byGroup, 1, 1)), substr(medDebtPlot$byGroup, 2, nchar(medDebtPlot$byGroup)))
  
  if ("PrivacySuppressed" %in% medDebtPlot$value) {
    warning("Data has been withheld for privacy reasons.")
  }
  
  medDebtPlot$medDebt<-as.numeric(medDebtPlot$value)
  
  ggplot(data=medDebtPlot, aes(x=INSTNM, y=medDebt, fill=byGroup)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_colour_brewer(palette = "Set1") +
    labs(x="",y="Median Debt",fill="") 
  }
}

### Debt Percentiles ###
#' Returns a box plot of the 10th, 25th, 50th, 75th, and 90th percentiles of student debt by school. If using API please provide
#' apiKey and leave dataset field blank. If using a dataset, please leave apiKey field blank. Dataset must
#' use the same naming conventions are the College Scorecard.  
#'
#' @param apiKey
#' @param dataset
#' @param schools
#' @examples
#' debtPercentiles(,CS2013,c("Harvard University","Northeastern University"))
#' @export
#'
debtPercentiles<-function(apiKey,dataset,schools) {
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
