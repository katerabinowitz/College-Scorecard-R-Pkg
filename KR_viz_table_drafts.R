setwd("/Users/katerabinowitz/Documents/StanfordGrad/STATS290/Package")

library(reshape2)
library(data.table)
library(ggplot2)

dd<-read.csv("CollegeScorecardDataDictionary-09-12-2015.csv",stringsAsFactors=FALSE,strip.white=TRUE) 
CS2013<-read.csv("MERGED2013_PP.csv", stringsAsFactors=FALSE,strip.white=TRUE) 

### Academic - Top 5 Degrees ###
academic<-subset(dd,dd$dev.category=="academics") 
academic<-academic[(1:38),] 
variables<-academic$VARIABLE.NAME
variables<-c(variables, c="INSTNM")

col.num <- which(colnames(CS2013) %in% variables)
cs13Academic <- CS2013[,sort(c(col.num))]
Degrees<-melt(cs13Academic, id.vars="INSTNM")
Degrees.Named<-merge(x = Degrees, y = dd[ , c("developer.friendly.name", "VARIABLE.NAME")], 
                     by.x="variable", by.y = "VARIABLE.NAME", all.x=TRUE)
Degrees.Named$Degree<-gsub("program_percentage.","",Degrees.Named$developer.friendly.name)
Degrees.Named$Degree<-gsub("_"," ",Degrees.Named$Degree)
Degrees.Named$Degree<-paste0(toupper(substr(Degrees.Named$Degree, 1, 1)), substr(Degrees.Named$Degree, 2, nchar(Degrees.Named$Degree)))

Degrees.Named<-Degrees.Named[order(Degrees.Named$INSTNM,Degrees.Named$value, decreasing=TRUE),]

degreesD <- data.table(Degrees.Named)
setkey(degreesD,INSTNM)
degreesD<-degreesD[,lapply(.SD,function(x) head(x,5)),by = key(degreesD)]

### Academic - Programs Offered ###


### Students - Racial Makeup ###
academic<-subset(dd,dd$dev.category=="student") 
variables<-academic$VARIABLE.NAME
variables<-c(variables, c="INSTNM")
col.num <- which(colnames(CS2013) %in% variables)
cs13Student <- CS2013[,sort(c(col.num))]
attach(cs13Student)
race<-cs13Student[ , which(names(cs13Student) %in% c("INSTNM","UGDS_WHITE","UGDS_BLACK","UGDS_HISP","UGDS_ASIAN","UGDS_AIAN",
                                                     "UGDS_NHPI","UGDS_2MOR","UGDS_NRA","UGDS_UNKN"))]
race<-melt(race, id.vars="INSTNM")
raceName<-merge(x = race, y = dd[ , c("developer.friendly.name", "VARIABLE.NAME")], 
                     by.x="variable", by.y = "VARIABLE.NAME", all.x=TRUE)
raceName$Race<-gsub("demographics.race_ethnicity.","",raceName$developer.friendly.name)
raceName$Race<-gsub("_"," ",raceName$Race)
raceName$Race<-paste0(toupper(substr(raceName$Race, 1, 1)), substr(raceName$Race, 2, nchar(raceName$Race)))
raceName$Proportion<-as.numeric(raceName$value)

raceName<-subset(raceName,raceName$Race %in% c("White","Black","Hispanic") & 
                   raceName$INSTNM %in% c("Boston University","Boston College","Harvard University","Northeastern University"))

ggplot(aes(y=Proportion, x=INSTNM, fill = factor(Race)), data = raceName) +
  geom_bar(stat = 'identity') +
  coord_flip()

### Student Family Income Rates ###

