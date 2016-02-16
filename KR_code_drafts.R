### Easy API to data frame testing ###
library(httr)
apiKey=""
schoolInsert="DC"
myUrl = paste("https://api.data.gov/ed/collegescorecard/v1/schools?school.state=",schoolInsert,
  "&_fields=id,school.name,&api_key=",apiKey,sep="")
res <- GET(myUrl)
result <- content(res, as = "parsed")
DF  <-  as.data.frame(t(matrix(unlist(result$results), nrow=length(unlist(result$results[1])))))
colnames(DF)<-names(result$results[[1]])

### Academic Section ###
#programmatically choose all academic variables for API call
dd<-read.csv("/Users/katerabinowitz/Documents/StanfordGrad/STATS290/Package/College-Scorecard-R-Pkg/data/CollegeScorecardDataDictionary-09-08-2015.csv",
             stringsAsFactors=FALSE)
academic<-subset(dd,dd$dev.category=="academics")
academic<-academic[(1:38),]
academic$developer.friendly.name<-paste("2013.academics.",academic$developer.friendly.name,sep="")
variables<-as.data.frame(academic$developer.friendly.name)

toRead<-"school.name"
for(i in 1:(nrow(variables)-1))
{
  toRead<-paste(toRead,academic$developer.friendly.name[i],academic$developer.friendly.name[i+1],sep=",")
}
toRead
#because the variable names are so long, the API runs into length issues really quickly. 

myUrl = paste("https://api.data.gov/ed/collegescorecard/v1/schools.json?school.state=",schoolInsert,
              "&_fields=",toRead,",&api_key=",apiKey,sep="")

res <- GET(myUrl)
result <- content(res, as = "parsed")
DF  <-  as.data.frame(matrix(unlist(result$results), nrow=length(unlist(result$results[1]))))
#this worked on a more a straight forward example above, but it looks like here the number of variables
#that were returned vary by school. 39 variables requested for 20 schools, but only 590 observations
colnames(dd)<-names(result$results[[1]])

### Student Section ###


### Aid Section ###


### Completion Section ###