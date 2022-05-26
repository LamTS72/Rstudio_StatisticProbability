#import data from chicken_feed.csv
#set alpha = 0.05
#H0: The long-run mean weights are the same under all six crops.
#H1: At least one of the long-run mean weights is different.


chickendata<-read.csv("chicken_feed.csv",header = T,sep= ",")
chickendata
chickendata<-read.csv("C:/Users/Admin/Downloads/chicken_feed.csv", header = T, colClasses = c('factor','numeric','factor'))
summary(chickendata)
#handle missing value in table
is.na(chickendata) #find missing value
which(is.na(chickendata),arr.ind = T) #identify index
sum(is.na(chickendata)) #count number of missing value
chickendata<-na.omit(chickendata) #omit missing value and update chickendata
chickendata #new table

drop_na(chickendata)
sum(is.na(chickendata))# check again the number of missing value

unique(chickendata$feed) #show number of feed



boxplot(weight~feed,data=chickendata,col='gray') #show boxplot from data

Anova_result<-aov(weight~feed,data=chickendata)#handle data
summary(Anova_result) #show table 

p_valWeight=summary(Anova_result)[[1]][["Pr(>F)"]] #get F_value from table Anova_Result
p_value=p_valWeight[1]
p_value #p_value that use compare

#conclusion
if(p_value<0.05){
  print("We have evidence to prove there is a different in the mean weight of chickens that are feed different chicken feeds")
}else{
  print("We don't have evidence to prove there is a different in the mean weight of chickens that are feed different chicken feeds")
}

#qqnorm to check condition
par(mfrow=c(3,2))
qqnorm(subset(chickendata,feed=='casein')$weight)
qqnorm(subset(chickendata,feed=='horsebean')$weight)
qqnorm(subset(chickendata,feed=='linseed')$weight)
qqnorm(subset(chickendata,feed=='meatmeal')$weight)
qqnorm(subset(chickendata,feed=='soybean')$weight)
qqnorm(subset(chickendata,feed=='sunflower')$weight)

aggregate(weight~feed,data=chickendata,FUN=sd) #calculate sd weight of each feed

aggregate(weight~feed,data=chickendata,FUN=mean) #calculate mean weight of each feed to compare

#load the library
library(car) 
#Levene's test
leveneTest(weight ~ feed, data = chickendata)

#multiple comparison by bonferroni methods
pairwise.t.test(chickendata$weight,chickendata$feed,p.adj='bonferroni') #Pairwise comparisons using t tests with pooled SD 

#multiple comparison by TurkeyHSD of Post Hoc test
TukeyHSD(Anova_result,conf.level=0.95)

#Kruskal-Wallis test
kruskal.test(weight ~ feed, data = chickendata)

