#Project 2 Exercise 2
#Resource: https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings
library(car)

#Import data
data <- read.csv(file = '/Users/khaiphan/Downloads/Video_Games_Sales.csv', sep = ",",header = T, nrows = 100)
#find the head's number to get choosen data
names(data)
newdata <- data[,c(4,10)]
#eliminate line 1 to use ANOVA-One-way test
newdata <- newdata[-c(1),]
newdata

#Data cleaning
newdata <- na.omit(newdata)


#min and max of global sales
rng <- range(newdata$Global_Sales)
#min
rng[1]
#max
rng[2]
#range of global sales
rng[2] - rng[1]

#mean
mean(newdata$Global_Sales)
#median
median(newdata$Global_Sales)
#the standard deviation
sd(newdata$Global_Sales)
#the standard variance
var(newdata$Global_Sales)

#modify margines of boxplot
par(mai=c(2.0, 0.8, 0.8, 0.4))
#draw boxplot
boxplot(newdata$Global_Sales ~ newdata$Genre, las = 2, xlab = "", ylab = "Global Sales")
title(xlab = "Genre", line = 5);

#One-way ANOVA test
Anova_Result = aov(Global_Sales ~ Genre, data = newdata)
summary(Anova_Result) #Show Anova result

	#Step 1: Prepare the data 
	
	library(readr)
	#import data from file csv
	data <- read.csv("E:\\R\\Video_Games.csv", sep = ",", header = T, nrows = 100) 
	
	#find the head's number to get choosen data
	names(data)
	newdata <- data[,c(4,10)]
	#The question is whether there is a difference in average Global sales among game's genres or not.
	#So we have this Assumption:
	# H0: There is not a difference in average Global sales among game’s genres
	# H1: There is a difference in average Global sales among game’s genres
	
	#eliminate line 1 to use ANOVA-One-way test
	newdata <- newdata[-c(1),]
	#Data cleaning
	newdata <- na.omit(newdata)
	newdata	
	
	#Step 2: Do One-way Anova test
	Anova_Result = aov(newdata$Global_Sales ~ newdata$Genre, data = newdata)
	summary(Anova_Result) #Show Anova result
	
	
	#Get F_value from Anova_Result
	F_Value_Genre <- (summary(Anova_Result))[[1]][["F value"]]
	F_value <- F_Value_Genre[1]
	F_value #F_value that used to compare
	
	df <- (summary(Anova_Result))[[1]][["Df"]] #get df from Anova_Result
	df_b = df[1] #df between
	df_w = df[2] #df within
	df_b
	df_w

	F_critical = qf(1-alpha, df_b, df_w) #search F_critical from F distribution
	F_critical
	
	#Step 3: Conclusion
	if(F_value >= F_critical) {
		print("At alpha = 0.05, we have enough evidence to conclude that")
		print(" there is a difference between Global_sales in different Game Genre")
	}else{
		print("At alpha = 0.05, We do not have enough evidence to conclude that")
		print(" there is a difference between Global_sales in different Game Genre")	
	}



#Multiple pair comparison
TukeyHSD(Anova_Result)

#Check validity of ANOVA test assumption
leveneTest(Global_Sales ~ Genre, data = newdata)
plot(Anova_Result, 2)

#Kruskal-Wallis test
kruskal.test(Global_Sales ~ Genre, data = newdata)

#Linear regression model:
	###Step 1: Prepare the data
	#import the data
	
	data <- read.csv("E:\\R\\Video_Games.csv", sep = ",", header = T)
	names(data) #to see what variable we want to use 
	
	linear_data = head(data[, c(3, 10)], 100) 
	#independent variable is Year_of_Release.
	#dependent variable is Global_Sales.

	#convert Year into numeric
	linear_data$Year_of_Release <- as.integer(linear_data$Year_of_Release)
	linear_data
	
	###Step 2: do the linear model
	plot(linear_data$Year_of_Release, linear_data$Global_Sales)
	abline(lm(linear_data$Global_Sales ~ linear_data$Year_of_Release, data = linear_data))
	linearMod <- lm(linear_data$Global_Sales ~ linear_data$Year_of_Release, data = linear_data)
	linearMod
	#Check the model
	summary(linearMod)