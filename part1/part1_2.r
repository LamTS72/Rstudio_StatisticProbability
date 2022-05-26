# load some library for further analysis
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

#load the data into sales.data
sales.data <- read.csv("C:/Users/Admin/Desktop/2.csv", 
                       header = TRUE, 
                       colClasses = c("factor", "factor", "numeric"))

summary(sales.data)
two.way = aov(sales~districts * industries , data = sales.data)
summary(two.way)
dependence = aov(sales~districts * industries , data = sales.data)
summary (dependence)

tukey<-TukeyHSD(two.way)
tukey

qqnorm(sales.data$sales)
qqline(sales.data$sales , col = 'blue')


# R program to illustrate
# Levene's test
# Import required package
library(car)

# Using leveneTest()
result = leveneTest(sales ~ interaction(districts, industries), 
                        +                     data = sales.data)

# print the result
print(result)
