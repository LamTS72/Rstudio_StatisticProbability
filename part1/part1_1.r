#Exercise 1

#H0 : The amount of milk produced by these types of cows is not different
#H1 : The amount of milk produced by these types of cows is different
#Load base packages:
library(datasets)

#Data Input
data <- c(92,37,46,53,15,19,75,19,12)
#classify data
types <- factor(rep(c("A", "B", "C"),each = 3))
amount <- factor(rep(c("Litle", "Medium", "Much"),each = 3))

#result
result = aov (data ~ types + amount)
summary(result)

#draw the boxplot relative to types of cow
boxplot(data ~ types)

TukeyHSD(result)
