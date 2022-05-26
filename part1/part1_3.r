#Exercise 3-Topic 5 (Chi-square)

#H0 : the age and the income level are not related
#H1 : the age and the income level are related

#Load base packages:
library(datasets)

#Step I. Prepare data for Chi-Square test:
data <- matrix(c(6,18,11,9,19,12,5,8,17), nrow = 3)
rownames(data) <- c("Under 40", "40-54", "Over 54")
colnames(data) <- c("Under $100000", "$100000-$400000", "Over $400000")
data #print out the table

#Step II. Do Chi-square test:
chisq.test(data)
#X-squared of test = 6.8549 
#df = (num_row-1)*(num_column-1) = (3-1)*(3-1) = 4
x2_table = qchisq(0.99, df = 4)
x2
#X-squared = 13.2767 with alpha 0.01 and df = 4 searched from Chi-squared distribution table.

#Step III. Conclusion:

x2 <- (chisq.test(data))[[1]][["X-squared"]] #get chi-squared value
#Because X-square of test = 6.8549 < 13.2767, so H0 is not rejected.
if(x2 >= x2_table) {
	print("At alpha = 0.01, we have enough evidence to confirm that the age and the income level are related")
}else {
	print("At alpha = 0.01, we do not have enough evidence to confirm that the age and the income level are related")
}

