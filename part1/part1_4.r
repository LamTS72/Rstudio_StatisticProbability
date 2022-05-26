#Analysis of Variance (ANOVA)


#H0 :assume that the amount of newspaper sold is the same in 5 districts
#H1 :the amount of newspaper sold is at least different in 5 districts
District1<-c(22,21,25,24,28,30) #input the number of newspaper follows district group
District2<-c(18,18,25,24,19,22)
District3<-c(22,22,25,18,15,28)
District4<-c(18,18,19,20,22,25)
District5<-c(18,19,20,22,25,25)
#combines data into table
Combined_Districts<-data.frame(cbind(District1,District2,District3,District4,District5))
Combined_Districts #show table
summary(Combined_Districts) #show min, median, mean, max.

Stacked_District<-stack(Combined_Districts) #push value to stack
Stacked_District #show table of Stacked_District

Anova_Result<-aov(values~ind, data=Stacked_District) #handle data 
summary(Anova_Result) #show Anova_Result

F_valueDistrict=summary(Anova_Result)[[1]][["F value"]] #get F_value from table Anova_Result
F_value=F_valueDistrict[1]
F_value #F_value that use compare

df=summary(Anova_Result)[[1]][["Df"]] #get df from table Anova_Result1
df_b=df[1] #df between
df_w=df[2]  #df within

df_b
df_w
F_critical=qf(1-0.01,df_b,df_w) #search F_critical from F distribution table
F_critical

#conclusion
if(F_value > F_critical){
  print("We reject H0,so there is a difference in the amount of newspapers sold in 5 districts")
}else{
  print("We fail to reject H0,so there is no difference in the amount of newspapers sold in 5 districts")
}

#H0: assume that the amount of newspaper sold is not effected by days of weeks
#H1: the amount of newspaper sold is effected by days of weeks

Monday<-c(22,18,22,18,18) #input the number of newspaper follows day group
Tuesday<-c(21,18,22,18,19)
Wednesday<-c(25,25,25,19,20)
Thursday<-c(24,24,18,20,22)
Friday<-c(28,19,15,22,25)
Saturday<-c(30,22,28,25,25)

Combined_Days<-data.frame(cbind(Monday,Tuesday,Wednesday,Thursday,Friday,Saturday))
Combined_Days #show table
summary(Combined_Days) #show min, median, mean, max.

Stacked_Day<-stack(Combined_Days) #push value to stack
Stacked_Day #show table of Stacked_Day

Anova_Result1<-aov(values~ind, data=Stacked_Day) #handle data 
summary(Anova_Result1) #show Anova_Result


F_valDay=summary(Anova_Result1)[[1]][["F value"]] #get F_value from table Anova_Result1
F_valDay #F_value that use compare

df=summary(Anova_Result1)[[1]][["Df"]] #get df from table Anova_Result1
df_b=df[1] #df between
df_w=df[2]  #df within

df_b
df_w
F_critDay=qf(1-0.01,df_b,df_w) #search F_critical from F distribution table
F_critDay
#conclusion
if(F_valDay>F_critDay){
  print("We reject H0,so the amount of newspapers sold is effected by days of week")
}else{
  print("We fail to reject H0,so the amount of newspapers sold is not effected by days of week")
}


