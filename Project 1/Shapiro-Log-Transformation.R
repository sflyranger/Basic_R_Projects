#Creating a box plot and density plot for Income Status and different responses#
library(readxl)
Income_PS <- read_excel("Downloads/Income_PS.xlsx")
View(Income_PS)

#Reshaping data and renaming columns.#
install.packages("reshape2")
New<- melt(Income_PS)
colnames(New)<-c("Income_Status","response")

#Box-plot and density plot.#
favstats(response ~ Income_Status, data=New)
densityplot(~response, groups=Income_Status, auto.key=TRUE, data=New)
bwplot(response ~ Income_Status, data=New)

#Shapiro's test for Normality of different income status.#
install.packages("nortest") 
require(nortest)
attach(New)
shapiro.test(response[Income_Status=="Low_income"])
shapiro.test(response[Income_Status=="High_income"])

#Log transform of variables.#
New = transform(New , logresp=log(response))
favstats(logresp ~ Income_Status, data=New)

#T-test for difference from zero.#
ttestlog= t.test(logresp ~ Income_Status, data=New)

#Calculating difference of median values.#
LowMedian=exp(7.361447)
HighMedian=exp(7.232187)
Difference=LowMedian-HighMedian

