#Read file#
library(readxl)
Auto_file <- read_excel("Downloads/Auto_file.xlsx")
View(Auto_file)
# Variable Creation#
Y<-Auto_file$MPG; Y
X1<- Auto_file$Weight; X1
X2<- Auto_file$Cyl; X2
X3<- Auto_file$HP; X3
X4<- Auto_file$Foreg; X4
#Linear Regression for weight only.#
OLS<-lm(Y ~ X1) # lm: Linear Model
summary(OLS)
#Linear Regression for all variables.#
OLS2<- lm(Y ~ X1+X2+X3+X4) #lm for all variables
summary(OLS2)
