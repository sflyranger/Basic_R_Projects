install.packages("psych") 
#Data Set import for LaQuinta Inns operating margins. #
library(readxl)
Variable_sel <- read_excel("Downloads/Variable_sel.xlsx")
View(Variable_sel)
attach(Variable_sel)
Margin_full <- lm(Margin ~ ., data=Variable_sel)
summary(Margin_full) 
#Running a backward selection process to pick best model on this criteria (AIC). #
Margin_back <- step(Margin_full, direction= "backward")
# In the output we can see two iterations where the AIC at first was 348.13. The variable Distance was then removed from the model and the AIC lowered to 347.83. #
#Forward selection to compare.#
Margin_base <- lm(Margin~ 1, data= Variable_sel)
Margin_forw <- step(Margin_base, scope= Margin ~ Number + Nearest + Space + Income + Enrollment + Distance, direction= "forward")
#Stepwise selection to confirm #
Margin_step <- step(Margin_base, scope= Margin ~ Number + Nearest + Space + Income + Enrollment + Distance, direction = "both")
# From the output we can see that all three of these selections procedures came up with the best model having excluded Distance and included all other variables for an AIC of 347.83. #


