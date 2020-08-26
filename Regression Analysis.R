#install packages
library(readr)
library(dplyr)
library(ggplot2)
#read in data
test.raw <- read_csv('CandidateSummaryAction1B.csv')
#packages for missingness map
library(Amelia)
library(mlbench)
#missingness chart for visualizing missing data points
missmap(test.raw)
#try again with importing, now with NA in blanks to replace later
test.raw <- read.csv('CandidateSummaryAction1B.csv',header=T,na.strings=c(""))
#validated that it was the same
missmap(test.raw)
#delete non-independent variables
test.raw[c(1,2,3,6,7,9,10,13,15,17,18)] <- list(NULL)
#replace NA with 0
test.raw[is.na(test.raw)]=0
#check to make sure nothing is missing
missmap(test.raw)
#check variable formats
sapply(test.raw, class)
#have to convert characters to factors to get categorical variables inputted into the regression
cols <- c("can_par_aff", "can_inc_cha_ope_sea", "winner")
test.raw[cols] <- lapply(test.raw[cols], factor)
sapply(test.raw, class)
# Check categorical variables encoding for better understanding of the fitted model
contrasts(test.raw$can_par_aff)
#partition test and training data
train <- test.raw[1:1444,]
test <- test.raw[1445:1805,]
#fit into lr model. specify binomial
model <- glm(winner ~.,family=binomial(link='logit'),data=train)
summary (model)
#anova
anova(model,test="Chisq")
#predict winners. threshold probability of .5
test.results <- predict(model,newdata=test,type='response')
test.results <- ifelse(test.results > 0.5,1,0)
misClasificError <- mean(test.results != test$winner)
print(paste('Accuracy',1-misClasificError))
# ROC and AUC
install.packages("ROCR")
library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$winner)
# TPR = sensitivity, FPR=specificity
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,main = "ROC Plot",xlab = "False Positive Rate",ylab = "True Positive Rate")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc