
library(caret)
library(lmtest)
library(MKmisc)
library(survey)
library(ROCR)
library(aod)
library(Rcpp)

# current work directory
getwd()

# set work directory
setwd("C:/Users/User/Desktop/Protiviti")

# read the data
rawdata <- read.csv("train_data.csv", header=TRUE, sep=",")

# missing values 
missing <- rawdata[!complete.cases(rawdata),]

# creating new dataset without missing data 
cleandata <- na.omit(rawdata)

# using 2/3rd of the data for training and rest for testing the model
train <- cleandata[sample(nrow(cleandata),6595),]
test <- cleandata[-train,]
# i tried the above code, but it was throwing an error. I couldn't figure
# out the problem. So i sampled in SAS and then imported the dataset

train <- read.csv("train.csv", header=TRUE, sep=",")
test <- read.csv("test.csv", header=TRUE, sep=",")

attach(train)
names(train)
sapply(train, class)

# glm model with logit link function
model1 <- glm(y~x1,family=binomial(link="logit"))
summary(model1)
BIC(model1)

model2 <- glm(y~x1+x2,family=binomial(link="logit"))
summary(model2)
BIC(model2)

names(model1)

# getting the odds ratio of the individual predictor
exp(coefficients(model1))
exp(coefficients(model2))

# goodness of fit
anova(model1, model2, test ="Chisq")

# Likelihood ratio test
lrtest(model1, model2)

# both the chi-square test and likelihood state that the difference between
# the full and reduced model is significant

final_model <- model2
summary(final_model)
BIC(final_model)
coefficients(final_model)
confint(final_model)

# HL test
HLgof.test(fit = fitted(final_model), obs = train$y)
# H0: model fits the data
# Conclude null hypothesis

# wald test
regTermTest(final_model, "x1")
regTermTest(final_model, "x2")

# h0: parameter estimate = 0
# based on the above wald test we reject the ho for both the predictors

# we can also use the wald.test fn. Here the h0 is the inverse of above
wald.test(b = coef(final_model), Sigma = vcov(final_model), Terms=2:3)

# cut-off chart
cutoff_fn = function(cut, final_model, y)
{
  yhat = (final_model$fitted.values>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  out = t(as.matrix(c(sensitivity, specificity)))
  colnames(out) = c("sensitivity", "specificity")
  return(out)
}

s = seq(.1,1,length=100)
OUT = matrix(0,100,2)
for(i in 1:100) OUT[i,]=cutoff_fn(s[i],final_model,y)
plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
legend(0.75,0.5,col=c("darkgreen","darkred"),lwd=c(2,2),c("Sensitivity","Specificity"))

diff <- OUT[,1] - OUT[,2]
classification_table <- as.data.frame(cbind(s,OUT,diff))
colnames(classification_table) = c("Probability","Sensitivity","Specificity","Difference")

# optimal cut-off: 0.3636364

## finding the accuracy of predictions in the training set
fitted_results_train <- predict(final_model,type='response')
fitted_results_train <- ifelse(fitted_results_train > 0.3636364,1,0)

error <- mean(fitted_results_train != train$y)
print(paste('Accuracy',1-error))

# Accuracy 0.687338893100834

# ROC curve
prob <- predict(final_model, newdata=test, type="response")
pred <- prediction(prob, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col="blue")
abline(a=0,b=1,col="red")

auc <- performance(pred, measure = "auc")
auc
# AUC = 0.782952

# test accuracy
fitted_results_test <- predict(final_model,newdata=test, type='response')
fitted_results_test <- ifelse(fitted_results_test > 0.3636364,1,0)

error_test <- mean(fitted_results_test != test$y)
print(paste('Accuracy',1-error_test))

# Accuracy 0.7116434202547
## accuracy of the predictions in the test set is greater than in the training

# Using this model to predict the outcome for the test_data
testing_data <- read.csv("test_data.csv", header=TRUE, sep=",")
testing_pred <- predict(final_model,newdata=testing_data, type='response')
testing_pred <- as.data.frame(cbind(testing_data,ifelse(testing_pred > 0.3636364,1,0)))

write.table(testing_pred, sep=",", "C:/Users/User/Desktop/Protiviti/test_data_pred.csv")

save.image (file = "Yuvaraj_Logistic_Regression.Rdata")
