options(scipen=999)

### librarries to be loaded are as follows
library(caret)
library(lmtest)
library(MKmisc)
library(survey)
library(ROCR)
library(aod)
library(Rcpp)
library(eeptools)
library(reshape2)
library(stringr)
library(scales)
library(ggplot2)
library(corrplot)
library(pscl)
library(Epi)
library(rpart)
library(tree)

##########################################################################

### current work directory
getwd()

### set work directory
setwd("E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model")

### importing the raw data
rawdata <- read.csv("cs-training.csv", header=TRUE, sep=",")

### removing missing data
loandata <- subset(subset(rawdata,Naflag==0),Debtflag==0)
loandata <- subset(loandata,select = -c(1,13:14))

### names of the data variables
names(loandata)

### channging the names of specific columns, they are very big
colnames(loandata)[4] <- "Num30to59DaysPastDue"
colnames(loandata)[8] <- "Num90DaysPastDue"
colnames(loandata)[10] <- "Num60to89DaysPastDue"

### changind the data format to factor
loandata$SeriousDlqin2yrs <- factor(loandata$SeriousDlqin2yrs)

### attach the data
attach(loandata)

### data types of the input data's columns
sapply(loandata, class)
table(sapply(loandata, class))

### summarize the input dataset
summary(loandata)

######################### Data preparation   #############################
######################## Univariate analysis #################

################# 01. frquency plot of the loans by their status #########
### overall default rate
default <- as.data.frame(table(SeriousDlqin2yrs))
default$col_pct <- default$Freq/sum(default$Freq)

### exporting the results to a csv file
write.table(default, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/01.Default_rate.csv")

### barplot of the freq
ggplot(default, aes(SeriousDlqin2yrs,Freq, fill=SeriousDlqin2yrs)) + scale_y_continuous(limits=c(0,125000))+
  geom_bar(stat="identity") + labs(title="Loan Default rate", x="Loan status", y="% of loans") +
  geom_text(aes(label=paste(round(col_pct*100,digits=1),"%",sep="")),position="identity",vjust = -0.25, hjust=0) + 
  theme(legend.position="bottom") +   scale_fill_discrete("Legend",labels=c("0=Active loans","1=Default loans"))

################# 02. Revolving credit utilization #################
### percentile distribution
pct_dist_creditbalance <- 
  as.data.frame(quantile(
    RevolvingUtilizationOfUnsecuredLines, 
    c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,.997,.998,.999,1)))

colnames(pct_dist_creditbalance) = "Revolving_Utilization_of_Credit"

pct_dist_creditbalance$Revolving_Utilization_of_Credit <- 
  paste(round(pct_dist_creditbalance$Revolving_Utilization_of_Credit*100,digits=1),"%",sep="")

### the difference between the 99.7th and 99.99th percentile is too big
### it is skewing the observations
### capping the maximum credit utlitization % @ 99.7th percentile
loandata$RevolvingUtilizationOfUnsecuredLines <- 
  ifelse(loandata$RevolvingUtilizationOfUnsecuredLines > 1.708416,1.708416,loandata$RevolvingUtilizationOfUnsecuredLines)

attach(loandata)

### capped percentile distribution
pct_dist_revolving_credit_cap <- 
  as.data.frame(quantile(
    RevolvingUtilizationOfUnsecuredLines, 
    c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,.997,.998,.999,1)))

colnames(pct_dist_revolving_credit_cap) = "Revolving_Utilization_of_Credit"

pct_dist_revolving_credit_cap$Revolving_Utilization_of_Credit <- 
  paste(round(pct_dist_revolving_credit_cap$Revolving_Utilization_of_Credit*100,digits=1),"%",sep="")

write.table(pct_dist_creditbalance, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/02.Pct_dist_Revovling_credit.csv")

################# 03. age of the borrowers #################
### histogram of the age of the borrowers

ggplot(loandata, aes(age, fill=as.factor(SeriousDlqin2yrs))) + scale_y_continuous(limits=c(0,20000))+
  geom_histogram(binwidth=5) + labs(title="Age distribution by loan status", x="Age of borrowers") +
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

### creating age buckets
bins_age <- seq(20,120,by=20)

### counting the number of loans under each age bracket by deliquency
default_age <- as.data.frame(table(cut(age,bins_age), SeriousDlqin2yrs))
colnames(default_age) <- c("Age_bracket","Default","Num_of_Borrowers")

### using regular expression to replace the , with -
### replacing multiple strings is not possible with gsub
default_age$Age_bracket <- gsub(",", "-",default_age$Age_bracket)

write.table(default_age, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/03.Default_rate_by_age.csv")

# 100% stacked bar plot by age and loan status
ggplot(default_age,aes(x = Age_bracket, y = Num_of_Borrowers,fill = Default)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())

ggplot(default_age,aes(x = Age_bracket, y = Num_of_Borrowers,fill = Default)) + scale_y_continuous(labels = percent_format()) +
  geom_bar(stat="identity",position="fill") + labs(title="Loan Default rate by age", x="Age bracket", y="% of borrowers") +
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

################# 04. Num30to59DaysPastDue #################

summary(Num30to59DaysPastDue)

bins_30to59days <- seq(0,100,by=10)
Default_by_30to59Days <- 
  table(cut(Num30to59DaysPastDue,bins_30to59days), SeriousDlqin2yrs)

print(Default_by_30to59Days)
# 30 to 59 days past due doesn't appear to be a good predictor

################# 05. Debt ratio #################

summary(DebtRatio)

### percentile distribution of debt-ratio
pct_dist_debtratio <-
  as.data.frame(quantile(DebtRatio, c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,0.997,0.998,0.999,1)))

colnames(pct_dist_debtratio) = "DebtRatio"

pct_dist_debtratio$DebtRatio <- 
  paste(round(pct_dist_debtratio$DebtRatio*100,digits=1),"%",sep="")

### capping the maximum debt ratio % @ 99.7th percentile i.e 3-sigma
loandata$DebtRatio <- 
  ifelse(loandata$DebtRatio > 2.752635,2.752635,loandata$DebtRatio)

attach(loandata)

write.table(pct_dist_debtratio, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/05.pct_dist_debtratio.csv")

### average debt-ratio by deliquency
tapply(DebtRatio, SeriousDlqin2yrs, FUN = mean)

################# 06. Monthly income #################

summary(MonthlyIncome)

hist(MonthlyIncome, main="Distribution of loans by monthly income of borrowers", col="green",
     xlab="MOnthly income", ylab="Num. of people", ylim=c(0,120000))

### percentile distribution of the income
pct_dist_income <- 
  as.data.frame(quantile(MonthlyIncome, c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,.997,.998,.999,1)))

colnames(pct_dist_income) = "MonthlyIncome"

### capping the maximum debt ratio % @ 99.7th percentile
loandata$MonthlyIncome <- 
  ifelse(loandata$MonthlyIncome > 50000,50000,loandata$MonthlyIncome)

attach(loandata)

write.table(pct_dist_income, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/06.pct_dist_income.csv")

### counting the number of loans under each income bracket by deliquency
default_income <- as.data.frame(table(
  cut(MonthlyIncome,
      c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,100000,500000)),
  SeriousDlqin2yrs))

colnames(default_income) <- c("Income_bracket","Default","Num_of_Borrowers")

write.table(default_income, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/06.Default_rate_by_income.csv")

### 100% stacked bar plot by monthly income and loan status
ggplot(default_income,aes(x = Income_bracket, y = Num_of_Borrowers,fill = Default)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())


################# 07. Number of credit lines open #################

summary(NumberOfOpenCreditLinesAndLoans)

hist(NumberOfOpenCreditLinesAndLoans, 
     main="Distribution of borrowers by number of credit lines", 
     xlab="Num. of credit line", col="green")

### average number of open credit lines by loan status
tapply(NumberOfOpenCreditLinesAndLoans, SeriousDlqin2yrs, FUN = mean)

pct_dist_creditlines <-
  as.data.frame(quantile(
    NumberOfOpenCreditLinesAndLoans, 
    c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,0.997,0.998,0.999,1)))

colnames(pct_dist_creditlines) = "Num_of_Credit_Lines"

default_num_creditlines <- as.data.frame(table(
  cut(NumberOfOpenCreditLinesAndLoans,
      c(0,3,6,8,11,16,18,20,100)),
  SeriousDlqin2yrs))

colnames(default_num_creditlines) <- c("NumCreditLines","Default","Num_of_Borrowers")

### 100% stacked bar plot by number of credit lines and loan status
ggplot(default_num_creditlines,aes(x = NumCreditLines, y = Num_of_Borrowers,fill = Default)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())

write.table(default_num_creditlines, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/07.Default_rate_num_creditlines.csv")

################# 09. Number of mortgage and real estate loans #################

summary(NumberRealEstateLoansOrLines)

hist(NumberRealEstateLoansOrLines, 
     main="Distribution of borrowers by property loans", 
     xlab="Num. of credit line", col="green")

### average number of property loans or mortgages
tapply(NumberRealEstateLoansOrLines, SeriousDlqin2yrs, FUN = mean)

pct_dist_propertyloans <-
  as.data.frame(quantile(
    NumberRealEstateLoansOrLines, 
    c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,0.997,0.998,0.999,1)))

colnames(pct_dist_propertyloans) = "Num_of_Property_loan"

default_num_propertyloans <- as.data.frame(table(
  cut(NumberRealEstateLoansOrLines,
      c(0,1,2,3,4,5,10,60)),
  SeriousDlqin2yrs))

colnames(default_num_propertyloans) <- c("NumPropertyLoans","Default","Num_of_Borrowers")

### 100% stacked bar plot by number of property loans and loan status
ggplot(default_num_propertyloans,aes(x = NumPropertyLoans, y = Num_of_Borrowers,fill = Default)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())

write.table(default_num_propertyloans, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/09.Default_rate_num_propertyloans.csv")


################# 11. Number of Dependents #################

summary(NumberOfDependents)

### histogram
ggplot(loandata, aes(NumberOfDependents, fill=as.factor(SeriousDlqin2yrs))) + scale_y_continuous(limits=c(0,100000))+
  geom_histogram(binwidth=1) + labs(title="Num. of dependents by loan status", x="Num. of dependents") +
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

default_dependents <- as.data.frame(table(NumberOfDependents, by=SeriousDlqin2yrs))

colnames(default_dependents) <- c("Num_of_dependents","Default","Num_of_Borrowers")

ggplot(default_dependents,aes(x = Num_of_dependents, y = Num_of_Borrowers,fill = Default)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())

write.table(default_dependents, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/11.Default_rate_by_dependents.csv")

### to be sure that any changes made to the data is updated, re-attach the data
attach(loandata)

######################## Bivariate analysis #################

##### conditional density plot of loan status and other predictors
cdplot(as.factor(SeriousDlqin2yrs) ~ RevolvingUtilizationOfUnsecuredLines,
       col=c(3,2),ylab = "Loan status", xlab = "Revolving credit usage")
cdplot(as.factor(SeriousDlqin2yrs) ~ age,col=c(3,2),
       ylab = "Loan status", xlab = "Age of borrower")
cdplot(as.factor(SeriousDlqin2yrs) ~ DebtRatio,col=c(3,2),
       ylab = "Loan status", xlab = "Debt to Income ratio")
cdplot(as.factor(SeriousDlqin2yrs) ~ MonthlyIncome,col=c(3,2),
       ylab = "Loan status", xlab = "Monthly Income")
cdplot(as.factor(SeriousDlqin2yrs) ~ NumberOfOpenCreditLinesAndLoans,
       col=c(3,2),ylab = "Loan status", xlab = "Num. of open credit lines")

#pairs(~Num30to59DaysPastDue + Num60to89DaysPastDue + Num90DaysPastDue + MonthlyIncome + DebtRatio)

### correlation matrix and plot
corrmatrix <- cor(subset(loandata,select = -1))
colnames(corrmatrix)= c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")
corrplot(corrmatrix, method = "circle")

### scatter plot

# plot(MonthlyIncome, DebtRatio, col="blue", pch=20)
# # first degree
# fit1 <- lm(DebtRatio ~ MonthlyIncome)
# # second degree
# fit2 <- lm(DebtRatio ~ poly(MonthlyIncome,2,raw = TRUE))
# # third degree
# fit3 <- lm(DebtRatio ~ poly(MonthlyIncome,3,raw = TRUE))
# 
# lines(MonthlyIncome, predict(fit1, data.frame(x=MonthlyIncome)), col="red")
# lines(MonthlyIncome, predict(fit2, data.frame(x=MonthlyIncome)), col="green")
# lines(MonthlyIncome, predict(fit3, data.frame(x=MonthlyIncome)), col="black")
# 
# logestimate <- lm(DebtRatio ~ log(MonthlyIncome) )
# 
# plot(MonthlyIncome,predict(fit1),type='l',col='blue')
# lines(MonthlyIncome,predict(logestimate),col='red')
# 
# ggplot(default, aes(SeriousDlqin2yrs,Freq, fill=SeriousDlqin2yrs)) + scale_y_continuous(limits=c(0,125000))+
#   geom_bar(stat="identity") + labs(title="Loan Default rate", x="Loan status", y="% of loans") +
#   geom_text(aes(label=paste(round(col_pct*100,digits=1),"%",sep="")),position="identity",vjust = -0.25, hjust=0) + 
#   theme(legend.position="bottom") +   scale_fill_discrete("Legend",labels=c("0=Active loans","1=Default loans"))

### income vs age
ggplot(loandata, aes(age,MonthlyIncome,fill=as.factor(SeriousDlqin2yrs))) + 
  geom_point() + geom_smooth(method="auto")

ggplot(loandata, aes(NumberOfOpenCreditLinesAndLoans,DebtRatio, fill=as.factor(SeriousDlqin2yrs))) + 
  geom_point() + geom_smooth(method="auto")

### debt ratio vs property loans
ggplot(loandata, aes(NumberRealEstateLoansOrLines,DebtRatio, 
                     color=as.factor(SeriousDlqin2yrs))) + 
  geom_point() + geom_smooth(method="auto")

### Age vs revolving credit utilization
ggplot(loandata, aes(age,RevolvingUtilizationOfUnsecuredLines,
                     fill=as.factor(SeriousDlqin2yrs),color=as.factor(SeriousDlqin2yrs))) + 
  geom_point() + geom_smooth(method="auto") +
  labs(title="Age vs Credit usage", x="Age", y="Revolving credit utilization") +
  theme(legend.position="bottom")

#####################################################################################
################  logistics regression  ##################

### partitioning the data into training and testing dataset

inputdata <- loandata
Train <- createDataPartition(inputdata$SeriousDlqin2yrs, p=0.5, list=FALSE)
training <- inputdata[ Train, ]
testing <- inputdata[ -Train, ]

training$SeriousDlqin2yrs <- factor(training$SeriousDlqin2yrs)
testing$SeriousDlqin2yrs <- factor(testing$SeriousDlqin2yrs)

### model building

mlogit_full <- glm(SeriousDlqin2yrs~., family=binomial(link="logit"), data=training)
mlogit_AIC_bwd <- step(mlogit_full, direction="backward", data=training)

### checking if forward selection yields the same result
mlogit_null <- glm(SeriousDlqin2yrs~1, family=binomial(link="logit"), data=training)
mlogit_AIC_fwd <- step(mlogit_null, scope=list(lower=formula(mlogit_null),upper=formula(mlogit_full)),direction="forward")

summary(mlogit_AIC_bwd)

### removing Number of real estate loans or lines
mlogit <- glm(SeriousDlqin2yrs~.-NumberRealEstateLoansOrLines, family=binomial(link="logit"), data=training)

### test to check if the difference in the models upon removing the 
### property loan variable is significant

# goodness of fit
anova(mlogit_full, mlogit, test ="Chisq")
# Likelihood ratio test
lrtest(mlogit_full, mlogit)

### the difference in models is significant hence we will proceed with the reduced

summary(mlogit)
#AIC: 24620
BIC(mlogit)
#BIC: 24709
deviance(mlogit)
#deviance: 24599

anova(mlogit, test='Chisq')
pR2(mlogit)
#McFadden R2: 0.16825

### getting the odds ratio of the individual predictor
exp(coefficients(mlogit))
# direction of odd-ratio are logically intuitive and are explainalable

varImp(mlogit)

### HL test
HLgof.test(fit = fitted(mlogit), obs = training$SeriousDlqin2yrs)

### wald.test
wald.test(b = coef(mlogit), Sigma = vcov(mlogit), Terms=2:10)
### model does not fit the data well

# ROC curve
prob <- predict(mlogit, newdata=testing, type="response")
pred <- prediction(prob, testing$SeriousDlqin2yrs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col="blue")
abline(a=0,b=1,col="red")

auc <- performance(pred, measure = "auc")
auc
# AUC = 0.7942786

### cut-off chart & optimal point
rc <- ROC(form = formula(mlogit), plot="sp")
opt <- which.max(rowSums(rc$res[, c("sens", "spec")]))
rc$res$lr.eta[opt]
#0.06317427

# test accuracy
fitted_results_test <- predict(mlogit,newdata=testing, type='response')
fitted_results_test <- ifelse(fitted_results_test > 0.06317427,1,0)

error_test <- mean(fitted_results_test != testing$SeriousDlqin2yrs)
print(paste('Accuracy',1-error_test))
#Accuracy 0.722099447513812



### Rdata file
save.image (file = "Pazhamalai_Yuvaraj_02292016.Rdata")

#####################################################################################
################  CART  ##################

### using tree package
rt1 <- tree(factor(SeriousDlqin2yrs)~age, data=training)
rt1
summary(rt1)
plot(rt1)
text(rt1)

rt2 <- tree(factor(SeriousDlqin2yrs)~., data=training, control=tree.control(57921,mindev=.005))
plot(prune.tree(rt2))
abline(v=7,col="red")
plot(rt2)
text(rt2)
summary(rt2)
rt2

rt2.cv <- cv.tree(rt2)
plot(rt2.cv)
rt2.prune <- prune.tree(rt2,best=5)

plot(rt2.prune)
text(rt2.prune)
summary(rt2.prune)
rt2.prune
