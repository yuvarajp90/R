options(scipen=999)

### libraries to be loaded (please run them twice)
library(lmtest)
library(MKmisc)
library(survey)
library(ROCR)
library(pROC)
library(aod)
library(Rcpp)
library(eeptools)
library(reshape2)
# library(stringr)
library(scales)
library(ggplot2)
library(corrplot)
library(rpart)
library(randomForest)
# library(caret)
library(e1071)
library(ipred)
library(earth)
library(kernlab)
# library(xgboost)
library(plyr)
library(dplyr)
library(gbm)
# library('rJava')
# library("bartMachine") 

##########################################################################

### current work directory
getwd()

### set work directory
setwd("E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model")

### importing the raw data (there should be 150K records and 11 columns)
data <- read.csv("cs-training.csv", header=TRUE, sep=",")
table(data$SeriousDlqin2yrs)
# there are 10,026 Y or 1's in the raw data (response to be predicted)

### checking for missing values
check1 <- na.omit(data)
table(check1$SeriousDlqin2yrs)
rm(check1)
# number of 1's has reduced to 8,357
# if all the rows with "NA" are removed 20% of the total data & 15% of the loan defaults
# would be excluded. So instead will replace columns with NA with median value

### names of the data variables
names(data)

### channging the names of specific columns, they are very big
colnames(data)[4] <- "Num30to59DaysPastDue"
colnames(data)[8] <- "Num90DaysPastDue"
colnames(data)[10] <- "Num60to89DaysPastDue"

### changing the data format of the response variable to factor
data$SeriousDlqin2yrs <- factor(data$SeriousDlqin2yrs)

######################### Data preparation & checks   #############################

# missing value treatment
# 96 and 98 signify missing values. 
# Hence first converting them to NA's and then replacing them with the median values
data$Num30to59DaysPastDue[data$Num30to59DaysPastDue >= 96] <- NA
data$Num60to89DaysPastDue[data$Num60to89DaysPastDue >= 96] <- NA
data$Num90DaysPastDue[data$Num90DaysPastDue >= 96] <- NA

data$Num30to59DaysPastDue[is.na(data$Num30to59DaysPastDue)] <- median(data$Num30to59DaysPastDue,na.rm=TRUE)
data$Num60to89DaysPastDue[is.na(data$Num60to89DaysPastDue)] <- median(data$Num60to89DaysPastDue,na.rm=TRUE)
data$Num90DaysPastDue[is.na(data$Num90DaysPastDue)] <- median(data$Num90DaysPastDue,na.rm=TRUE)

data$NumberOfDependents[is.na(data$NumberOfDependents)] <- median(data$NumberOfDependents,na.rm=TRUE)

# instead of replacing the missing values for monthly income with mean/median
# I have coded them differently, so that the model will understand that these values
# are different. Didnt replace them since changing 20% of the data will introduce bias
data$MonthlyIncome[is.na(data$MonthlyIncome)] <- -1

### checking for missing values again after the above changes to be sure
check2 <- na.omit(data)
table(check2$SeriousDlqin2yrs)
rm(check2)

### data types of the input data's columns
table(sapply(data, class))

### summarize the input dataset
summary(data)

attach(data)

# Intead of running the EDA, to jum to the model part directly
# please search for the string "Model part starts from here"

######################## EDA #########################

################# 01. frquency plot of the loans by their status #########
### overall default rate
default <- as.data.frame(table(SeriousDlqin2yrs))
default$col_pct <- default$Freq/sum(default$Freq)

### exporting the results to a csv file
write.table(default, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/01.Default_rate.csv")

### barplot of the freq
ggplot(default, aes(SeriousDlqin2yrs,Freq, fill=SeriousDlqin2yrs)) + scale_y_continuous(limits=c(0,150000))+
  geom_bar(stat="identity") + labs(title="Loan Default rate", x="Loan status", y="% of loans") +
  geom_text(aes(label=paste(round(col_pct*100,digits=1),"%",sep="")),position="identity",vjust = -0.25, hjust=0) + 
  theme(legend.position="bottom") +   scale_fill_discrete("Legend",labels=c("0=Active loans","1=Default loans"))

# current ratio of defaults to non-defaults is 10,026 to 139,974
# The current ratio of defaults to non-defaults is 1:14
# This is much higher than the actual loan default rates
# Hence we will keep the all of the default loans but downsize the non-defaults to an acceptable ratio
# Current industry standards is <10% but during the 2008 recession, default was as high as 10-12%
# so i will maintain a 1:9 ratio

# sampling from the non-defaults to achieve the aforementioned ratio
default_loans <- subset(data,SeriousDlqin2yrs==1)
nondefault_loans <- subset(subset(data,(SeriousDlqin2yrs==0)),MonthlyIncome != -1)

set.seed(12345)
nondefault_loans_f <- nondefault_loans[sample(nrow(nondefault_loans), 90234),]

loandata <- rbind(default_loans, nondefault_loans_f)
loandata <- loandata[order(as.numeric(rownames(loandata))),]
loandata_backup <- loandata

detach(data)

summary(loandata)

attach(loandata)

################# 01. frquency plot of the loans by their status #########
### overall default rate
default_new <- as.data.frame(table(SeriousDlqin2yrs))
default_new$col_pct <- default_new$Freq/sum(default_new$Freq)

### exporting the results to a csv file
write.table(default_new, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/01.Default_rate_new.csv")

### barplot of the freq
ggplot(default_new, aes(SeriousDlqin2yrs,Freq, fill=SeriousDlqin2yrs)) + scale_y_continuous(limits=c(0,100000))+
  geom_bar(stat="identity") + labs(title="Loan Default rate", x="Loan status", y="% of loans") +
  geom_text(aes(label=paste(round(col_pct*100,digits=1),"%",sep="")),position="identity",vjust = -0.25, hjust=0) + 
  theme(legend.position="bottom") +   scale_fill_discrete("Legend",labels=c("0=Active loans","1=Default loans"))

################# 02. Revolving credit utilization #################
### percentile distribution

### box-plot 
ggplot(loandata, aes(x=SeriousDlqin2yrs, y=RevolvingUtilizationOfUnsecuredLines)) + 
  geom_boxplot(outlier.colour = "red") +
  labs(title="Distribution of Credit utilization", x="Loan status", y="Credit utilization") +
  theme(legend.position="bottom") +   scale_fill_discrete("Legend",labels=c("0=Active loans","1=Default loans"))

pct_dist_utilization <- 
  as.data.frame(quantile(
    RevolvingUtilizationOfUnsecuredLines, 
    c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,.997,.998,.999,1)))

colnames(pct_dist_utilization) = "Revolving_Utilization_of_Credit"

pct_dist_utilization$Revolving_Utilization_of_Credit <- 
  paste(round(pct_dist_utilization$Revolving_Utilization_of_Credit*100,digits=1),"%",sep="")

# the difference between the 99.7th and 99.99th percentile is too big
# it is skewing the observations
# capping the maximum credit utlitization % @ 99.7th percentile (3-sigma)
loandata$RevolvingUtilizationOfUnsecuredLines <- 
  ifelse(loandata$RevolvingUtilizationOfUnsecuredLines > 1.775355907,1.775355907,loandata$RevolvingUtilizationOfUnsecuredLines)

attach(loandata)

### percentile distribution after capping
pct_dist_utilization_cap <- 
  as.data.frame(quantile(
    RevolvingUtilizationOfUnsecuredLines, 
    c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,.997,.998,.999,1)))

colnames(pct_dist_utilization_cap) = "Revolving_Utilization_of_Credit"

pct_dist_utilization_cap$Revolving_Utilization_of_Credit <- 
  paste(round(pct_dist_utilization_cap$Revolving_Utilization_of_Credit*100,digits=1),"%",sep="")

write.table(pct_dist_creditbalance, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/02.Pct_dist_Revovling_utlization.csv")

### bar plot of the credit usage by default status
ggplot(loandata, aes(RevolvingUtilizationOfUnsecuredLines, fill=as.factor(SeriousDlqin2yrs))) +
  geom_histogram(binwidth=0.1) + labs(title="Credit Utilization distribution by loan status", x="Credit Usage") +
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

### box plot after capping
ggplot(loandata, aes(x=SeriousDlqin2yrs, y=RevolvingUtilizationOfUnsecuredLines)) + 
  geom_boxplot(outlier.colour = "red") +
  labs(title="Distribution of Credit utilization", x="Loan status", y="Credit utilization") +
  theme(legend.position="bottom") +   scale_fill_discrete("Legend",labels=c("0=Active loans","1=Default loans"))

### conditional density plot
cdplot(as.factor(SeriousDlqin2yrs) ~ RevolvingUtilizationOfUnsecuredLines,
       col=c(3,2),main="Likelihood of default by Credit usage",
       ylab = "Loan status", xlab = "Revolving credit usage")

# cdp clearly represents that fact the chance of a customers defaulting on their
# loan increases with the increased credit usage month on month i.e.
# a customers reliance on the credit to fund their expenses evetually will result in
# them defaulting on one if not all of their loans

################# 03. age of the borrowers #################
### histogram of the age of the borrowers

### box plot of age
ggplot(loandata, aes(x=SeriousDlqin2yrs, y=age)) + 
  geom_boxplot(outlier.colour = "red") +
  labs(title="Distribution of age", x="Loan status", y="Age of customers") +
  theme(legend.position="bottom") +   scale_fill_discrete("Legend",labels=c("0=Active loans","1=Default loans"))

### distribution of loan default by age
ggplot(loandata, aes(age, fill=as.factor(SeriousDlqin2yrs))) +
  geom_histogram(binwidth=5) + labs(title="Age distribution by loan status", x="Age of borrowers") +
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

### creating age buckets
bins_age <- seq(20,120,by=20)

### counting the number of loans under each age bracket by deliquency
default_age <- as.data.frame(table(cut(age,bins_age), SeriousDlqin2yrs))
colnames(default_age) <- c("Age_bracket","Default","Num_of_Borrowers")

# using regular expression to replace the , with -
# replacing multiple strings is not possible with gsub
default_age$Age_bracket <- gsub(",", "-",default_age$Age_bracket)

write.table(default_age, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/03.Default_rate_by_age.csv")

### 100% stacked bar plot by age and loan status
ggplot(default_age,aes(x = Age_bracket, y = Num_of_Borrowers,fill = Default)) + scale_y_continuous(labels = percent_format()) +
  geom_bar(stat="identity",position="fill") + labs(title="Loan Default rate by age", x="Age bracket", y="% of borrowers") +
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

### conditional density plot
cdplot(as.factor(SeriousDlqin2yrs) ~ age,col=c(3,2),
       ylab = "Loan status", xlab = "Age of borrower",
       main="Likelihood of default by Age")

# chances of loan default decreases with age

################# 04. Num30to59DaysPastDue #################

summary(Num30to59DaysPastDue)
# 75% of the customers, both with and without defaults have 30-59 dues in the past

bins_30to59days <- seq(0,15,by=1)
Default_by_30to59Days <- 
  table(cut(Num30to59DaysPastDue,bins_30to59days), SeriousDlqin2yrs)

print(Default_by_30to59Days)

### 100 stacked chart
ggplot(loandata, aes(Num30to59DaysPastDue, fill=as.factor(SeriousDlqin2yrs))) +
  geom_histogram(binwidth=1) + 
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

# Majority of the population have 0 30-59 dues and even among defaulters, there is 
# a decline in the number of customers with 30-59days past dues.

### conditional density plot
cdplot(as.factor(SeriousDlqin2yrs) ~ Num30to59DaysPastDue,
       col=c(3,2),ylab = "Loan status", xlab = "Num30to59DaysPastDue",
       main="Likelihood of default by Num30to59DaysPastDue")

# though from the bar chart we observed that majority of customers have 0 30-59 days
# the cdp shows that amongst the defaulters, the chances of the default increases with
# greater number of 30-59 days past due.

################# 05. Debt ratio #################

summary(DebtRatio)

### box plot
ggplot(loandata, aes(x=SeriousDlqin2yrs, y=DebtRatio)) + 
  geom_boxplot(outlier.colour = "red")

### percentile distribution of debt-ratio
pct_dist_debtratio <-
  as.data.frame(quantile(DebtRatio, c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,0.997,0.998,0.999,1)))

colnames(pct_dist_debtratio) = "DebtRatio"

pct_dist_debtratio$DebtRatio <- 
  paste(round(pct_dist_debtratio$DebtRatio*100,digits=1),"%",sep="")

##### conditional density plot
cdplot(as.factor(SeriousDlqin2yrs) ~ DebtRatio,
       col=c(3,2),ylab = "Loan status", xlab = "Debt Ratio",
       main="Likelihood of default by Debt")

### capping the maximum debt ratio % @ 6x or 500%
loandata$DebtRatio <- 
  ifelse(loandata$DebtRatio > 5,5,loandata$DebtRatio)

attach(loandata)

### conditional density plot post capping the debt ratio
cdplot(as.factor(SeriousDlqin2yrs) ~ DebtRatio,
       col=c(3,2),ylab = "Loan status", xlab = "Debt Ratio",
       main="Likelihood of default by Debt")

# general trend looks like there is a higher chance for deaulting amongst those with a
# high expense to income ratio

### bar plot
ggplot(loandata, aes(DebtRatio, fill=as.factor(SeriousDlqin2yrs))) +
  geom_histogram(binwidth=0.5) +  labs(title="Loan Default rate by Debt ratio", x="Debt Ratio", y="# of borrowers") +
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

write.table(pct_dist_debtratio, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/05.pct_dist_debtratio.csv")

### average debt-ratio by deliquency
tapply(DebtRatio, SeriousDlqin2yrs, FUN = mean)
# the average debt of defaulter is about 3 times that of the non-defaulters

### box plot after capping
ggplot(loandata, aes(x=SeriousDlqin2yrs, y=DebtRatio)) + 
  geom_boxplot(outlier.colour = "red")
# even post capping the maximum debt, there still seems to be a lot of outliers in both ends

################# 06. Monthly income #################

summary(MonthlyIncome)

### average income by deliquency
tapply(MonthlyIncome, SeriousDlqin2yrs, FUN = mean)

hist(MonthlyIncome, main="Distribution of loans by monthly income of borrowers", col="green",
     xlab="MOnthly income", ylab="Num. of people", ylim=c(0,120000))

### box plot
ggplot(loandata, aes(x=SeriousDlqin2yrs, y=MonthlyIncome)) + 
  geom_boxplot(outlier.colour = "red")

### percentile distribution of the income
pct_dist_income <- 
  as.data.frame(quantile(MonthlyIncome, c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,.997,.998,.999,1)))

colnames(pct_dist_income) = "MonthlyIncome"

### capping the maximum debt ratio % @ 99.7th percentile (3-sigma)
loandata$MonthlyIncome <- 
  ifelse(loandata$MonthlyIncome > 50000,50000,loandata$MonthlyIncome)

attach(loandata)

### average income by deliquency
tapply(MonthlyIncome, SeriousDlqin2yrs, FUN = mean)

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
# overall number of defaulter appears to decrease with increase in monthly income

# conditional density plot
cdplot(as.factor(SeriousDlqin2yrs) ~ MonthlyIncome,
       col=c(3,2),ylab = "Loan status", xlab = "Monthly Income",
       main="Likelihood of default by Monthly Income")
# general trend indicates a decline in the defaults with increase in monthly income
# but there are pocket of areas even amongst the high earners where is a higher concentration
# of defaults

### bar plot after capping
ggplot(loandata, aes(x=SeriousDlqin2yrs, y=MonthlyIncome)) + 
  geom_boxplot(outlier.colour = "red")

################# 07. Number of credit lines open #################

summary(NumberOfOpenCreditLinesAndLoans)
# it looks like in general people have lot of open credit lines, since the bare mininum
# itself seems to be couple with 50% of the population with 8.

hist(NumberOfOpenCreditLinesAndLoans, 
     main="Distribution of borrowers by number of credit lines", 
     xlab="Num. of credit line", col="green")

### average number of open credit lines by loan status
tapply(NumberOfOpenCreditLinesAndLoans, SeriousDlqin2yrs, FUN = mean)
# no big difference between the defaulters and their counterparts in terms of number of credit lines

pct_dist_creditlines <-
  as.data.frame(quantile(
    NumberOfOpenCreditLinesAndLoans, 
    c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,0.997,0.998,0.999,1)))

colnames(pct_dist_creditlines) = "Num_of_Credit_Lines"

default_num_creditlines <- as.data.frame(table(
  cut(NumberOfOpenCreditLinesAndLoans,
      c(0,3,6,8,11,16,18,20,100)), SeriousDlqin2yrs))

colnames(default_num_creditlines) <- c("NumCreditLines","Default","Num_of_Borrowers")

### 100% stacked bar plot by number of credit lines and loan status
ggplot(default_num_creditlines,aes(x = NumCreditLines, y = Num_of_Borrowers,fill = Default)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())
# the bar plot shows that there are more defaulters in the 0-3 group which seems 
# counter-intuitive. But when combined with credit usage, debt and income basically
# this points towards people who live paycheck to paycheck and their credit. But 
# cant say for sure, need more details

write.table(default_num_creditlines, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/07.Default_rate_num_creditlines.csv")

# conditional density plot
cdplot(as.factor(SeriousDlqin2yrs) ~ NumberOfOpenCreditLinesAndLoans,
       col=c(3,2),ylab = "Loan status", xlab = "# of open credit lines & loans",
       main="Default likelihood by Number of credit lines")


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
      c(0,1,2,3,4,5,10,60)), SeriousDlqin2yrs))

colnames(default_num_propertyloans) <- c("NumPropertyLoans","Default","Num_of_Borrowers")

### 100% stacked bar plot by number of property loans and loan status
ggplot(default_num_propertyloans,aes(x = NumPropertyLoans, y = Num_of_Borrowers,fill = Default)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())
# more the number of properties, higher the likelihood of default

write.table(default_num_propertyloans, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/09.Default_rate_num_propertyloans.csv")

### conditional density plot
cdplot(as.factor(SeriousDlqin2yrs) ~ NumberRealEstateLoansOrLines,
       col=c(3,2),ylab = "Loan status", xlab = "# of property lines or loans",
       main="Loan defualt likelihood by Real estate loans")

################# 11. Number of Dependents #################

summary(NumberOfDependents)
# half the population have either no or 1 dependent

### average number of property loans or mortgages
tapply(NumberOfDependents, SeriousDlqin2yrs, FUN = mean)
tapply(NumberOfDependents, SeriousDlqin2yrs, FUN = median)
# both sets seems to have on an average 1 dependent.

### histogram
ggplot(loandata, aes(NumberOfDependents, fill=as.factor(SeriousDlqin2yrs))) +
  geom_histogram(binwidth=1) + labs(title="Num. of dependents by loan status", x="Num. of dependents") +
  theme(legend.position="bottom") + scale_fill_discrete("Loan status",labels=c("0=Active","1=Default"))

default_dependents <- as.data.frame(table(NumberOfDependents, by=SeriousDlqin2yrs))

colnames(default_dependents) <- c("Num_of_dependents","Default","Num_of_Borrowers")

ggplot(default_dependents,aes(x = Num_of_dependents, y = Num_of_Borrowers,fill = Default)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format())
# an obvious conclusion perhaps. The proportion of defaulters increases with dependents

write.table(default_dependents, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/11.Default_rate_by_dependents.csv")

### conditional density plot
cdplot(as.factor(SeriousDlqin2yrs) ~ NumberOfDependents,
       col=c(3,2),ylab = "Loan status", xlab = "# of Dependents")


### to be sure that any changes made to the data is updated, re-attach the data
attach(loandata)

######################## Bivariate analysis #################

### correlation matrix and plot
corrmatrix <- cor(subset(loandata[,-c(14,15)],select = -1))
colnames(corrmatrix)= c("Credit Utilization","Age","30-59DaysPastDue","Debt","Income","#Credit","90DaysPastDue","RealEstateLoans","60-89DaysPastDue","Dependents","TotalCredit","Expense")
corrplot(corrmatrix, method = "circle")

### scatter plot matrix
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

# the following code returns the scatter plot matrix with the correlation coefficients
# I have commented it since if the entire script is run in one shot it this line of code
# takes a long time, so if require uncomment it and run

# pairs(loandata[,-1], upper.panel = panel.cor)

corr_matrix <- as.data.frame(cor(loandata[,-1]))
write.table(corr_matrix, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/Model/Outputs/12.correlation_matrix.csv")

# 1. Age has a low inverse correlation with credit usage and number of dependents
# 2. Credit usage has a small +ve correlation with the due days past
# 3. Debt ratio and monthly income, small -ve correlation

### income vs age
ggplot(loandata, aes(age,MonthlyIncome)) + geom_point() + geom_smooth(method="auto")

# loandata_backup$monthlyincome_log <- log(loandata_backup$MonthlyIncome,base=exp(1))

ggplot(loandata_backup, aes(age,monthlyincome_log)) + geom_point() + geom_smooth(method="auto")

ggplot(loandata, aes(NumberOfOpenCreditLinesAndLoans,DebtRatio, fill=as.factor(SeriousDlqin2yrs))) + 
  geom_point() + geom_smooth(method="auto")

### debt ratio vs property loans
ggplot(loandata, aes(NumberRealEstateLoansOrLines,DebtRatio)) + geom_point() + geom_smooth(method="auto")

### Age vs revolving credit utilization
ggplot(loandata, aes(age,RevolvingUtilizationOfUnsecuredLines)) + 
  geom_point() + geom_smooth(method="auto") +
  labs(title="Age vs Credit usage", x="Age", y="Revolving credit utilization") +
  theme(legend.position="bottom")

######################## data checks ends here ########################

##########################################################################

# Model part starts from here

# if the EDA part above is not run, then run the below part else skip to the composite variables section

# sampling from the non-defaults to achieve the aforementioned ratio
default_loans <- subset(data,SeriousDlqin2yrs==1)
nondefault_loans <- subset(subset(data,(SeriousDlqin2yrs==0)),MonthlyIncome != -1)

set.seed(12345)
nondefault_loans_f <- nondefault_loans[sample(nrow(nondefault_loans), 90234),]

loandata <- rbind(default_loans, nondefault_loans_f)
loandata <- loandata[order(as.numeric(rownames(loandata))),]

rm(list= c("default_loans", "nondefault_loans_f","nondefault_loans"))

detach(data)
summary(loandata)
attach(loandata)

# capping the maximum credit utlitization % @ 99.7th percentile (3-sigma)
loandata$RevolvingUtilizationOfUnsecuredLines <- 
  ifelse(loandata$RevolvingUtilizationOfUnsecuredLines > 1.775355907,1.775355907,loandata$RevolvingUtilizationOfUnsecuredLines)

### capping the maximum debt ratio % @ 6x or 500%
loandata$DebtRatio <- 
  ifelse(loandata$DebtRatio > 5,5,loandata$DebtRatio)

### capping the maximum debt ratio % @ 99.7th percentile (3-sigma)
loandata$MonthlyIncome <- 
  ifelse(loandata$MonthlyIncome > 50000,50000,loandata$MonthlyIncome)

attach(loandata)
summary(loandata)

loandata_backup <- loandata
##########################################################################

### creating compound metrics i.e combining two or more variables or indexes or existing variables
# combine the two credit lines
# monthly expense * debt-ratio

loandata$totalcreditlines <- loandata$NumberOfOpenCreditLinesAndLoans + loandata$NumberRealEstateLoansOrLines
loandata$expense <- loandata$DebtRatio * loandata$MonthlyIncome

loandata$log_creditusage <- log((RevolvingUtilizationOfUnsecuredLines*100)+1,base=exp(1))
loandata$log_debtratio <- log((DebtRatio*100)+1,base=exp(1))

### function to check for NaN's in a dataframe
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

### checking for NaN in the 
table(is.nan.data.frame(loandata))

### function to check for inifnity in a dataframe
is.infinite.data.frame <- function(x)
  do.call(cbind, lapply(x, is.infinite))

### checking for infinity
table(is.infinite.data.frame(loandata))

attach(loandata)

#####################################################################################
################  model developlemt  ##################

### splitting the data into training and test data
# there are in total 100,260 records
# 50% of the data will be used for training/cross-validation.
# remaining 50% will be used for testing/validation

set.seed(12345)
index <- sample(nrow(loandata), 50260)
trainingdata <- loandata[index,]
testdata <- loandata[-index,]

table(trainingdata$SeriousDlqin2yrs)
# 10.12% of the loans are defaults

table(testdata$SeriousDlqin2yrs)

### data types of the input data's columns
table(sapply(loandata, class))
table(sapply(testdata, class))
table(sapply(trainingdata, class))

set.seed(12345)
tuningdata <- trainingdata[sample(nrow(trainingdata), 5000),]
table(tuningdata$SeriousDlqin2yrs)

tunechk <- tuningdata
tunechk$newVariable <- factor(tunechk$SeriousDlqin2yrs, levels=c(0,1), labels=c("No", "Yes"))


################  model selection  ##################



### logistic regression
logistic_null <- glm(SeriousDlqin2yrs~1, family=binomial(link="logit"), data=trainingdata)

logistic_fm1 <- glm(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, family=binomial(link="logit"), data=trainingdata)
logistic1 <- step(logistic_null, scope=list(lower=formula(logistic_null),upper=formula(logistic_fm1)),direction="both")

# using the transformed variables
logistic_fm2 <- glm(SeriousDlqin2yrs~.-RevolvingUtilizationOfUnsecuredLines-DebtRatio, family=binomial(link="logit"), data=trainingdata)
logistic2 <- step(logistic_null, scope=list(lower=formula(logistic_null),upper=formula(logistic_fm2)),direction="both")

# ROC curve
prob <- predict(logistic1, newdata=testdata, type="response")
pred <- prediction(prob, testdata$SeriousDlqin2yrs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col="blue")
abline(a=0,b=1,col="red")
auc <- performance(pred, measure = "auc")
auc
# AUC = 0.8620108

prob <- predict(logistic2, newdata=testdata, type="response")
pred <- prediction(prob, testdata$SeriousDlqin2yrs)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col="blue")
abline(a=0,b=1,col="red")
auc <- performance(pred, measure = "auc")
auc
# AUC = 0.8428645

### GAM

gam1 <- gam(SeriousDlqin2yrs ~ s(RevolvingUtilizationOfUnsecuredLines) +
              s(age)+s(Num30to59DaysPastDue)+s(DebtRatio)+s(MonthlyIncome)+
              s(NumberOfOpenCreditLinesAndLoans)+s(Num90DaysPastDue)+
              s(NumberRealEstateLoansOrLines)+s(Num60to89DaysPastDue)+
              s(NumberOfDependents)+s(totalcreditlines)+s(expense),
            family = binomial, method="GCV.Cp", data=trainingdata)

gam.check(gam)
plot.gam(gam)

### MARS

tc_mars <- trainControl("cv",10)
mars.grid <- expand.grid(degree=seq(1,5,1)) 
train(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=tuningdata, method="earth", 
                  trControl=tc_mars)
# nprune: 8

earth(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainingdata ,
      nprune=8,degree=1,penalty=0)

earth(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainingdata ,
      nprune=8,degree=1,penalty=1)

earth(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainingdata ,
      nprune=8,degree=1,penalty=-1)

# nprune:8
# degree:1
# penalty:-1

### CART
tc <- trainControl("cv",10)
rpart.grid <- expand.grid(cp=seq(0,1,0.01)) 
rpart_cp <- train(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainingdata, method="rpart", 
                  trControl=tc,  tuneGrid=rpart.grid)
# cp: 0.01

rpart_tune <- tune.rpart(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainingdata,
                         cp=0.01,minsplit = c(2,3,4,5),maxdepth=c(3,4,5,6,7))
# minsplit  cp  maxdepth
# 2 0.01  3

### RandomForest

tc_rf <- trainControl("cv",10)
rf.grid <- expand.grid(mtry=seq(2,6,1))
train(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=tuningdata, method="rf", 
      trControl=tc_rf,  tuneGrid=rf.grid)
# mtry:2

rf <- randomForest(SeriousDlqin2yrs~.-log_creditusage-log_debtratio,data=trainingdata, 
                   mtry=2, ntree=500, replace=TRUE, keep.forest=TRUE, importance=TRUE)

plot(rf)

### bagged cart

baggedcart <- bagging(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainingdata,
                      nbagg=30,coob=T,control= rpart.control(minsplit=2, cp=0.01, maxdepth=3,xval=0))

names(baggedcart)

baggedcart$err
# 0.08125746

baggedcartresponse <- predict(baggedcart,newdata=testdata[,-1],type="class")

names(baggedcartresponse) <- "y"
baggedcartresult <- as.data.frame(cbind(baggedcartresponse, testdata[, 1]))
names(baggedcartresult) <- c("Predicted", "Actual")

mean(baggedcartresult$Predicted!= baggedcartresult$Actual)
0.0784

### SUPPORT VECTOR MACHINE MODEL
set.seed(1234)
# Setup for cross validation
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,		    # do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)

tunechk <- tuningdata
tunechk$newVariable <- factor(tunechk$SeriousDlqin2yrs, levels=c(0,1), labels=c("No", "Yes"))

#Train and Tune the SVM
svm.tune <- train(x=tunechk[,-c(1,14,15,16)], y=tunechk[,16],
                  method = "svmLinear",
                  preProc = c("center","scale"),
                  metric="ROC",
                  trControl=ctrl)

svm.tune
# c:0.25
# gamma:4

svm.grid <- expand.grid(C=c(0.1,1,10))

svm.tune2 <- train(x=tunechk[,-c(1,14,15,16)], y=tunechk[,16],
                  method = "svmLinear",
                  preProc = c("center","scale"),
                  metric="ROC",
                  trControl=ctrl, tuneGrid=svm.grid)

# set.seed (1234)
# svm.tune3=tune(svm,
#               SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=tuningdata, 
#               kernel ="linear",
#               ranges =list(cost=c(0.01,0.1,0.75,1,1.5,2,5,10,100)))

svm_final <- svm(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainingdata, 
                 kernel ="linear", cost =1, gamma=4, scale =T , probability=TRUE)

### GBM

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = c(100, 250, 500),
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(6, 8, 10, 15, 20),
  gamma = 0,
  colsample_bytree = c(0.3,0.4,0.5),
  min_child_weight = 10  
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_1 = train(
  x=tunechk[,-c(1,14,15,16)], y=tunechk[,16],
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree",
  metric = "ROC"
)
# 
# nrounds:100
# gamma:0
# eta:0.001
# maxdepth:6
# colsample_bytree:0.5
# min_child_weight = 10

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

library(caret)
library(gbm)
library(dplyr)
library(plyr)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  classProbs=TRUE)

set.seed(12345)
gbmFit4 <- train(x=tunechk[,-c(1,14,15,16)], y=tunechk[,16],
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Only a single model can be passed to the
                 ## function when no resampling is used:
                 tuneGrid = data.frame(interaction.depth = 6,
                                       n.trees = 100,
                                       shrinkage = .01,
                                       n.minobsinnode = 10),
                 metric = "ROC")

gbmFit4

testfit <- predict(gbmFit4, newdata=testdata[,-1],type="prob")
actual <- as.data.frame(testdata[,1])
result <- as.data.frame(cbind(testfit$Yes, actual))

names(result) <- c("m1","actual")

result[paste0("m", 1)] <- (result[paste0("m", 1)] > .5) + 0
mean(result$m1!= result$actual)

### BART
options(java.parameters = "-Xmx5000m")

set_bart_machine_num_cores(4)
#bartMachine now using 4 computing cores available for use.
y <- trainingdata[,1]
X <- trainingdata[,-1]
bart_machine <- bartMachine(X,y)
#default bartmachine model with set hyperparameters
bart_machine
#summary of Bartmachine
k_fold_cv(X, y, k_folds = 10)
#assess level of overfitting by k-fold cross validation using 10 randomized folds
#pseudo R square indicates slightly overfitting nature
rmse_by_num_trees(bart_machine, num_replicates = 20)
#this is done to examine how out-of-sample predictions vary by the number of trees
bart_machine_cv <- bartMachineCV(X, y)
#bartMachine CV win: k: 5 nu, q: 10, 0.75 m: 200 
#returns the winning model which is the lowest out-of sample RMSE over a 10-fold CV
k_fold_cv(X, y, k_folds = 10, k = 2, nu = 10, q = 0.75, num_trees = 200)
#was done to evaluate the in-sample performance and the out-of-sample performance of the bartmachine model
testerror <- cbind(predict(bart_machine_cv, testdata[,-1]), testdata[,1])
names(testerror) <- c("pred1","actual")
result[paste0("pred", 1)] <- (result[paste0("pred", 1)] > .5) + 0
mean(result$m1!= result$actual)

check_bart_error_assumptions(bart_machine_cv)
#to assess Normality and heteroskedasticity. QQ plot and residual-by-predicted plot is obtained
#appears that errors are likely normal and heteroskedastic
plot_convergence_diagnostics(bart_machine_cv)
# four types of convergence diagnostics is obtained.
# observed that model has been sufficently burned-in
investigate_var_importance(bart_machine_cv, num_replicates_for_avg = 20)
#this is done to find out which are the most important variables among the lot
#done by inclusion proportions which tells us the which are the most important variables
#and which significantly affects the response
cov_importance_test(bart_machine_cv)


#####################################################################################
### 10-fold cross-validation

# Creating 10 equally size folds
folds <- cut(seq(1,nrow(trainingdata)),breaks=10,labels=FALSE)

# checking if the folds are of equal size
foldsize <- table(folds)

rm(model,temp, classifiererror)
model <- vector(mode = "list", length = 1)
temp <- as.data.frame(matrix(0, nrow = max(foldsize), ncol = length(model)))
classifiererror <- as.data.frame(matrix(0, nrow = 10, ncol = length(model)))

for(i in 1:10){
  
  rm(prediction,result,testindexes,testfold,trainfold,temp1)
  
  prediction <- data.frame()
  result <- data.frame()
  
  #Segement the training data by fold using the which() function 
  testindexes <- which(folds==i)
  testfold <- trainingdata[testindexes, ]
  trainfold <- trainingdata[-testindexes, ]
  
  ### logistic
  
  # model1
  model[[1]] <- logistic1

  #model2
  model[[2]] <- logistic2

  ### GAM - model3
  
  model[[3]] <- gam(SeriousDlqin2yrs ~ s(RevolvingUtilizationOfUnsecuredLines) +
                s(age)+s(Num30to59DaysPastDue)+s(DebtRatio)+s(MonthlyIncome)+
                s(NumberOfOpenCreditLinesAndLoans)+s(Num90DaysPastDue)+
                s(NumberRealEstateLoansOrLines)+s(Num60to89DaysPastDue)+
                s(NumberOfDependents)+s(totalcreditlines)+s(expense),
              family = binomial, method="GCV.Cp", data=trainfold)
  
  
  ### MARS - model4
  
  model[[4]] <- earth(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainfold,
                      nprune=8,degree=1,penalty=-1)
  
  
  ### Random Forest - model5
  
  model[[5]] <- randomForest(SeriousDlqin2yrs~.-log_creditusage-log_debtratio,data=trainfold,
                              mtry=2, ntree=500, replace=TRUE, keep.forest=TRUE, importance=TRUE)
  

  ### cart - model6
  
  model[[6]] <- rpart(SeriousDlqin2yrs~.-log_creditusage-log_debtratio,
                       method="class",control=rpart.control(cp=0.01, minsplit=2, xval=10, maxdepth=3), data=trainfold)
  

  ### SVM - model7
  model[[6]] = svm(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainfold, 
                   kernel ="linear", cost =1, scale =T)

  
 for (j in 1:5) {
    temp1 <- as.data.frame(predict(model[[j]], testfold[,-1], type="response"))
    temp[,j] <- temp1
    }
  
  for (j in 6:7) {
   temp1 <- as.data.frame(predict(model[[j]], testfold[,-1], type="class"))
   temp[,j] <- temp1
   }
  
  
  # append this iteration's predictions to the end of the prediction data frame
  # prediction <- rbind(prediction, temp)
  prediction <- temp
  # testsetcopy <- rbind(testsetcopy, as.data.frame(testfold[,1]))
  result <- cbind(prediction, testfold[,1])
  names(result) <- c("m1","actual")
  # names(result) <- c("m1","m2","m3","m4","m5","m6","actual")
  # names(result) <- c("m1","m2","m3","m4","m5","actual")
  # result[paste0("m", 1:2)] <- (result[paste0("m", 1:2)] > .5) + 0
  
  classifiererror[i,1] <- mean(result$m1!= result$actual)
  # classifiererror[i,2] <- mean(result$m2!= result$actual)
  # classifiererror[i,3] <- mean(result$m3!= result$actual)
  # classifiererror[i,4] <- mean(result$m4!= result$actual)
  # classifiererror[i,5] <- mean(result$m5!= result$actual)
  # classifiererror[i,6] <- mean(result$m6!= result$actual)
  # classifiererror[i,7] <- mean(result$m7!= result$actual)
  # classifiererror[i,8] <- mean(result$m8!= result$actual)

}

summary(classifiererror)

#####################################################################################

### Model comparison

### random forest
rf_final <- randomForest(SeriousDlqin2yrs~.-log_creditusage-log_debtratio-totalcreditlines,data=trainingdata, 
              mtry=2, ntree=500, replace=TRUE, keep.forest=TRUE, importance=TRUE)

pred_rf <- as.data.frame(predict(rf_final, newdata=testdata[,-1], type="response"))
testerror_rf <- cbind(pred_rf, testdata[,1])
names(testerror_rf) <- c("model","actual")
mean(testerror_rf$model!= testerror_rf$actual)

pred_mean <- as.data.frame(predict(logistic_null, newdata=testdata[,-1], type="response"))
testerror_mean <- cbind(pred_mean, testdata[,1])
names(testerror_mean) <- c("model1","actual")
testerror_mean[paste0("model", 1)] <- (testerror_mean[paste0("model", 1)] > .5) + 0
mean(testerror_mean$model1!= testerror_mean$actual)
# 0.09878

# partial dependence plots for random forest
partialPlot(rf_final,trainingdata,age, main="Partial Dependence on Age")
partialPlot(rf_final,trainingdata,RevolvingUtilizationOfUnsecuredLines, main="Partial Dependence on Credit Utilization")
partialPlot(rf_final,trainingdata,expense, main="Partial Dependence on Expense")
partialPlot(rf_final,trainingdata,DebtRatio,main="Partial Dependence on Debt-to-Income ratio")
partialPlot(rf_final,trainingdata,MonthlyIncome, main="Partial Dependence on Income")
partialPlot(rf_final,trainingdata,Num90DaysPastDue, main="Partial Dependence on Num90DaysPastDue")

### SVM
svm_final <- svm(SeriousDlqin2yrs~.-log_creditusage-log_debtratio, data=trainingdata, 
    kernel ="linear", cost =1, gamma=4, scale =T , probability=TRUE)

# AUC - mean
prob_mean <- predict(logistic_null, newdata=testdata[,-1], type="response")
pred_mean <- prediction(prob_mean, testdata$SeriousDlqin2yrs)
perf_mean <- performance(pred_mean, measure = "tpr", x.measure = "fpr")
performance(pred_mean, measure = "auc")
# 0.5

# AUC - logistic
prob_log <- predict(logistic1, newdata=testdata[,-1], type="response")
pred_log <- prediction(prob_log, testdata$SeriousDlqin2yrs)
perf_log <- performance(pred_log, measure = "tpr", x.measure = "fpr")
performance(pred_log, measure = "auc")
# 0.8596288

# AUC - Random Forest
prob_rf <- predict(rf_final, newdata=testdata[,-1], type="prob")[,2]
pred_rf <- prediction(prob_rf, testdata$SeriousDlqin2yrs)
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")
performance(pred_rf, measure = "auc")
# 0.8707457

# AUC - Bagged Cart
prob_bc <- predict(baggedcart, newdata=testdata[,-1], type="prob")[,2]
pred_bc <- prediction(prob_bc, testdata$SeriousDlqin2yrs)
perf_bc <- performance(pred_bc, measure = "tpr", x.measure = "fpr")
performance(pred_bc, measure = "auc")
# 0.7058856

# AUC - SVM
prob_svm <- predict(svm_final, type="prob", newdata=testdata[,-1], probability = TRUE)
pred_svm <- prediction(attr(prob_svm, "probabilities")[,2], testdata$SeriousDlqin2yrs)
perf_svm <- performance(pred_svm, measure = "tpr", x.measure = "fpr")
performance(pred_svm, measure = "auc")
# 0.8608012

# AUC - GBM
prob_gbm <- predict(gbmFit4, newdata=testdata[,-1], type="prob")[,2]
pred_gbm <- prediction(prob_gbm, testdata$SeriousDlqin2yrs)
perf_gbm <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")
performance(pred_gbm, measure = "auc")
# 0.8629926

plot(perf_log, col=2, main="ROC curves of classifiers")
plot(perf_rf,add=TRUE,col=3)
plot(perf_bc,add=TRUE,col=4)
plot(perf_svm,add=TRUE,col=5)
plot(perf_gbm,add=TRUE,col=6)
# Draw a legend.
abline(a=0,b=1,col="black")
legend(0.55, 0.5, c('Logistic Regression: 0.860', 
                   'Random Forest: 0.871', 
                   'Bagged Cart: 0.706',
                   'SVM: 0.861',
                   'GBM: 0.863'), 2:6, cex=0.75)

importance(rf_final)
varImpPlot(rf_final)

varImpPlot(rf_final,sort=TRUE,scale=TRUE,cex=1.2,pch=16,main="Variable Importance")
partialPlot(rf_final,x.var='MonthlyIncome',pred.data=loandata, ylab='Num90DaysPastDue')

final_model <- rf_final

save.image (file = "IE590_Project_Pazhamalai_04272015.Rdata")

