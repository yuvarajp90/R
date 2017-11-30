### libraries to be loaded (please run them twice)
library(lattice)
library(car)
library(ggplot2)
library(corrplot)
library(compare)
library(GGally)
library(MASS)
library(fmsb)
library(caret)
library(e1071)
library(nnet)

##########################################################################

### current work directory
getwd()

### set work directory
setwd("E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/IE 590 - Info. Engg. Murugappan")

### importing the raw data
data <- read.csv("housing_urban_data.csv", header=TRUE, sep=",")

### Dimensions of the raw data
dim(data)
# 637x32

### checking for missing values
check1 <- na.omit(data)

ifelse(compare(check1, data, equal=TRUE) == "TRUE",rm(check1), "Further Check Required")

### names of the data variables
names(data)

### variables type
as.data.frame(sapply(data,class))

### univariate of the predictors
summary(data[,-c(1,2,4)])

### percentile distribution of Y
y_pct_dist <- as.data.frame(quantile(
    data$y,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.96,.97,.98,.99,.997,.998,.999,1)))

ggplot(data, aes(y)) + geom_histogram(binwidth=5) + geom_density(kernel="gaussian")+
  labs(title="Distribution of Housing Quality")

ggplot(data, aes(y)) + geom_density(kernel="gaussian")+
  labs(title="Distribution of Housing Quality")
# the response Y is approx. normal

# md stands for model data
md <- data[,-c(1,2)]
######################## Bivariate analysis #################

### correlation matrix and plot
corrmatrix <- cor(md[,-2])
write.table(corrmatrix, sep=",", "E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/05. Project/IE 590 - Info. Engg. Murugappan/correlation.csv")

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

# pairs(md[,c(1,3,4,5,7,10,12,13,14,15,16,17,19,27)], upper.panel = panel.cor)

# ggpairs(md[,c(1,3,4,5,7,10,12)])

# spm(md[,c(1,3,4,5,7,10)], smoother=loessLine)
spm(md[,c(1,3,4,5,7,10)], diagonal="density", smooth=F, cex.main=2.5)

################  model selection  ##################

######## linear regression
lr1 <- lm(y ~ ., data=md)
lr_aic <- stepAIC(lr1, direction="both", trace=FALSE)

# problem with the above step aic approach is that there are insiginificant predictors included
# instead run the step aic function with p-value cut-off of 0.05
qchisq(0.05, 1, lower.tail = F)

lr_aic <- stepAIC(lr1, direction="both", k=qchisq(0.05, 1, lower.tail = F), trace=FALSE)

summary(lr_aic)
vif(lr_aic)

lr2 <- lm(y ~ region+roof_concrete+floor_mud+tapwater+water_away+electricity+lpg+
               banking+phone+all_assets+sex_ratio+backward_pop+literacy, data=md)

summary(lr2)
vif(lr2)

# > AIC(lr_aic)
# [1] 4169.06
# AIC(lr2)
# [1] 4198.388
# > BIC(lr_aic)
# [1] 4262.652
# > BIC(lr2)
# [1] 4283.067

### model diagnostics

plot(lr2)

# Assessing Outliers

# Bonferonni p-value for most extreme obs
outlierTest(lr2)

### non-normality
#qq plot for studentized resid 
qqnorm(lr2$res)
qqline(lr2$res)
qqPlot(lr2, main="QQ Plot")
hist(lr2$res, main="Histogram of the residuals")

### Non-constant variance (Evaluate homoscedasticity)
ncvTest(lr2)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(lr2)

### Non-linearity
crPlots(lr2)

### test for indenpendence
# Test for Autocorrelated Errors
durbinWatsonTest(lr2)

# assumptions of constant variance and normal errors are not met
# hence Y as to be transformed

y_transformation <- boxcox(lr_aic,lambda=seq(-3,3,by=.1))

lr_trans <- lm(log(y) ~ ., data=md)
lr_transf_aic <- stepAIC(lr_trans, direction="both", k=qchisq(0.05, 1, lower.tail = F), trace=FALSE)

summary(lr_transf_aic)
vif(lr_transf_aic)

lr3 <- lm(log(y) ~ region+roof_concrete+floor_mud+tapwater+electricity+lpg+tv+
            phone+all_assets+sex_ratio+backward_pop+female_literacy, data=md)

summary(lr3)
vif(lr3)

### model diagnostics
plot(lr3)

qqPlot(lr3, main="QQ Plot")
ncvTest(lr3)
crPlots(lr3)

plot(y=lr3$res, x=md$roof_concrete);abline(0,0)
plot(y=lr3$res, x=md$floor_mud);abline(0,0)
plot(y=lr3$res, x=md$tapwater);abline(0,0)
plot(y=lr3$res, x=md$electricity);abline(0,0)
plot(y=lr3$res, x=md$lpg);abline(0,0)
plot(y=lr3$res, x=md$tv);abline(0,0)
plot(y=lr3$res, x=md$phone);abline(0,0)
plot(y=lr3$res, x=md$all_assets);abline(0,0)
plot(y=lr3$res, x=md$sex_ratio);abline(0,0)
plot(y=lr3$res, x=md$backward_pop);abline(0,0)
plot(y=lr3$res, x=md$female_literacy);abline(0,0)

# some variables have non-constant variance
# taking log of all variables to check the validity of a log-log model
md_log <- md
md_log[,-2] <- log(md_log[,-2]+1)

lr_log <- lm(y ~ ., data=md_log)
lr_log_aic <- stepAIC(lr_log, direction="both", k=qchisq(0.05, 1, lower.tail = F), trace=FALSE)

summary(lr_log_aic)
vif(lr_log_aic)

lr4 <- lm(y ~ region+roof_concrete+floor_mud+tapwater+electricity+lpg+tv+
            phone+all_assets+sex_ratio+backward_pop+literacy, data=md_log)

summary(lr4)
vif(lr4)

### model diagnostics
plot(lr4)

qqPlot(lr4, main="QQ Plot")
hist(lr4$res, main="Histogram of the residuals")
ncvTest(lr4)
crPlots(lr4)

plot(y=lr4$res, x=md_log$roof_concrete);abline(0,0)
plot(y=lr4$res, x=md_log$floor_mud);abline(0,0)
plot(y=lr4$res, x=md_log$tapwater);abline(0,0)
plot(y=lr4$res, x=md_log$electricity);abline(0,0)
plot(y=lr4$res, x=md_log$lpg);abline(0,0)
plot(y=lr4$res, x=md_log$tv);abline(0,0)
plot(y=lr4$res, x=md_log$phone);abline(0,0)
plot(y=lr4$res, x=md_log$all_assets);abline(0,0)
plot(y=lr4$res, x=md_log$sex_ratio);abline(0,0)
plot(y=lr4$res, x=md_log$backward_pop);abline(0,0)
plot(y=lr4$res, x=md_log$literacy);abline(0,0)

# Influential Observations
# added variable plots 
av.plots(lr4)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(md_log)-length(lr4$coefficients)-2)) 
plot(lr4, which=4, cook.levels=cutoff)

AIC(lr4)
BIC(lr4)
AIC(lr_log)
BIC(lr_log)
AIC(lr_log_aic)
BIC(lr_log_aic)
# > AIC(lr4)
# [1] -980.1074
# > BIC(lr4)
# [1] -899.8856
# > AIC(lr_log)
# [1] -979.8736
# > BIC(lr_log)
# [1] -823.8867
# > AIC(lr_log_aic)
# [1] -998.8079
# > BIC(lr_log_aic)
# [1] -914.1293


rmse <- function(error)
{
  sqrt(mean(error^2))
}

rmse(lr4$residuals)
# 0.1089887

######## Support Vector Machine Regression (SVM)

svm1 <- svm (y ~ ., data=md_log)
rmse(svm1$residuals)
# 0.07108316

# perform a grid search
tuneResult <- tune(svm, y ~ .,  data = md_log,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(-10:9))
)

print(tuneResult)

svm2 <- svm(y~.,epsilon=0.1,cost=10, data=md_log)
rmse(svm2$residuals)
# 0.02536014

######## Neural Network

my.grid <- expand.grid(.decay = c(0.5, 0.1, 0.01), .size = c(5,6,7,8,9,10))
nn.fit <- train(y/100 ~ ., data = md,
                      method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)    

nn1 <- nnet(y/100 ~ ., size=10, decay=0.1, data=md, maxit=1000)
nn1.predict <- predict(nn1)*100 
nn1_error <- as.data.frame(cbind(log(nn1.predict),md_log$y))
nn1_error$residual <- nn1_error[,2] - nn1_error[,1]
rmse(nn1_error$residual)
# 0.0474492723

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn1)

#####################################################################################
### 7-fold cross-validation

# Creating 10 equally size folds
folds <- cut(seq(1,nrow(md_log)),breaks=7,labels=FALSE)

# checking if the folds are of equal size
foldsize <- table(folds)

rm(model,temp, rmserror)
model <- vector(mode = "list", length = 3)
temp <- as.data.frame(matrix(0, nrow = max(foldsize), ncol = length(model)))
rmserror <- as.data.frame(matrix(0, nrow = 7, ncol = length(model)))

for(i in 1:7){
  
  rm(prediction,result,testindexes,testfold,trainfold,temp1)
  
  prediction <- data.frame()
  result <- data.frame()
  
  #Segement the training data by fold using the which() function 
  testindexes <- which(folds==i)
  testfold <- md_log[testindexes, ]
  trainfold <- md_log[-testindexes, ]
  
  testfold_nn <- md[testindexes, ]
  trainfold_nn <- md[-testindexes, ]
  
  
  # linear regression
  model[[1]] <- lm(y ~ region+roof_concrete+floor_mud+tapwater+electricity+lpg+tv+
                     phone+all_assets+sex_ratio+backward_pop+literacy, data=trainfold)
  
  # SVM
  model[[2]] <- svm(y~.,epsilon=0.1,cost=10, data=trainfold)
  
  # Neural Networks
  model[[3]] <- nnet(y/100 ~ ., size=10, decay=0.1, data=trainfold_nn, maxit=1000)
  
  for (j in 1:2) {
    temp1 <- as.data.frame(predict(model[[j]], testfold[,-1], type="response"))
    temp[,j] <- temp1
  }
  
  for (j in 3) {
    temp1 <- as.data.frame(predict(model[[j]], testfold_nn[,-1])*100)
    temp[,j] <- temp1
  }
  
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- temp
  result <- cbind(prediction, testfold[,1])
  names(result) <- c("LinearRegression","SVM","Neural Net","Actual")
  rmserror[i,1] <- rmse(result[,1]- result[,4])
  rmserror[i,2] <- rmse(result[,2]- result[,4])
  rmserror[i,3] <- rmse(log(result[,3])- result[,4])
}

summary(rmserror)

save.image (file = "IE590_Project.Rdata")

####### clustering (k-means)

# response variable
responseY <- md[,1]
# predictors
predictorX <- md[,-c(1,2)]

# PCA of the predictors
pca <- princomp(predictorX, cor=T)
summary(pca)
pc.comp <- pca$scores

pc.comp1 <- -1*pc.comp[,1]
pc.comp2 <- -1*pc.comp[,2]

X <- cbind(pc.comp1, pc.comp2)
cl <- kmeans(X,5)
plot(pc.comp1, pc.comp2,col=cl$cluster)
points(cl$centers, pch=16)

