#############
###source: http://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/

set.seed(1)
#We want to use  the Boston dataset in the MASS package
#data about housing values in the suburbs of Boston
#goal is to predict the median value of owner-occupied homes (medv) using all the other continuous variables available
library(MASS)

data <- Boston

#make sure there are no missing values
apply(data,2,function(x) sum(is.na(x)))

##randomly splitting the data into a train and a test set. 
index <- sample(1:nrow(data),round(0.75*nrow(data)))

train <- data[index,]

test <- data[-index,]

then we fit a linear regression model and test it on the test set
lm.fit <- glm(medv~., data=train)

summary(lm.fit)

#How do the residuals look?
res<-residuals(lm.fit)
library(car)
qqPlot(res)

pr.lm <- predict(lm.fit,test)

MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)


###Preparing the data for neural nets
###data preprocessing

##We use the min-max method and scale the data in the interval [0,1]. Usually scaling in the intervals [0,1] or [-1,1] tends to give better results.

maxs <- apply(data, 2, max) 

mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train<- scaled[index,]

test<- scaled[-index,]

#load the library
library(neuralnet)

n <- names(train)

f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

#Let's have 2 hidden layers
# The hidden argument accepts a vector with the number of neurons for each hidden layer, while the argument linear.output is used to specify whether we want to do regression linear.output=TRUE or classification linear.output=FALSE

nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)

plot(nn)

#Let's make predictions
pr.nn <- compute(nn,test[,1:13])

##Remember that the net will output a normalized prediction, so we need to scale it back in order to make a meaningful comparison (or just a simple prediction).

pr.nn<- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn)^2)/nrow(test)


print(paste(MSE.lm,MSE.nn))



# # A visual approach to the performance of the network and the linear model on the test set is plotted below
par(mfrow=c(1,2))

plot(test$medv,pr.nn,col='red',main='Real vs predicted NN',pch=18,cex=0.7)

abline(0,1,lwd=2)

legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)

abline(0,1,lwd=2)

legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


###Let's plot them on the same graph
plot(test$medv,pr.nn,col='red',main='Real vs predicted NN',pch=18,cex=0.7)

points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)

abline(0,1,lwd=2)

legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


