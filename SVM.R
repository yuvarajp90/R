
##Let's generate the data
set.seed (1)
x=matrix (rnorm (20*2) , ncol =2)
y=c(rep (-1,10) , rep (1 ,10) )
x[y==1,]= x[y==1,] + 1


##We begin by checking whether the classes are linearly separable.
plot(x, col =(3-y))

##They are not. Next, we fit the support vector classifier. Note that in order
##for the svm() function to perform classification (as opposed to SVM-based
##regression), we must encode the response as a factor variable. We now
##create a data frame with the response coded as a factor.
dat=data.frame(x=x, y=as.factor(y))

library (e1071)

# A cost  argument allows us to specify the cost of a violation to the margin. When the cost  argument is small, then the margins will be wide and many support vectors will be on the margin or will violate the margin. When the cost  argument is large, then the margins will be narrow and there will be few support vectors on the margin or violating the margin.

svmfit =svm (y~., data=dat ,kernel ="linear", cost =10,scale =T)

plot(svmfit , dat)

##The support vectors are plotted as crosses and the remaining observations are plotted as circles; we see here that there are seven support vectors.

svmfit$index

summary (svmfit )

# What if we instead used a smaller value of the cost parameter?
 
svmfit =svm(y~., data=dat , kernel ="linear", cost =0.1, scale =T)

plot(svmfit , dat)

svmfit$index


##The e1071 library includes a built-in function, tune(), to perform crosstune() validation. By default, tune()  performs ten-fold cross-validation on a set of models of interest.
 
set.seed (1)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear",ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100), gamma=seq(0,10,0.5)))

summary (tune.out)

bestmod =tune.out$best.model

summary (bestmod)


xtest=matrix (rnorm (20*2) , ncol =2)

ytest=sample (c(-1,1) , 20, rep=TRUE)

xtest[ytest ==1 ,]= xtest[ytest ==1,] + 1

testdat =data.frame (x=xtest , y=as.factor (ytest))

ypred=predict (bestmod ,testdat)

table(predict =ypred , truth= testdat$y )

##What if we had instead used cost=0.01 ?
svmfit =svm(y~., data=dat , kernel ="linear", cost =.01,scale =FALSE )

ypred=predict (svmfit ,testdat )

table(predict =ypred , truth= testdat$y )

x[y==1 ,]= x[y==1 ,]+0.5

plot(x, col =(y+5) /2, pch =19)

dat=data.frame(x=x,y=as.factor (y))

svmfit =svm(y~., data=dat , kernel ="linear", cost =1e5)

summary (svmfit )

svmfit =svm(y~., data=dat , kernel ="linear", cost =1)

summary (svmfit )

plot(svmfit ,dat )

set.seed (1)

x=matrix (rnorm (200*2) , ncol =2)
x[1:100 ,]=x[1:100 ,]+2

x[101:150 ,]= x[101:150 ,] -2

y=c(rep (1 ,150) ,rep (2 ,50) )

dat=data.frame(x=x,y=as.factor (y))

plot(x, col=y)

train=sample (200 ,100)

svmfit =svm(y~., data=dat [train ,], kernel ="radial", gamma =1,cost =1)

plot(svmfit , dat[train ,])

summary (svmfit )

svmfit =svm(y~., data=dat [train ,], kernel ="radial",gamma =1,cost=1e5)

plot(svmfit ,dat [train ,])

set.seed (1)

tune.out=tune(svm,y~., data=dat[train ,], kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4) ))

summary (tune.out)

svmfit =svm(y~., data=dat [train ,], kernel ="radial",gamma =1,cost=1e5)

plot(svmfit ,dat [train ,])

set.seed (1)

tune.out=tune(svm, y~., data=dat[train ,], kernel ="radial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma=c(0.5,1,2,3,4) ))
summary (tune.out)

table(true=dat[-train ,"y"], pred=predict (tune.out$best.model ,newx=dat[-train ,]))

library (ROCR)

rocplot =function (pred , truth , ...)
{predob = prediction (pred , truth )
	perf = performance (predob , "tpr", "fpr")
	plot(perf ,...)}
	
svmfit.opt=svm(y~.,data=dat[train ,], kernel ="radial",gamma =2, cost=1, decision.values =T)

fitted =attributes(predict (svmfit.opt,dat[train,], decision.values =TRUE))$decision.values

par(mfrow =c(1,2))

rocplot(fitted,dat[train,'y'], main="Training Data")

svmfit.flex=svm (y~., data=dat[train ,], kernel ="radial",gamma =50, cost=1, decision.values =T)

fitted =attributes(predict (svmfit.flex ,dat[train ,], decision.values =T))$decision.values

rocplot(fitted,dat [train,"y"], add =T,col ="red")

fitted =attributes(predict (svmfit.opt ,dat[-train,], decision.values =T))$decision.values

rocplot (fitted ,dat [-train ,"y"], main ="Test Data")

fitted =attributes (predict (svmfit.flex ,dat[-train ,], decision.values =T))$decision.values

rocplot (fitted ,dat [-train ,"y"], add=T,col =" red")

######SVM with Multiple Classes
set.seed (1)
x=rbind(x,matrix(rnorm (50*2),ncol =2))
y=c(y, rep (0 ,50) )
x[y==0 ,2]= x[y==0 ,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow =c(1,1))
plot(x,col =(y+1))

svmfit =svm(y~.,data=dat,kernel ="radial",cost =10, gamma =1)
plot(svmfit,dat)
library (ISLR)

names(Khan)

dim( Khan$xtrain)

dim( Khan$xtest)

length (Khan$ytrain)

length (Khan$ytest)


table(Khan$ytrain)

table(Khan$ytest)
dat=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out=svm(y~., data=dat,kernel ="linear",cost =10)
summary(out)
table(out$fitted,dat$y)


