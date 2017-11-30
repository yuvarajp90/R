# MARS is used when the relation between the response and predictor has non-linear

#install MARS package
install.packages('earth')
install.packages("plotmo")
install.packages("plotrix")
install.packages("TeachingDemos")

#load the package
library(plotrix)
library(TeachingDemos)
library(plotmo)
library(earth)

input<-read.table(file='sabattelli_mann.txt', header=T)
attach(input)

#Model fitting
##what is the difference between the four models below?
model1<-earth(TC~SST+NINO+NAO,degree=1,penalty=2)
summary(model1)
model2<-earth(TC~SST+NINO+NAO,degree=2,penalty=3)
summary(model2)

# penalty=0 doesn't penalize the terms itself only the knots
model3<-earth(TC~SST+NINO+NAO,degree=1,penalty=0)
summary(model3)

# R-sq decreases with increase in degrees

# penalty=-1 means no penalty, such that GCV = RSS/n
model4<-earth(TC~SST+NINO+NAO,degree=1,penalty=-1)
summary(model4)

# n-fold is the number of CV iterations
model5<-earth(TC~SST+NINO+NAO,degree=1,penalty=-1,nfold=10)

model6<-earth(TC~., data=input)
summary(model6)
# R-sq without any degree or penalty is the higher than when degree=2

model7<-earth(TC~.-Year,data=input)

model7$bx
model7$dir
model7$cuts

#model selection
error<-matrix(NA,ncol=2)
print(error)

modela<-earth(TC~SST+NINO+NAO,degree=1,penalty=2)
modelb<-earth(TC~SST+NINO+NAO,degree=1,penalty=-1)
a<-summary(modela)
b<-summary(modelb)

print(a)
print(b)
# without penalty, the MARS explains more variability in the data (high r-sq)

error[1]<-a[8]
error[2]<-b[8]
print(error)



###############################################################################

example <- function(degree, pred, parents)
{
  allowed.set = c(3,5)
  if (degree > 1 &&
        (all(pred != allowed.set) || any(parents[-allowed.set])))
    return(FALSE)
  TRUE
}
model8 <- earth(TC ~ ., data = data, degree = 2, allowed = example)
print(summary(model8))

###############################################################################

example2 <- function(degree, pred, parents)
{
  if (degree > 1 && (pred == 3 || parents[3]))
    return(FALSE)
  TRUE
}
model9 <- earth(TC ~ ., data = data, degree = 2, allowed = example2)
print(summary(model9))