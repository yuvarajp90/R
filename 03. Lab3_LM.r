
# Linear Models
install.packages('gamair')
library(gamair)
data(hubble)
attach(hubble)

# set the names of the columns
colnames(hubble)<-c("Galaxy", 'velocity','distance')

attach(hubble)

#velocity=beta1*distance+error
# what does this measure represent?
sum(hubble$distance*hubble$velocity)/sum(hubble$distance^2)

plot(velocity~distance,data=hubble,pch=25,col='blue')
plot(distance~velocity,data=hubble,pch=19,col='blue')

# linear model (-1 means no intercept)
hmod<-lm(velocity~distance-1,data=hubble)

# prints the model coefficients
coef(hmod)

# matrix creates a matrix with the elements 1 to 2 in 2 columns
# what is the purpose of layout
layout(matrix(1:2,ncol=2))

plot(velocity~distance,data=hubble, pch=19, col='red')
# shows some warnings, why?
plot(velocity, distance,data=hubble, pch=19, col='red')

# adds a regression line through the current plot
abline(hmod,lty=2, lwd=3,col='blue')

# hmod is the linear regression result
# which=1 plots just the first one i.e. residual vs predicted
plot(hmod,which=1)

#returns the residuals
resid<-residuals(hmod)

# returns the fitted/predicted values
fit<-fitted(hmod)

# i dont get why this plot returns a blank ?
plot(fit, resid, xlab='fitted',ylab='residuals', pch= 19, type='n')
abline(h=0, lyt=2)
text(fit,resid, labels=rownames(hubble),col='blue',font=1)

qqnorm(resid, ylab='residuals')
qqline(resid)
cook = cooks.distance(hmod)

