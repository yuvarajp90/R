###Nonlinear Models
#========================================================
require(ISLR)
attach(Wage)

#Polynomials
#------------

#First we will use polynomials, and focus on a single predictor age:


fit=lm(wage~poly(age,4),data=Wage)
summary(fit)


#Lets make a plot of the fitted function, along with the standard errors of the fit.


agelims=range(age)

age.grid=seq(from=agelims[1],to=agelims[2])

preds=predict(fit,newdata=list(age=age.grid),se=TRUE)

se.bands=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se)

plot(age,wage,col="darkgrey")

lines(age.grid,preds$fit,lwd=2,col="blue")

matlines(age.grid,se.bands,col="blue",lty=2)


##Alternative

fita=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(fita)


plot(fitted(fit),fitted(fita))


fita=lm(wage~education,data=Wage)
fitb=lm(wage~education+age,data=Wage)
fitc=lm(wage~education+poly(age,2),data=Wage)
fitd=lm(wage~education+poly(age,3),data=Wage)
anova(fita,fitb,fitc,fitd)


### Polynomial logistic regression

#Now we fit a logistic regression model to a binary response variable, 
#constructed from `wage`. We code the big earners (`>250K`) as 1, else 0.


fit=glm(I(wage>250) ~ poly(age,3), data=Wage, family=binomial)

summary(fit)

preds=predict(fit,list(age=age.grid),se=T)

se.bands=preds$fit + cbind(fit=0,lower=-2*preds$se,upper=2*preds$se)

se.bands[1:5,]


#We have done the computations on the logit scale. To transform we need to apply the inverse logit
mapping 

prob.bands=exp(se.bands)/(1+exp(se.bands))
matplot(age.grid,prob.bands,col="blue",lwd=c(2,1,1),lty=c(1,2,2),type="l",ylim=c(0,.1))
points(jitter(age),I(wage>250)/10,pch="|",cex=.5)


###Splines
#-------
#Splines are more flexible than polynomials, but the idea is rather similar.
#Here we will explore cubic splines.


require(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
plot(age,wage,col="darkgrey")
lines(age.grid,predict(fit,list(age=age.grid)),col="darkgreen",lwd=2)
abline(v=c(25,40,60),lty=2,col="darkgreen")


#The smoothing splines does not require knot selection, but it does have a smoothing parameter,
#which can conveniently be specified via the effective degrees of freedom or `df`.


fit=smooth.spline(age,wage,df=16)
lines(fit,col="red",lwd=2)


#Or we can use LOO cross-validation to select the smoothing parameter for us automatically:


fit=smooth.spline(age,wage,cv=TRUE)
lines(fit,col="purple",lwd=2)


#Generalized Additive Models
---------------------------


require(gam)
gam1=gam(wage~s(age,df=4)+s(year,df=4)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam1,se=T)
gam2=gam(I(wage>250)~s(age,df=4)+s(year,df=4)+education,data=Wage,family=binomial)
plot(gam2)


#Lets see if we need a nonlinear terms for year


gam2a=gam(I(wage>250)~s(age,df=4)+year+education,data=Wage,family=binomial)

anova(gam2a,gam2,test="Chisq")



par(mfrow=c(1,3))
lm1=lm(wage~ns(age,df=4)+ns(year,df=4)+education,data=Wage)
plot.gam(lm1,se=T)






 

