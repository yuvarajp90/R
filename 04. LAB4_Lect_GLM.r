##Excercise #1: red blood cells sedinemtation rate- develop a diagnostic model

#ESR: red blood cells sedimentation rate
#absolute value of ESR: non-important
##ESR<20mm/hour: healthy individual

getwd()

library("HSAUR2")

# loading the data without first calling the library stores it as a variable
# not as a dataframe
data("plasma",package='HSAUR2')

# what does this
layout(matrix(1:2, ncol=2))

# cdplot is conditional density plot
# It is the probability of x-axis variables falling in the y-axis categories
cdplot(ESR~fibrinogen, data=plasma,col=c(2,3),border=1)
cdplot(ESR~globulin, data=plasma)

# runs a glm model with error distribution = binomial and link = logit
glm1<-glm(ESR~fibrinogen,data=plasma, family=binomial())

# returns the confidence interval
confint(glm1, parm='fibrinogen')
summary(glm1)

# exponential transformation of the glm coefficients to understand their meaning
# exponential since the link function of binomial is logit
exp(coef(glm1))['fibrinogen']
exp(confint(glm1, parm='fibrinogen'))

glm2<-glm(ESR~fibrinogen+globulin,data=plasma, family=binomial())
summary(glm2)

# run anova test to check if the difference between the 2 model
# results are different
anova(glm1,glm2, test="Chisq")

# calculate the predicted values for the 2nd model using the glm coefficients
prob <- predict(glm2, type = "response")


plot(globulin~fibrinogen,data=plasma,xlim=c(2,6), col='blue',ylim=c(25,55), pch='*')

# how to interpret this plot
symbols(plasma$fibrinogen,plasma$globulin,circles=prob, add=T)


################
#Excercise #2: #data on colonic polyps
data("polyps", package = 'HSAUR2')

polyps_glm_1 <- glm(number ~ treat + age, data = polyps,family=poisson())

polyps_glm_2 <- glm(number ~ treat + age, data = polyps, family = quasipoisson()) 

summary(polyps_glm_2)