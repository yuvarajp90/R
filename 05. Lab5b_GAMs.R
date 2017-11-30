library(HSAUR2)

# load the dataset "men1500m" in HSAUR2 library
data("men1500m", package="HSAUR2")

names(men1500m)

# plot the distribution of winners time by year
plot(time~year,data=men1500m, pch="*", col='orange')

# subsetting only those who won after 1899
men1500m1900 <- subset(men1500m, year >= 1900) 

# running a ols model to see the relation between time and year
men1500m_lm <- lm(time ~ year, data = men1500m1900) 

coefficients(men1500m_lm)

# finding the confidence interval of the predicted time for 2008 & 2012
predict(men1500m_lm, newdata = data.frame(year = c(2008, 2012)), interval = "confidence")

# whats different from the previous plot is this time only years >=1900 are considered
plot(time ~ year, data = men1500m1900) 
abline(men1500m_lm)

# creating two temp datasets
x <- men1500m1900$year 
y <- men1500m1900$time 

#  Lowess creates a smooth line through a timeplot or scatter plot 
# to help see the relationship between variables and foresee trends

men1500m_lowess <- lowess(x, y) 
plot(time ~ year, data = men1500m1900)
lines(men1500m_lowess, lty = 2) 


library(nlme)
library(mgcv)

# s applies cube root smoothing on x 
men1500m_cubic <- gam(y ~ s(x, bs = "cr")) 

# comparing lowess with gam
lines(x, predict(men1500m_cubic), lty = 3)

# another alternative is to see if instead of GAM, if a 2nd degree polynomial
# explains the interaction better

men1500m_lm2<- lm(time ~ year + I(year^2), data = men1500m1900) 
predict(men1500m_lm2,  newdata = data.frame(year = c(2008, 2012)),  interval = "confidence")
plot(time~year, data=men1500m1900)
lines(men1500m1900$year,predict(men1500m_lm2))

library(parallel)
library(stabs)
library("mboost") 
USair_boost <- gamboost(SO2 ~ ., data = USairpollution) 
USair_aic <- AIC(USair_boost) 
USair_aic

USair_gam <- USair_boost[mstop(USair_aic)]

layout(matrix(1:6, ncol = 3))

plot(USair_gam, ask = FALSE)

SO2hat <- predict(USair_gam) 

SO2 <- USairpollution$SO2 

plot(SO2hat, SO2-SO2hat, type = "n", xlim = c(0, 110)) 

text(SO2hat, SO2-SO2hat, labels=rownames(USairpollution),  adj=0) 

abline(h = 0, lty = 2, col = "grey")

layout(matrix(1:3, nrow = 1))
library(rpart)
spineplot(Kyphosis ~ Age, data = kyphosis, ylevels = c("present", "absent")) 
spineplot(Kyphosis ~ Number, data = kyphosis,ylevels = c("present", "absent")) 
spineplot(Kyphosis ~ Start, data = kyphosis, ylevels = c("present", "absent"))

kyphosis_gam <- gam(Kyphosis ~ s(Age, bs = "cr") + s(Number, bs = "cr", k = 3) + s(Start, bs = "cr", k = 3), family = binomial, data = kyphosis)
 
kyphosis_gam

trans <- function(x) + binomial()$linkinv(x) 

layout(matrix(1:3, nrow = 1))

plot(kyphosis_gam, select = 1, shade = TRUE, trans = trans) 

plot(kyphosis_gam, select = 2, shade = TRUE, trans = trans) 

plot(kyphosis_gam, select = 3, shade = TRUE, trans = trans)

gamchk <- gam(SeriousDlqin2yrs ~ s(age, bs="cr") + s(expense, b="cs"), family = binomial, data = tuningdata)
