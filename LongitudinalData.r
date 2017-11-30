
data("BtheB", package = "HSAUR2") 

BtheB$subject <- factor(rownames(BtheB)) 

nobs <- nrow(BtheB) 

BtheB_long <- reshape(BtheB, idvar = "subject", varying = c("bdi.2m", "bdi.3m", "bdi.5m", "bdi.8m"), direction = "long") 

BtheB_long$time <- rep(c(2, 3, 5, 8), rep(nobs, 4)) 

subset(BtheB_long, subject %in% c("1", "2", "3"))

layout(matrix(1:2, ncol = 2)) 

ylim=range(BtheB[,grep('bdi',names(BtheB))], na.rm=T)
tau<-subset(BtheB,treatment=='TAU')[,grep('bdi',names(BtheB))]
boxplot(tau,main='Treated as Usual',ylab='BDI',xlab="Time (in months)", names=c(0,2,3,5,8),ylim=ylim)

btheb<-subset(BtheB, treatment=="BtheB")[,grep('bdi',names(BtheB))]
boxplot(btheb,main="Beat the Blues", ylab='BDI',xlab='Time (in months)', names=c(0,2,3,5,8), ylim=ylim)

subset(BtheB_long,subject%in%c('1','2','3'))

library("lme4")
BtheB_lmer1 <- lmer(bdi ~ bdi.pre + time + treatment + drug + length + (1 | subject), data = BtheB_long, REML = FALSE, na.action = na.omit) 

BtheB_lmer2 <- lmer(bdi ~ bdi.pre + time + treatment + drug + length + (time | subject), data = BtheB_long, REML = FALSE, na.action = na.omit) 

anova(BtheB_lmer1, BtheB_lmer2)

summary(BtheB_lmer1)

install.packages('multcomp')
library(multcomp)
cftest(BtheB_lmer1)

layout(matrix(1:2, ncol = 2)) 
qint <- ranef(BtheB_lmer1)$subject[["(Intercept)"]] 
qres <- residuals(BtheB_lmer1) 
qqnorm(qint, ylab = "Estimated random intercepts",xlim = c(-3, 3), ylim = c(-20, 20), main = "Random intercepts") 
qqline(qint) 

qqnorm(qres, xlim = c(-3, 3), ylim = c(-20, 20), ylab = "Estimated residuals", main = "Residuals") 
qqline(qres)

bdi <- BtheB[, grep("bdi", names(BtheB))] 
 
plot(1:4, rep(-0.5, 4), type = "n", axes = FALSE, ylim = c(0, 50), xlab = "Months", ylab = "BDI") 
axis(1, at = 1:4, labels = c(0, 2, 3, 5)) 
axis(2) 

for (i in 1:4) {
	dropout <- is.na(bdi[,i + 1])
	points(rep(i, nrow(bdi))+ifelse(dropout, 0.05, -0.05),jitter(bdi[,i]), pch = ifelse(dropout, 20, 1))}