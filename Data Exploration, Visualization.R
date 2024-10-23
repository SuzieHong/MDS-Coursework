
load(file.choose())

## Question 1

head(plastic)
summary(plastic)
summary(plastic$PWaste)
summary(plastic$MPWaste)

par(mfrow=c(1,2))
hist(plastic$PWaste, main='Histogram of Plastic Waste')
hist(plastic$MPWaste, main='Histogram of Mismanaged Plastic Waste')
b1 = boxplot(plastic$PWaste, main='PWaste', main='Boxplot of Plastic Waste')
b2 = boxplot(plastic$MPWaste, main='MPWaste', main='Boxplot of Mismanaged Plastic Waste')

b1$stats
b2$stats

# print outliers
PWaste_out = boxplot.stats(plastic$PWaste)$out
PWaste_out_ind = which(plastic$PWaste %in% c(PWaste_out))
plastic[PWaste_out_ind,]
nrow(plastic[PWaste_out_ind,])

MPWaste_out = boxplot.stats(plastic$MPWaste)$out
MPWaste_out_ind = which(plastic$MPWaste %in% c(MPWaste_out))
plastic[MPWaste_out_ind,]
nrow(plastic[MPWaste_out_ind,])

# print rows which have same value
subset(plastic, plastic$PWaste == plastic$MPWaste)
nrow(subset(plastic, plastic$PWaste == plastic$MPWaste))

# find location of missing values
sum(is.na(plastic$PWaste))
plastic[which(is.na(plastic$PWaste)),]

sum(is.na(plastic$MPWaste))
plastic[which(is.na(plastic$MPWaste)),]

## Question 2

par(mfrow=c(1,1))
xtabs(PWaste~Region, data=plastic)
mosaicplot(xtabs(PWaste~Region, data=plastic), col=2:8, las=2, main='Region & Plastic Waste')

par(mfrow=c(1,2))
barplot(xtabs(PWaste~Region, data=plastic), col=2:8, las=2, main='Region & Plastic Waste')
barplot(xtabs(MPWaste~Region, data=plastic), col=2:8, las=2, main='Region & Mismanaged Plastic Waste')
barplot(xtabs(PWaste~IncomeStatus, data=plastic), col=2:8, main='Income Status & Plastic Waste')
barplot(xtabs(MPWaste~IncomeStatus, data=plastic), col=2:8, main='Income Status & Mismanaged Plastic Waste')

## Question 3

par(mfrow=c(1,1))
plot(plastic$PWaste, plastic$MPWaste, xlab = 'Pwaste', ylab='MPWaste', main='Plastic Waste & Mismanaged Plastic Waste')

plot(plastic$Region, plastic$IncomeStatus, las=2, col=2:5, main='Region & Income Status')

## Question 4

pairs(plastic[c(4,6,7,8,9,10)], pch=16)

plastic[which.max(plastic$PWaste), "Country"]
plastic[which.max(plastic$MPWaste), "Country"]

plastic[which.min(plastic$PWaste), "Country"]
plastic[which.min(plastic$MPWaste), "Country"]


library(scales)
colours = alpha(rep('black', length=nrow(plastic[c(4,6,7,8,9,10)])), 0.5)
colours[which.max(plastic$PWaste)] = 'red'
colours[which.max(plastic$MPWaste)] = 'blue'
colours[which.min(plastic$PWaste)] = 'green'
colours[which.min(plastic$MPWaste)] = 'purple'

pairs(plastic[c(4,6,7,8,9,10)], col=colours, pch=16)

library(lattice)
parallelplot(plastic[c(4,6,7,8,9,10)], horizontal=FALSE, col=colours)

# corplot
install.packages("corrplot")
library(corrplot)
corrplot(cor(plastic[c(4,6,7,8,9,10)]))
cor(plastic$UrbanPopPC, plastic$GDP)
cor(plastic$UrbanPopPC, plastic$CoastalPopPC)
cor(plastic$CoastalPopPC, plastic$GDP)

## Question 5

plot(x=plastic$GDP, y=plastic$MPWaste, xlab='GDP', ylab='MPWaste', pch=16)

fit <- ksmooth(x=plastic$GDP, y=plastic$MPWaste, kernel = "box", bandwidth = 5)
lines(x=fit$x, y=fit$y, col="red", lwd=3)
fit <- ksmooth(x=plastic$GDP, y=plastic$MPWaste, kernel = "box", bandwidth = 10)
lines(x=fit$x, y=fit$y, col="blue", lwd=3)
fit <- ksmooth(x=plastic$GDP, y=plastic$MPWaste, kernel = "box", bandwidth = 1)
lines(x=fit$x, y=fit$y, col="green", lwd=3)

# local regression and loess

lfit <- loess(MPWaste~GDP, data=plastic, span=0.66) 
xs <- seq(min(plastic$GDP), max(plastic$GDP), length=200)
lpred <- predict(lfit, data.frame(GDP=xs), se = TRUE) 

plot(x=plastic$GDP, y=plastic$MPWaste, xlab="GDP", ylab="Plastic Waste", pch=16) 
lines(x=xs, y=lpred$fit, col='red',lwd=4) 

upr <- lpred$fit + 1.96*lpred$se.fit ## upper limit of 95% CI
lwr <- lpred$fit - 1.96*lpred$se.fit ## lower limit of 95% CI
plot(x=plastic$GDP, y=plastic$MPWaste, xlab="GDP", ylab="Plastic Waste", pch=16)
lines(x=xs, y=lpred$fit,col='red',lwd=4)
lines(x=xs, y=upr,col='red',lwd=1, lty=2)
lines(x=xs, y=lwr,col='red',lwd=1, lty=2)

install.packages("scales")
library(scales)

polygon(x=c(xs,rev(xs)), 
        y=c(upr,rev(lwr)),
        col=alpha('red',0.2), border=NA)

#-----#

par(mfrow=c(2,2))

lfit <- loess(GDP~PWaste, data=plastic, span=0.66) 
xs <- seq(min(plastic$PWaste, na.rm=TRUE), max(plastic$PWaste, na.rm=TRUE), length=200)
lpred <- predict(lfit, data.frame(PWaste=xs), se = TRUE) 

plot(x=plastic$PWaste, y=plastic$GDP, xlab="Plastic Waste", ylab="GDP", pch=16) 
lines(x=xs, y=lpred$fit, col='red',lwd=4) 

upr <- lpred$fit + 1.96*lpred$se.fit ## upper limit of 95% CI
lwr <- lpred$fit - 1.96*lpred$se.fit ## lower limit of 95% CI
#plot(x=plastic$PWaste, y=plastic$GDP, xlab="Plastic Waste", ylab="GDP", pch=16)
lines(x=xs, y=lpred$fit,col='red',lwd=4)

library(scales) 
polygon(x=c(xs,rev(xs)), 
        y=c(upr,rev(lwr)),
        col=alpha('red',0.2), border=NA)

#-----#

lfit <- loess(UrbanPopPC~PWaste, data=plastic, span=0.66) 
xs <- seq(min(plastic$PWaste, na.rm=TRUE), max(plastic$PWaste, na.rm=TRUE), length=200)
lpred <- predict(lfit, data.frame(PWaste=xs), se = TRUE) 

plot(x=plastic$PWaste, y=plastic$UrbanPopPC, xlab="Plastic Waste", ylab="Urban Population", pch=16) 
lines(x=xs, y=lpred$fit, col='red',lwd=4) 

upr <- lpred$fit + 1.96*lpred$se.fit ## upper limit of 95% CI
lwr <- lpred$fit - 1.96*lpred$se.fit ## lower limit of 95% CI
#plot(x=plastic$PWaste, y=plastic$UrbanPopPC, xlab="Plastic Waste", ylab="Urban Population", pch=16)
lines(x=xs, y=lpred$fit,col='red',lwd=4)

library(scales) 
polygon(x=c(xs,rev(xs)), 
        y=c(upr,rev(lwr)),
        col=alpha('red',0.2), border=NA)

#-----#

lfit <- loess(GDP~MPWaste, data=plastic, span=0.66) 
xs <- seq(min(plastic$MPWaste, na.rm=TRUE), max(plastic$MPWaste, na.rm=TRUE), length=200)
lpred <- predict(lfit, data.frame(MPWaste=xs), se = TRUE) 

plot(x=plastic$MPWaste, y=plastic$GDP, xlab="Mismanaged Plastic Waste", ylab="GDP", pch=16) 
lines(x=xs, y=lpred$fit, col='red',lwd=4) 

upr <- lpred$fit + 1.96*lpred$se.fit ## upper limit of 95% CI
lwr <- lpred$fit - 1.96*lpred$se.fit ## lower limit of 95% CI
#plot(x=plastic$MPWaste, y=plastic$GDP, xlab="Mismanaged Plastic Waste", ylab="GDP", pch=16)
lines(x=xs, y=lpred$fit,col='red',lwd=4)

library(scales) 
polygon(x=c(xs,rev(xs)), 
        y=c(upr,rev(lwr)),
        col=alpha('red',0.2), border=NA)

#-----#

lfit <- loess(UrbanPopPC~MPWaste, data=plastic, span=0.66) 
xs <- seq(min(plastic$MPWaste, na.rm=TRUE), max(plastic$MPWaste, na.rm=TRUE), length=200)
lpred <- predict(lfit, data.frame(MPWaste=xs), se = TRUE) 

plot(x=plastic$MPWaste, y=plastic$UrbanPopPC, xlab="Mismanaged Plastic Waste", ylab="Urban Population", pch=16) 
lines(x=xs, y=lpred$fit, col='red',lwd=4) 

upr <- lpred$fit + 1.96*lpred$se.fit ## upper limit of 95% CI
lwr <- lpred$fit - 1.96*lpred$se.fit ## lower limit of 95% CI
#plot(x=plastic$MPWaste, y=plastic$UrbanPopPC, xlab="Mismanaged Plastic Waste", ylab="Urban Population", pch=16)
lines(x=xs, y=lpred$fit,col='red',lwd=4)

library(scales) 
polygon(x=c(xs,rev(xs)), 
        y=c(upr,rev(lwr)),
        col=alpha('red',0.2), border=NA)



















