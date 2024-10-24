
# library

library(tidyverse)
library(psych)
library(corrplot)
library(ggplot2)
library(GGally)
library(factoextra)
library(ggfortify)

## 1. Exploratory data analysis

dat = read.csv("/Users/seungji/Library/Mobile Documents/com~apple~CloudDocs/DU/Term2/DEVUL/Assignment2/MaunaLoa.csv")
dim(dat)
names(dat)
str(dat)
colSums(is.na(dat))
duplicated(dat)
table(duplicated(dat))
summary(dat)

# histogram
par(mfrow=c(2,3))
hist(dat$CO, main="CO")
hist(dat$CO2, main="CO2")
hist(dat$Methane, main="Methane")
hist(dat$NitrousOx, main="NitrousOx")
hist(dat$CFC11, main="CFC11")

# outliers
par(mfrow=c(1,5))
boxplot(dat$CO, main="CO")
boxplot(dat$CO2, main="CO2")
boxplot(dat$Methane, main="Methane")
boxplot(dat$NitrousOx, main="NitrousOx")
boxplot(dat$CFC11, main="CFC11")

# detecting outliers
CO_out = boxplot.stats(dat$CO)$out
CO_out_ind = which(dat$CO %in% c(CO_out))
dat[CO_out_ind,]
nrow(dat[CO_out_ind,])

CO2_out = boxplot.stats(dat$CO2)$out
CO2_out_ind = which(dat$CO2 %in% c(CO2_out))
dat[CO2_out_ind,]
nrow(dat[CO2_out_ind,])

Methane_out = boxplot.stats(dat$Methane)$out
Methane_out_ind = which(dat$Methane %in% c(Methane_out))
dat[Methane_out_ind,]
nrow(dat[Methane_out_ind,])

NitrousOx_out = boxplot.stats(dat$NitrousOx)$out
NitrousOx_out_ind = which(dat$NitrousOx %in% c(NitrousOx_out))
dat[NitrousOx_out_ind,]
nrow(dat[NitrousOx_out_ind,])

CFC11_out = boxplot.stats(dat$CFC11)$out
CFC11_out_ind = which(dat$CFC11 %in% c(CFC11_out))
dat[CFC11_out_ind,]
nrow(dat[CFC11_out_ind,])

# outliers remove
dat = dat[-c(131, 132, 133, 134, 135),]

# boxplot check again
par(mfrow=c(1,5))
boxplot(dat$CO, main="CO")
boxplot(dat$CO2, main="CO2")
boxplot(dat$Methane, main="Methane")
boxplot(dat$NitrousOx, main="NitrousOx")
boxplot(dat$CFC11, main="CFC11")

# histogram check again
par(mfrow=c(2,3))
hist(dat$CO, main="CO")
hist(dat$CO2, main="CO2")
hist(dat$Methane, main="Methane")
hist(dat$NitrousOx, main="NitrousOx")
hist(dat$CFC11, main="CFC11")

# normality test
shapiro.test(dat$CO)
shapiro.test(dat$CO2)
shapiro.test(dat$Methane)
shapiro.test(dat$NitrousOx)
shapiro.test(dat$CFC11)

# correlation
par(mfrow=c(1,1))
pairs.panels(dat[c(2:6)], method="pearson", hist.col="#00AFBB", density=TRUE, ellipses=TRUE)

## 2. Dimension Reduction

cor(dat[c(2:6)])
summary(dat[c(2:6)])
apply(dat[c(2:6)], 2, sd)
pr.out = prcomp(dat[c(2:6)], scale=TRUE)
pr.out$rotation # loading vectors
pr.out$x # scores
summary(pr.out)
fviz_screeplot(pr.out, addlabels=TRUE)

fviz_pca_ind(pr.out, axes=c(1,2), repel=TRUE, col.ind="blue")
autoplot(pr.out, data=dat[c(2:6)], label=TRUE)
fviz_pca_var(pr.out, axes=c(1,2), repel=TRUE, col.var="red")
fviz_pca_biplot(pr.out, axes=c(1,2), repel=TRUE, col.var="red")
fviz_contrib(pr.out, choice="var", axes=1, top=10)

## 3. Clustering
dat.scaled = scale(dat[,-1])

# K-means
fviz_nbclust(dat.scaled, kmeans, method="wss")
set.seed(123)
km.res = kmeans(dat.scaled, 3, nstart=25)
km.res
aggregate(dat[,-1], by=list(cluster=km.res$cluster), mean)
table(km.res$cluster)
fviz_cluster(km.res, dat[,-1], ellipse.type="norm")

# Hierarchical
dd = dist(dat.scaled, method="euclidean")
hc = hclust(dd, method="complete")
hc

dendros = as.dendrogram(hc)
plot(dendros, main="Complete linkage", ylab="Height", cex=.01)
abline(h=0.2, lty=2, col="red")
abline(h=0.428, lty=2, col="blue")
Hs = hc$height[(length(hc$height)-4):length(hc$height)]
abline(h=Hs, col=3, lty=2)

fviz_cluster(list(data=dat.scaled, cluster=cutree(hc, 3)), ellipse.type="norm")
fviz_dend(hc, k=3, cex=0.5, k_colors=c("#2E9FDF", "#00AFBB", "#E7B800"), color_labels_by_k=TRUE, ggtheme=theme_gray())

hcut = cutree(hc, k=3)
table(hcut)
aggregate(dat[,-1], by=list(cluster=hcut), mean)

