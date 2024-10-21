red = read.table("/Users/seungji/Library/Mobile Documents/com~apple~CloudDocs/DU/Term2/ML/Assignment2/data/winequality-red.csv", sep=";", header=TRUE, na.strings="", stringsAsFactors=F)
white = read.table("/Users/seungji/Library/Mobile Documents/com~apple~CloudDocs/DU/Term2/ML/Assignment2/data/winequality-white.csv", sep=";", header=TRUE, na.strings="", stringsAsFactors=F)

## packages
library(ggplot2) # creating graphics
library(GGally) # extension to ggplot2
library(RColorBrewer) # color palettes
library(corrplot) # correlation
library(leaps) # find best subset
library(dplyr) # transforming datasets
library(rpart) # recursive and partitioning trees
library(rpart.plot)
library(plotly) # data visualization
library(caret) # classification and regression training
library(Metrics) # RMSE (masked from caret)
library(rattle) # statistical and visual summaries of data
library(randomForest) # random forest (to fit forests)

#### data preprocessing

## check missing value
sum(is.na(red))
sum(is.na(white))

## check duplicate rows: keep all the observations for more information
dim(red[duplicated(red), ])
dim(white[duplicated(white), ])

## using boxplot for check outliers: almost all variables have outliers

par(mfrow=c(1,5), oma=c(1,1,0,0)+0.1, mar=c(3,3,1,1)+0.1)
boxplot(red$fixed.acidity, col = "slategray2", pch=19)
mtext("red.fixed.acidity", cex=0.8, side=1, line=2)
boxplot(red$volatile.acidity, col = "slategray2", pch=19)
mtext("red.volatile.acidity", cex=0.8, side=1, line=2)
boxplot(red$citric.acid, col = "slategray2", pch=19)
mtext("red.citric.acid", cex=0.8, side=1, line=2)
boxplot(red$residual.sugar, col = "slategray2", pch=19)
mtext("red.residual.sugar", cex=0.8, side=1, line=2)
boxplot(red$chlorides, col = "slategray2", pch=19)
mtext("red.chlorides", cex=0.8, side=1, line=2)

par(mfrow=c(1,6), oma=c(1,1,0,0)+0.1, mar=c(3,3,1,1)+0.1)
boxplot(red$free.sulfur.dioxide, col = "slategray2", pch=19)
mtext("red.free.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(red$total.sulfur.dioxide, col = "slategray2", pch=19)
mtext("red.total.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(red$density, col = "slategray2", pch=19)
mtext("red.density", cex=0.8, side=1, line=2)
boxplot(red$pH, col = "slategray2", pch=19)
mtext("red.pH", cex=0.8, side=1, line=2)
boxplot(red$sulphates, col = "slategray2", pch=19)
mtext("red.sulphates", cex=0.8, side=1, line=2)
boxplot(red$alcohol, col = "slategray2", pch=19)
mtext("red.alcohol", cex=0.8, side=1, line=2)

par(mfrow=c(1,5), oma=c(1,1,0,0)+0.1, mar=c(3,3,1,1)+0.1)
boxplot(white$fixed.acidity, col = "slategray2", pch=19)
mtext("white.fixed.acidity", cex=0.8, side=1, line=2)
boxplot(white$volatile.acidity, col = "slategray2", pch=19)
mtext("white.volatile.acidity", cex=0.8, side=1, line=2)
boxplot(white$citric.acid, col = "slategray2", pch=19)
mtext("white.citric.acid", cex=0.8, side=1, line=2)
boxplot(white$residual.sugar, col = "slategray2", pch=19)
mtext("white.residual.sugar", cex=0.8, side=1, line=2)
boxplot(white$chlorides, col = "slategray2", pch=19)
mtext("white.chlorides", cex=0.8, side=1, line=2)

par(mfrow=c(1,6), oma=c(1,1,0,0)+0.1, mar=c(3,3,1,1)+0.1)
boxplot(white$free.sulfur.dioxide, col = "slategray2", pch=19)
mtext("white.free.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(white$total.sulfur.dioxide, col = "slategray2", pch=19)
mtext("white.total.sulfur.dioxide", cex=0.8, side=1, line=2)
boxplot(white$density, col = "slategray2", pch=19)
mtext("white.density", cex=0.8, side=1, line=2)
boxplot(white$pH, col = "slategray2", pch=19)
mtext("white.pH", cex=0.8, side=1, line=2)
boxplot(white$sulphates, col = "slategray2", pch=19)
mtext("white.sulphates", cex=0.8, side=1, line=2)
boxplot(white$alcohol, col = "slategray2", pch=19)
mtext("white.alcohol", cex=0.8, side=1, line=2)

## add category and merge datasets
red['color'] = 'red'
white['color'] = 'white'
data = rbind(red, white)

## check data
head(data)
tail(data)
dim(data)
names(data)
summary(data)
str(data)

## quality: picks between 5 and 6 (normal distribution)
summary(data$quality)
table(data$quality)
qplot(quality, data=data, fill=color, binwidth=1) +
  scale_x_continuous(breaks=seq(3,10,1), lim=c(3,10)) +
  scale_y_sqrt()

## check correlation
ggpairs(data)
ggcorr(data, label=T, label_round=2)
# quality- alcohol: 0.444
# density - alcohol: -0.687
# density - fixed.acidity: 0.459
# residual sugar - density: 0.553
# residual sugar - total.sulfur.dioxide: 0.495


#### Modeling

## transforming 'quality' from an int to factor & dividing 'quality'
data$quality = ifelse(data$quality<5, 'bad', ifelse(data$quality<7, 'normal', 'good'))
data$quality = as.factor(data$quality)
str(data$quality)
table(data$quality)
prop.table(table(data$quality))

## create random sample (with ratio 80:20)
set.seed(2024)
train_sample = sample(nrow(data), 0.8*nrow(data))
data_train = data[train_sample,]
data_test = data[-train_sample,]
table(data_train$quality)
table(data_test$quality)
prop.table(table(data_train$quality))
prop.table(table(data_test$quality))

### train model: decision tree
data_model = rpart(quality ~ ., data=data_train)
summary(data_model)
fancyRpartPlot(data_model) # visualize decision trees
plot(data_model, uniform=TRUE, branch=0.6, margin=0.1) # visualize classification tree
text(data_model, all=TRUE, use.n=TRUE)

## cost complexity parameters
plotcp(data_model)

## model evaluation using test data
data_predict = predict(data_model, data_test, type="class")
table(data_test$quality, data_predict)

## performance measure (Accuracy: 0.7654)
confusionMatrix(table(data_predict, data_test$quality))

### pruning
data_model$cptable
min(data_model$cptable[,"xerror"])
which.min(data_model$cptable[,"xerror"])

## get cp with the minimum xerror
data_model_CP = data_model$cptable[5, "CP"]
data_model_CP

## prune tree
prune_tree = prune(data_model, cp=data_model_CP)
plot(prune_tree, margin=0.05) # visualize classification tree
text(prune_tree, all=TRUE, use.n=TRUE)

# classification table based on the pruned classification tree model (same Accuracy: 0.7654)
prune_predict = predict(prune_tree, data_test, type="class")
table(data_test$quality, prune_predict)
confusionMatrix(table(prune_predict, data_test$quality))

### train model: random forest
data_model2 = randomForest(quality ~ ., data=data_train)
data_model2

## error rate of random forest
plot(data_model2)

# variable importance check
importance(data_model2)
varImpPlot(data_model2)

## model evaluation using test data
data_predict2 = predict(data_model2, newdata=data_test)
table(data_test$quality, data_predict2)

## performance measure (Accuracy: 0.86)
confusionMatrix(table(data_predict2, data_test$quality))

### train model: random forest(2) - adjust mtry (Accuracy:0.8546)
data_model3 = randomForest(quality ~ ., data=data_train, mtry=2)
data_model3
data_predict3 = predict(data_model3, newdata=data_test)
table(data_test$quality, data_predict3)
confusionMatrix(table(data_predict3, data_test$quality))

### train model: random forest(3) - adjust tree (Accuracy: 0.8546)
data_model4 = randomForest(quality ~ ., data=data_train, ntree=200)
data_model4
data_predict4 = predict(data_model4, newdata=data_test)
table(data_test$quality, data_predict4)
confusionMatrix(table(data_predict4, data_test$quality))

### train model: random forest(4) - adjust both tree and mtry (Accuracy: 0.8569)
data_model5 = randomForest(quality ~ ., data=data_train, ntree=200, mtry=2)
data_model5
data_predict5 = predict(data_model5, newdata=data_test)
table(data_test$quality, data_predict5)
confusionMatrix(table(data_predict5, data_test$quality))


### model comparison

## confusion matrix
# decision tree
confusionMatrix(table(data_predict, data_test$quality))
# random forest
confusionMatrix(table(data_predict2, data_test$quality))

## prepare
dt_cm = as.matrix(confusionMatrix(table(data_predict, data_test$quality)))
rf_cm = as.matrix(confusionMatrix(table(data_predict2, data_test$quality)))

dt_n = sum(dt_cm)
rf_n = sum(rf_cm)

dt_diag = diag(dt_cm)
rf_diag = diag(rf_cm)

dt_rowsums = apply(dt_cm, 1, sum)
rf_rowsums = apply(rf_cm, 1, sum)

dt_colsums = apply(dt_cm, 2, sum)
rf_colsums = apply(rf_cm, 2, sum)

dt_p = dt_rowsums / dt_n
rf_p = rf_rowsums / rf_n

dt_q = dt_colsums / dt_n
rf_q = rf_colsums / rf_n

## accuracy
dt_accuracy = sum(dt_diag) / dt_n
rf_accuracy = sum(rf_diag) / rf_n

## precision
dt_precision = dt_diag / dt_rowsums
rf_precision = rf_diag / rf_rowsums

## recall
dt_recall = dt_diag / dt_colsums
rf_recall = rf_diag / rf_colsums

## f1
dt_f1 = 2*dt_precision*dt_recall / dt_precision+dt_recall
rf_f1 = 2*rf_precision*rf_recall / rf_precision+rf_recall

## per-class metrics
data.frame(dt_accuracy, mean(dt_precision), mean(dt_recall), mean(dt_f1))
data.frame(rf_accuracy, mean(rf_precision), mean(rf_recall), mean(rf_f1))
