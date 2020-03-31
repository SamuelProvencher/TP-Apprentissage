source("script.R")

library(randomForest)
library(pROC)

#### Bagging ####
set.seed(69)
bag <-randomForest(I(log(price))~., data = donnees.train, proximity=TRUE, ntree=100,importance=TRUE) 
bag
bag$mse
plot(1:length(bag$mse),bag$mse, type="l", xlab="B", ylab="Erreur quadratique moyenne", main="Bagging")
varImpPlot(bag)
bag_prev <- predict(bag, newdata=donnees.test, type="response")
#bag.res <- roc(log(donnees.test$price), bag_prev, auc=TRUE, plot=TRUE)
#bag.res$auc
(EQM.bag <- sum((bag_prev-log(donnees.test$price))^2)/length(donnees.test))

#### Foret aleatoire ####
# à ne pas rouler, très long, les résultats sont en bas
#oob.values <- vector(length=10)
#for(i in 1:15) {
#    temp.model <- randomForest(I(log(price)) ~ ., data=donnees.train, mtry=i, proximity=TRUE, 
#                               ntree=50,importance=TRUE)
#    oob.values[i] <- temp.model$mse[length(temp.model$mse)]
#}
# 0.06343711 0.03733935 0.03405029 0.03368466 0.03330120 0.03255624 0.03279187 0.03249364 0.03297875
#[10] 0.03265353 0.03280722 0.03251453 0.03262090 0.03302811 0.03298384
# mse minimale à m=8
foret <-randomForest(I(log(price))~., data = donnees.train, proximity=TRUE, ntree=100,
                     importance=TRUE, mtry=8) 
varImpPlot(foret)
foret_prev <- predict(foret, newdata=donnees.test, type="response")
#foret.res <- roc(log(donnees.test$price), foret_prev, auc=TRUE, plot=TRUE)
#bag.res$auc
(EQM.foret <- sum((foret_prev-log(donnees.test$price))^2)/length(donnees.test))
