source("script.R")

library(randomForest)
library(pROC)

#### Bagging ####
set.seed(69)
bag <-randomForest(price~., data = donnees.train, proximity=TRUE, ntree=50,importance=TRUE) 
bag
bag$mse[]
plot(1:length(bag$mse),bag$mse, type="l", xlab="B", ylab="Erreur quadratique moyenne", main="Bagging")
varImpPlot(bag)
bag_prev <- predict(bag, newdata=donnees.test, type="response")
bag.res <- roc(donnees.test$price, bag_prev, auc=TRUE, plot=TRUE)
bag.res$auc

#### Foret aleatoire ####
oob.values <- vector(length=10)
for(i in 4:15) {
    temp.model <- randomForest(price ~ ., data=donnees.train, mtry=i, proximity=TRUE, 
                               ntree=50,importance=TRUE)
    oob.values[i] <- temp.model$mse[length(temp.model$mse)]
}
oob.values
#38145767195 21680065279 19448833900 18971641912 18425064898 18093250790 17540099545 17391514525
# 18313559044 17062334435