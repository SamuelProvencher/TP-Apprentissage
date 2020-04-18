source("script2.r")

library(caret)
library(gbm)

gbm.controls <- trainControl(method = "cv",
                             number = 5,
                             summaryFunction = defaultSummary)

{
temp <- proc.time()
gbm.train <- train(log(price)~.,
                   data = donnees.train,
                   method = "gbm",
                   tuneGrid = expand.grid(interaction.depth = c(7), 
                                          n.trees = seq(0, 5000, 20), 
                                          shrinkage = 0.01, 
                                          n.minobsinnode = 5),
                   trControl = gbm.controls)
proc.time() - temp
} # à peu près 10 minutes de temps de calcul

plot(gbm.train)

PredGbm <- predict(gbm.train, newdata = donnees.test, n.trees = 5000)
EQM.gbm <- mean((PredGbm - log(donnees.test$price))^2)
