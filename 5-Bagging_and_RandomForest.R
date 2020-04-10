source("script2.R")

library(randomForest)
library(pROC)
library(caret)

#### Bagging ####
set.seed(69)
(bag <-randomForest(I(log(price))~., data = donnees.train, 
                    mtry=17, # on prend toutes les colonnes à chaque noeud
                    sampsize= nrow(donnees.train), # on prend un échantillon de taille n pour bâtir chaque arbre
                    ntree=150,
                    importance=TRUE)) 

plot(1:length(bag$mse),bag$mse, type="l", xlab="B", ylab="Erreur quadratique moyenne", main="Bagging")
varImpPlot(bag)
bag_prev <- predict(bag, newdata=donnees.test, type="response")
(EQM.bag <- mean((bag_prev-log(donnees.test$price))^2))

#### Foret aleatoire ####
control <- trainControl(method = "cv", number = 5)

rf.train <- train(log(price)~.,
                  data = donnees.train,
                  method = "rf",
                  metric = "RMSE",
                  tuneGrid = expand.grid(mtry = 2:15),
                  trControl = control,
                  sampsize = floor(0.75*nrow(donnees.train)),
                  ntree = 150,
                  nodesize=2)

foret <-randomForest(I(log(price))~., data = donnees.train, 
                     sampsize= floor(0.6*nrow(donnees.train)),
                     ntree=100,
                     importance=TRUE, 
                     mtry=9) 
varImpPlot(foret)
foret_prev <- predict(foret, newdata=donnees.test, type="response")
(EQM.foret <- mean((foret_prev-log(donnees.test$price))^2))

