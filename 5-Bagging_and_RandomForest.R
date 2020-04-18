## Toute la mémoire est dans la ligne de code suivante
load("save.Rdata")

source("script2.R")

library(randomForest)
library(pROC)
library(caret)

#### Bagging ####
set.seed(69)
bag <-randomForest(I(log(price))~., data = donnees.train, 
                    mtry=17, # on prend toutes les colonnes à chaque noeud
                    sampsize= nrow(donnees.train), # on prend un échantillon de taille n pour bâtir chaque arbre
                    ntree=500,
                    importance=TRUE) 

plot(1:length(bag$mse),bag$mse, type="l", xlab="ntree", ylab="Erreur quadratique moyenne", main="Bagging")
importance_var_bag <- varImpPlot(bag)
bag_prev <- predict(bag, newdata=donnees.test, type="response")
EQM.bag <- mean((bag_prev-log(donnees.test$price))^2)

#### Foret aleatoire ####
control <- trainControl(method = "cv", number = 5)
#très long, ne pas rouler
# rf.train <- train(log(price)~.,
#                    data = donnees.train,
#                    method = "rf",
#                    metric = "RMSE",
#                    tuneGrid = expand.grid(mtry = 6:12),
#                    trControl = control,
#                    sampsize = floor(0.75*nrow(donnees.train)),
#                    ntree = 150)

#interprétation
 foret.iml <- Predictor$new(rf.train)
 pdp.lat <- FeatureEffect$new(foret.iml, "lat", method = "pdp",
                                grid.size = 50)
 pdp.long <- FeatureEffect$new(foret.iml, "long", method = "pdp",
                              grid.size = 50)
 pdp.grade <- FeatureEffect$new(foret.iml, "grade", method = "pdp",
                              grid.size = 50)

#vrai modèle
 foret <-randomForest(I(log(price))~., data = donnees.train,
                      sampsize= floor(0.75*nrow(donnees.train)),
                      ntree=150,
                      importance=TRUE,
                      mtry=8)
 importance_var_foret <- varImpPlot(foret)
 foret_prev <- predict(foret, newdata=donnees.test, type="response")
 EQM.foret <- mean((foret_prev-log(donnees.test$price))^2)
