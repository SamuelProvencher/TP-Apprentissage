source("script2.R")

library(caret)
library(FNN)


#Optimisation de K

set.seed(69)
# controles <- caret::trainControl(method  = "cv", # validation croisée
#                           number  = 10) # 10 plis
# fit <- caret::train(log(price) ~ .,
#              method     = "knn",
#              preProcess = "scale",
#              tuneGrid   = expand.grid(k = 2:20),
#              trControl  = controles,
#              metric     = "RMSE",
#              data       = donnees.train)
# fit #k=9

data.train <- cbind(donnees.train[,1],sapply(donnees.train[,-1], scale))
data.test <- cbind(donnees.test[,1],sapply(donnees.test[,-1], scale))

train <- data.train[,-1]#Donnees d'entrainement
test <- data.test[,-1]#Donnees test

etiq_train <- log(data.train[,1])
etiq_test <- log(data.test[,1])

k <- 9

predictions <- knn.reg(train, test, y=etiq_train, k)


v <- predictions$pred

EQM.Kvoisins <- mean((predictions$pred-etiq_test)^2)
