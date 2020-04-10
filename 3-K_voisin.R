source("script.R")

library(caret)
library(FNN)



#Optimisation de K

controles <- trainControl(method  = "cv", # validation croisée
                          number  = 10) # 5 plis
fit <- train(log(price) ~ .,
             method     = "knn",
             preProcess = "scale",
             tuneGrid   = expand.grid(k = 2:20),
             trControl  = controles,
             metric     = "rmse",
             data       = donnees4)
fit #k=9

data <- cbind(donnees4[,1],sapply(donnees4[,-1], scale))
ind.train <- sample(1:nrow(data), 0.8*nrow(data), replace = F)
train <- data[ind.train,-1]#Donnees d'entrainement
test <- data[-ind.train,-1]#Donnees test

etiq_train <- log(data[ind.train,1])
etiq_test <- log(data[-ind.train,1])

k <- 9

predictions <- knn.reg(train, test, y=etiq_train, k)

ttest <- unique(predictions$pred)

v <- predictions$pred

