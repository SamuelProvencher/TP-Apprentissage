source("script.R")

control <- trainControl(method = "cv",
                        number = 5)

set.seed(69)

temp <- proc.time()

rf.train <- train(log(price)~.,
                data = donnees.train,
                method = "rf",
                metric = "RMSE",
                tuneGrid = expand.grid(mtry = 2:10),
                trControl = control,
                sampsize = floor(0.75*nrow(donnees.train)),
                ntree = 150,
                nodesize=2)
proc.time()-temp

save(rf.train, file = "save.Rdata")

