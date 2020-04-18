source("script2.R")

#### ModBase ####

ModBase <- lm(I(log(price))~., data = donnees.train)
summary(ModBase)
PredBase <- predict(ModBase, newdata = donnees.test, type = "response")

ModBase2 <- lm(I(log(price))~(.)^2, data = donnees.train)
summary(ModBase2)
PredBase2 <- predict(ModBase2, newdata = donnees.test, type = "response")

EQM.ModBase <- sum((PredBase-log(donnees.test$price))^2)/nrow(donnees.test)
EQM.ModBase2 <- sum((PredBase2-log(donnees.test$price))^2)/nrow(donnees.test)
