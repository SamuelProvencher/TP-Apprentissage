source("script.R")

ModBase <- lm(log(price)~., data = donnees.train)
summary(ModBase)
PredBase <- predict(ModBase, newdata = donnees.test, type = "response")

ModBase2 <- lm(log(price)~(.)^2, data = donnees.train)
summary(ModBase2)
PredBase2 <- predict(ModBase2, newdata = donnees.test, type = "response")