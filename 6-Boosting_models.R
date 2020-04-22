source("script2.r")

library(caret)
library(gbm)
library(iml)
library(ggplot2)


#### Construction du modèle ####


# {
# set.seed(69)
# temp <- proc.time()
# gbm.modele3 <- gbm(log(price)~.,
#                   distribution = "gaussian",
#                   data = donnees.train,
#                   n.trees = 30000,
#                   interaction.depth = 3,
#                   n.minobsinnode = 10,
#                   shrinkage = 0.01,
#                   bag.fraction = 0.5,
#                   verbose = F,
#                   cv.folds = 5)
# gbm.modele5 <- gbm(log(price)~.,
#                   distribution = "gaussian",
#                   data = donnees.train,
#                   n.trees = 25000,
#                   interaction.depth = 5,
#                   n.minobsinnode = 10,
#                   shrinkage = 0.01,
#                   bag.fraction = 0.5,
#                   verbose = F,
#                   cv.folds = 5)
# gbm.modele7 <- gbm(log(price)~.,
#                   distribution = "gaussian",
#                   data = donnees.train,
#                   n.trees = 25000,
#                   interaction.depth = 7,
#                   n.minobsinnode = 10,
#                   shrinkage = 0.01,
#                   bag.fraction = 0.5,
#                   verbose = F,
#                   cv.folds = 5)
# gbm.modele9 <- gbm(log(price)~.,
#                    distribution = "gaussian",
#                    data = donnees.train,
#                    n.trees = 13000,
#                    interaction.depth = 9,
#                    n.minobsinnode = 10,
#                    shrinkage = 0.01,
#                    bag.fraction = 0.5,
#                    verbose = F,
#                    cv.folds = 5)
# proc.time() - temp
# }

# très long: résultats ici
load("gbm_modeles.Rdata")

n.iter3 <- gbm.perf(gbm.modele3, method = "cv")
n.iter5 <- gbm.perf(gbm.modele5, method = "cv")
n.iter7 <- gbm.perf(gbm.modele7, method = "cv")
legend(x=3000,y=0.2,c("EQM de validation croisée","EQM d'entraînement", "Nombre d'itérations optimal"),
       cex=.8,col=c("green","black", "blue"), lty=c(1,1,2))
n.iter9 <- gbm.perf(gbm.modele9, method = "cv")

data.frame(Profondeur = c(3,5,7,9), EQM_CV = c(gbm.modele3$cv.error[n.iter3],
                                               gbm.modele5$cv.error[n.iter5],
                                               gbm.modele7$cv.error[n.iter7],
                                               gbm.modele9$cv.error[n.iter9]))
# Meilleur modèle: d=9, n.trees = 10515

PredGbm <- predict(gbm.modele9, newdata = donnees.test, n.trees = n.iter9)

EQM.gbm <- mean((PredGbm - log(donnees.test$price))^2)


ggplot(mapping = aes(x = PredGbm)) +
  geom_histogram() +
  theme_bw() +
  xlim(11, 16)
ggplot(mapping = aes(x = log(donnees.test$price))) +
  geom_histogram() +
  theme_bw() +
  xlim(11, 16)


#### Interprétation du modèle ####

mod.gbm.iml <- Predictor$new(gbm.modele9, data = donnees.train[, -1], y = log(donnees.train$price))

imp <- FeatureImp$new(mod.gbm.iml, loss = "mse", compare = "difference")
# 
# pdp.lat <- FeatureEffect$new(mod.gbm.iml, "lat", method = "pdp", grid.size = 40)
# pdp.sqft_above <- FeatureEffect$new(mod.gbm.iml, "sqft_above", method = "pdp", grid.size = 40)
# pdp.grade <- FeatureEffect$new(mod.gbm.iml, "grade", method = "pdp", grid.size = 40)
# pdp.long <- FeatureEffect$new(mod.gbm.iml, "long", method = "pdp", grid.size = 40)
# pdp.sqft_lot <- FeatureEffect$new(mod.gbm.iml, "sqft_lot", method = "pdp", grid.size = 40)

# très long: résultats ici
# load("gbm_iml.Rdata")
# load("gbm_imp.Rdata")
# load("gbm_pdp.Rdata")
# 
# plot(imp)
# plot(pdp.lat)
# plot(pdp.sqft_above)
# plot(pdp.grade)
# plot(pdp.long)
# plot(pdp.sqft_lot)
# 
# int.lat <- Interaction$new(mod.gbm.iml, "lat")
# plot(int.lat)
# 
# pdp.XXX.YYY <- FeatureEffect$new(mod.gbm.iml, feature = c("XXX", "YYY"),
#                                  method = "pdp", grid.size = 40)
# plot(pdp.XXX.YYY)

summary(gbm.modele9, n.trees = n.iter9, cex.names = 0.5, las = 1) # lequel utiliser?

plot(gbm.modele9, i.var = "grade", n.trees = n.iter9)
plot(gbm.modele9, i.var = "lat", n.trees = n.iter9)
plot(gbm.modele9, i.var = "sqft_above", n.trees = n.iter9)
plot(gbm.modele9, i.var = "sqft_living15", n.trees = n.iter9)
plot(gbm.modele9, i.var = "long", n.trees = n.iter9)

# hstat.vec <- sapply(colnames(donnees.train)[c(-1, -9)], function(i)
#   interact.gbm(gbm.modele9, data = donnees.train, i.var = c("grade", i), n.trees = n.iter9))
# hstat.grade <- data.frame(Variables = paste("grade", "+", colnames(donnees.train)[c(-1, -9)]), HStat = hstat.vec)
# rownames(hstat.grade) <- NULL

# assez long: résultats ici
load("gbm_hstat_grade.Rdata")

hstat.grade
ggplot(hstat.grade, aes(x = Variables, y = HStat)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(title = "Interactions")

# pdp.grade.lat <- plot(gbm.modele9, i.var = c("grade", "lat"), n.trees = n.iter9)
# pdp.grade.age <- plot(gbm.modele9, i.var = c("grade", "age"), n.trees = n.iter9)
# pdp.long.lat <- plot(gbm.modele9, i.var = c("long", "lat"), n.trees = n.iter9)

# assez long: résultats ici
load("gbm_pdp_bivaries.Rdata")
plot(pdp.grade.lat)
plot(pdp.grade.age)
plot(pdp.long.lat)
interact.gbm(gbm.modele9, data = donnees.train, i.var = c("lat", "long"), n.trees = n.iter9)
