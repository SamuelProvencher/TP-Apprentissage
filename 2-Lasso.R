source("script.R")
source("1-ModeleBase.R")

library(glmnet)
x.train <- model.matrix(ModBase2, data=donnees.train)[,-1]
x.test <- model.matrix(ModBase2, data=donnees.test)[,-1]
y.train <- log(donnees.train$price)
y.test <- log(donnees.test$price)

grid <- 10^seq(3,-2, length = 100)

modele.lasso <- glmnet(x.train, y.train,alpha=1,lambda=grid)
plot(modele.lasso,xvar="lambda",label=TRUE)

set.seed(4355)
cv.out <- cv.glmnet(x.train,y.train,alpha=1, nfolds=6)
plot(cv.out)

(meilleur.lam.lasso <- cv.out$lambda.min)

(msep.lasso <- mean((y.test-predict(modele.lasso,s=meilleur.lam.lasso, newx=x.test))^2))

predict(modele.lasso,type="coefficients",s=meilleur.lam.lasso)

ModLasso <- glm(I(log(price)) ~ sqft_lot + waterfront + sqft_above +sqft_basement+
                    lat + reno + bathrooms:lat + waterfront:lat + 
                    sqft_above:lat + sqft_basement:lat, data = donnees.train)
#Pas parfait encore, c'est juste un premier test
PredLasso <-  predict(ModLasso, newdata = donnees.test, type = "response")

pred <- cbind(log(donnees.test$price), PredLasso)


