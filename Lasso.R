source("script.R")
source("ModeleBase.R")

summary(ModBase2)

x.train <- model.matrix(ModBase2, data=donnees.train)[,-1]
x.test <- model.matrix(ModBase2, data=donnees.test)[,-1]
y.train <- donnees.train$price
y.test <- donnees.test$price

grid <- 10^seq(3,-2, length = 100)

modele.lasso <- glmnet(x.train, y.train, family = "gaussian" ,alpha=1,lambda=grid)
plot(modele.lasso,xvar="lambda",label=TRUE)

set.seed(4355)
cv.out <- cv.glmnet(x.train,y.train,alpha=1, nfolds=6)
plot(cv.out)

(meilleur.lam.lasso <- cv.out$lambda.min)

(msep.lasso <- mean((y.test-predict(modele.lasso,s=meilleur.lam.lasso, newx=x.test))^2))

predict(modele.lasso,type="coefficients",s=meilleur.lam.lasso)
