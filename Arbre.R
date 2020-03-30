#Arbre de décisions

source("script.R")

library(rpart)
library(rpart.plot)

tree.control <- rpart.control(cp = 0, minbucket = 5)

arbre1 <- rpart(I(log(price))~., data=donnees.train, control = tree.control)

plotcp(arbre1) #cross validation en 10 plis
#On pourrait aussi utiliser le package caret pour avoir le cp optimal, mais ca semble plus complexe

#View(arbre1$cptable) #18e

cp <- arbre1$cptable[min(which(arbre1$cptable[,1]<arbre1$cptable[,5])),1] #le premier qui est sous l'écart type

arbre.elague <- prune(arbre1, cp = cp)

PredArbre <- predict(arbre.elague, newdata = donnees.test, type = "vector") #type=vector pour la régression

rpart.plot(arbre.elague)

mean(PredArbre);mean(log(donnees.test$price)) #se ressemble
