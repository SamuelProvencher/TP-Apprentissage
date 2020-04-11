#Arbre de décisions

source("script.R")

library(rpart)
library(rpart.plot)

tree.control <- rpart.control(cp = 0, minbucket = 5)

arbre1 <- rpart(I(log(price))~., data=donnees.train, control = tree.control)

plotcp(arbre1) #cross validation en 10 plis
#On pourrait aussi utiliser le package caret pour avoir le cp optimal, mais ca semble plus complexe

#View(arbre1$cptable) 

#cp <- arbre1$cptable[min(which(arbre1$cptable[,1]<arbre1$cptable[,5])),1] #le premier qui est sous l'écart type

cp.choix <- arbre1$cptable[which.min(arbre1$cptable[,4]),1] #formule à MP dans exercice arbres poisson

arbre.elague <- prune(arbre1, cp = cp.choix)

PredArbre <- predict(arbre.elague, newdata = donnees.test, type = "vector") #type=vector pour la régression

rpart.plot(arbre.elague)

mean(PredArbre);mean(log(donnees.test$price)) #se ressemble

EQM.arbre <- sum((PredArbre-log(donnees.test$price))^2)/nrow(donnees.test)
