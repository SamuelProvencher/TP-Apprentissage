#Arbre de décisions

source("script2.R")

library(rpart)
library(rpart.plot)

##### Optimisation de l'argument minbucket (ou minsplit??? Un des deux je sais pas)
#### L'optimisation doit se faire manuellement https://stackoverflow.com/questions/36781755/how-to-specify-minbucket-in-caret-train-for?rq=1
#### Je sais pas si c'est bon

# fonction_arbre <- function(x)
# {
#     set.seed(1)
#     
#     tree.control <- rpart.control(cp = 0, minbucket = x) 
#     
#     arbre <- rpart(I(log(price))~., data=donnees.train, control = tree.control, method = "anova")
#     
#     cp.choix <- arbre$cptable[which.min(arbre$cptable[,4]),1]
#     
#     arbre.elague <- prune(arbre, cp = cp.choix)
#     
#     PredArbre <- predict(arbre.elague, type = "vector")
#     
#     EQM.arbre <- sum((PredArbre-log(donnees.train$price))^2)/nrow(donnees.train)
#     
#     data.frame(cp = cp.choix, EQM = EQM.arbre)
#     
# }
# fonction_arbre(x=5)
# 
# minbucket <- c(1,5,10, 15, 20, 25, 30, 40, 50, 100, 200)
# 
# EQM.test <- sapply(minbucket, function(i) fonction_arbre(x=i)$EQM) #minbucket = 10 amène le plus petit EQM
# 
# minbu <- minbucket[which.min(EQM.test)] #10
# 
# minbucket2 <- 1:15
# 
# EQM.test2 <- sapply(minbucket2, function(i) fonction_arbre(x=i)$EQM)
# 
# minbucket.choix <- minbucket2[which.min(EQM.test2)] #9

#### Modèle arbre de régression

set.seed(1)

tree.control <- rpart.control(cp = 0, minbucket = 9)

arbre1 <- rpart(I(log(price))~., data=donnees.train, control = tree.control, method = "anova")

#plotcp(arbre1) #cross validation en 10 plis
#On pourrait aussi utiliser le package caret pour avoir le cp optimal, mais ca semble plus complexe

#View(arbre1$cptable) 

#cp <- arbre1$cptable[min(which(arbre1$cptable[,1]<arbre1$cptable[,5])),1] #le premier qui est sous l'écart type

cp.choix <- arbre1$cptable[which.min(arbre1$cptable[,4]),1] #formule à MP dans exercice arbres poisson

arbre.elague <- prune(arbre1, cp = cp.choix)

PredArbre <- predict(arbre.elague, newdata = donnees.test, type = "vector") #type=vector pour la régression

#rpart.plot(arbre.elague) # ne marche pas malheureusement, MP a dit dans le forum que ca se pouvait, car on a beaucoup de données

#mean(PredArbre);mean(log(donnees.test$price)) #se ressemble

(EQM.arbre <- sum((PredArbre-log(donnees.test$price))^2)/nrow(donnees.test)) #0.0477653




