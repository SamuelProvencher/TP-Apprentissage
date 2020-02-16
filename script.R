############ TRAVAIL PRATIQUE

#Importation####

donnees <- read.csv("kc_house_data.csv")
donnees.init <- read.csv("kc_house_data.csv")

#Prétraitement####

str(donnees)
summary(donnees)

donnees <- donnees[!donnees$bathrooms == 0,]

donnees$bedrooms[which(donnees$bedrooms ==33)] <- 3

donnees$yr_renovated[which(donnees$yr_renovated != 0)] <- as.integer(1)
donnees$yr_renovated[which(donnees$yr_renovated == 0)] <- as.integer(0)

donnees <- donnees[,-1] #enlève ID

donnees$date <- substr(donnees$date,1,8)

annee <- substr(donnees$date,1,nchar(donnees$date)-4)
mois <- substr(donnees$date,nchar(donnees$date)-3,nchar(donnees$date)-2)
jour <- substr(donnees$date, nchar(donnees$date)-1, nchar(donnees$date))

donnees$date <- as.POSIXct(paste(annee,mois,jour,sep="-"), format="%Y-%m-%d", tz="UTC")

#ANALYSE EXPLORATOIRE DES DONNEES####

library(ggplot2)

ggplot(donnees, aes(x=price, y=bedrooms)) + geom_boxplot()

