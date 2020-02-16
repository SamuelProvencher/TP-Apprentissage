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

ggplot(donnees, aes(x=date, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(bedrooms), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(bathrooms), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=sqft_living, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=sqft_lot, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(floors), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(waterfront), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(view), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(condition), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(grade), y=log(price))) + geom_bar(stat="identity") + theme_bw()

ggplot(donnees, aes(x=sqft_above, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=sqft_basement, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=yr_built, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(yr_renovated), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=lat, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=long, y=log(price))) + geom_point(alpha=0.4) + theme_bw()



