############ TRAVAIL PRATIQUE

#### Importation####

donnees <- read.csv("kc_house_data.csv")
donnees.init <- read.csv("kc_house_data.csv")

#### Prétraitement ####

str(donnees)
summary(donnees)

donnees <- donnees[!donnees$bathrooms == 0,] #10 cas
donnees <- donnees[!donnees$bedrooms == 0,] #13 cas
#6 cas ont bedrooms = 0 et bathrooms = 0
#Si on considère que c'est des maisons/jumelé, on trouve ça peu logique de conserver les 6 cas sans chambres (s'il y avait appart, ça pourrait faire du sens)

#suppression 33 chambres
donnees <- donnees[!donnees$bedrooms == 33,]

#suppression sqft_living (redondance), id et zipcode
donnees <- donnees[,-c(which(colnames(donnees)=="sqft_living"), which(colnames(donnees)=="id"),
                       which(colnames(donnees)=="zipcode"))]

#separation champs de la date
donnees$date <- substr(donnees$date,1,8)
annee <- substr(donnees$date,1,nchar(donnees$date)-4)
mois <- substr(donnees$date,nchar(donnees$date)-3,nchar(donnees$date)-2)
jour <- substr(donnees$date, nchar(donnees$date)-1, nchar(donnees$date))

#changement format date
donnees$date <- as.POSIXct(paste(annee,mois,jour,sep="-"), format="%Y-%m-%d", tz="UTC")

#maisons à 1900
maison_1900 <- donnees[which(donnees$yr_built ==1900),] #Hypothèse MP : NA #Par contre ça ne semble pas être des NAs, mais bien que l'âge des maisons a été cappé à 115
nrow(maison_1900)
# 87 cas
nrow(donnees[which(donnees$yr_built ==1901),])
# vs 29 en 1901, donc théoriquement 87-29=58 maisons construites avant 1900. Pas énorme sur 21000 données. On devrait les garder.
nrow(donnees[which(donnees$yr_built ==1902),])
nrow(donnees[which(donnees$yr_built ==1903),])
nrow(donnees[which(donnees$yr_built ==1904),])
nrow(donnees[which(donnees$yr_built ==1905),])
# Autre hypothèse : 87 maisons réellement construites en 1900 (pas de données avant)

# facteur 0-1 pour la reno
donnees$reno <- (donnees$yr_renovated==0)*1

#creation de la variable age
donnees$age <- ifelse(as.numeric(annee) - donnees$yr_built >= 0, as.numeric(annee) - donnees$yr_built, 0) #cap à 115

#lat et long, on prendra la heat map
library(ggmap)
register_google(key = "AIzaSyDR2ob6a6HSgsBhZkN -k0QNVeJT3uio4Wg") 

map<-get_map(location = c(left = min(donnees$long), bottom = min(donnees$lat), right = max(donnees$long), top = max(donnees$lat)))
ggmap(map, extent = "device")

xmin <-  -122.4
xmax <-  -122
ymin <-  47.5
ymax <-  47.72

ggmap(map, extent = "device") + stat_summary_2d(data = donnees ,
                                                aes(x = long, y = lat, z = log(price)),
                                                fun = mean, alpha = 0.6, bins = 100) + 
    scale_fill_gradient(name = "Log(Price)", low = "green", high = "red") +
    annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, colour="black", lty = 1, lwd = 1.3, alpha = 0) +
    annotate("text", x= -122.2, y = 47.6, label = "Zone urbaine")

#Removed 2 rows containing missing values (geom_tile). ???

donnees$expensive_area <- sapply(1:nrow(donnees),
                                 function(i) as.numeric(donnees$lat[i] >= ymin & donnees$lat[i] <= ymax & donnees$long[i] >= xmin & donnees$long[i] <= xmax))
#vérif finale
str(donnees)
summary(donnees)

####Donnees entrainement et test ####

donnees4 <- donnees[,-c(which(colnames(donnees)=="yr_built"), which(colnames(donnees)=="yr_renovated"), 
                        which(colnames(donnees) == "date"))] #Même chose que donnees pour l'acp corrigé, mais on enlève pas price ici

ind.train <- sample(1:nrow(donnees4), 0.8*nrow(donnees4), replace = F)
donnees.train <- donnees4[ind.train,]#Donnees d'entrainement
donnees.test <- donnees4[-ind.train,]#Donnees test