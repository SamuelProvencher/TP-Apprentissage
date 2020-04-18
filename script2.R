#### Prétraitement des données ####

# Après ce script, les données sont prêtes à être utilisées dans les différents modèles.


donnees <- read.csv("kc_house_data.csv")

donnees <- donnees[!donnees$bathrooms == 0,] #10 cas
donnees <- donnees[!donnees$bedrooms == 0,]

donnees <- donnees[!donnees$bedrooms == 33,]

donnees <- donnees[,-c(which(colnames(donnees)=="sqft_living"), which(colnames(donnees)=="id"),
                       which(colnames(donnees)=="zipcode"))]

donnees$date <- substr(donnees$date,1,8)
annee <- substr(donnees$date,1,nchar(donnees$date)-4)
mois <- substr(donnees$date,nchar(donnees$date)-3,nchar(donnees$date)-2)
jour <- substr(donnees$date, nchar(donnees$date)-1, nchar(donnees$date))

donnees$date <- as.POSIXct(paste(annee,mois,jour,sep="-"), format="%Y-%m-%d", tz="UTC")

donnees$reno <- (donnees$yr_renovated==0)*1

donnees$age <- ifelse(as.numeric(annee) - donnees$yr_built >= 0, as.numeric(annee) - donnees$yr_built, 0) #cap à 115

xmin <-  -122.4
xmax <-  -122
ymin <-  47.5
ymax <-  47.72

donnees$expensive_area <- sapply(1:nrow(donnees),
                                 function(i) as.numeric(donnees$lat[i] >= ymin & donnees$lat[i] <= ymax & donnees$long[i] >= xmin & donnees$long[i] <= xmax))

donnees4 <- donnees[,-c(which(colnames(donnees)=="yr_built"), which(colnames(donnees)=="yr_renovated"), 
                        which(colnames(donnees) == "date"))] #Même chose que donnees pour l'acp corrigé, mais on enlève pas price ici

set.seed(69)
ind.train <- sample(1:nrow(donnees4), 0.8*nrow(donnees4), replace = F)
donnees.train <- donnees4[ind.train,]#Donnees d'entrainement
donnees.test <- donnees4[-ind.train,]#Donnees test
