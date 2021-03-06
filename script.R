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

#### ANALYSE EXPLORATOIRE DES DONNEES - Univariée ####
library(ggplot2)
ggplot(donnees, aes(x=date)) + geom_density() + theme_bw() #saisonalité dans la vente de maison

ggplot(donnees, aes(x=price)) + geom_density() + theme_bw() #asymétrie

ggplot(donnees, aes(x=log(price))) + geom_density() + theme_bw() #log pour variable réponse est mieux

ggplot(donnees, aes(x=factor(bedrooms))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=factor(bathrooms))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=factor(floor(bathrooms)))) + geom_bar() + theme_bw() #floor = arrondi à l'entier inférieur

ggplot(donnees, aes(x=sqft_living)) + geom_area(stat = "bin") + theme_bw()

ggplot(donnees[which(donnees$sqft_lot<300000),], aes(x=sqft_lot)) + geom_area(stat = "bin") + theme_bw()

ggplot(donnees, aes(x=factor(floors))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=factor(waterfront))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=factor(view))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=factor(condition))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=factor(grade))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=sqft_above)) + geom_area(stat = "bin") + theme_bw()

ggplot(donnees, aes(x=sqft_basement)) + geom_area(stat = "bin") + theme_bw() #pic de données à 0

ggplot(donnees, aes(x=yr_built)) + geom_density() + theme_bw() #Plus de données de maisons récentes

ggplot(donnees[which(donnees$yr_renovated!=0),], aes(x=yr_renovated)) + geom_density() + theme_bw() #Plus de maisons rénovées récemment

ggplot(donnees, aes(x=sqft_living15)) + geom_area(stat = "bin") + theme_bw()

ggplot(donnees, aes(x=sqft_lot15)) + geom_area(stat = "bin") + theme_bw()

ggplot(donnees, aes(x=factor(reno))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=age)) + geom_area(stat = "bin") + theme_bw() # On voit bien le maximum à 115 ans (Maison construite en 1900)

ggplot(donnees, aes(x=factor(expensive_area))) + geom_bar() + theme_bw()


#### ANALYSE EXPLORATOIRE DES DONNEES - Bivariée ####

ggplot(donnees, aes(x=date, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(bedrooms), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(floor(bathrooms)), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=sqft_living, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=log(sqft_living), y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=sqft_lot, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=log(sqft_lot), y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(floors), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(waterfront), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(view), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(condition), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(grade), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=sqft_above, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=log(sqft_above), y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees[!donnees$sqft_basement ==0,], aes(x=sqft_basement, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees[!donnees$sqft_basement ==0,], aes(x=log(sqft_basement), y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=age, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(reno), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=lat, y=log(price))) + geom_point(alpha=0.4) + theme_bw() #Utiliser la heatmap

ggplot(donnees, aes(x=long, y=log(price))) + geom_point(alpha=0.4) + theme_bw() #Utiliser la heatmap

#### Nettoyage des colonnes inutiles ####

#suppression données non utiles pour l'acp
donnees2 <- donnees[,-c(which(colnames(donnees)=="yr_built"), which(colnames(donnees)=="yr_renovated"), 
                       which(colnames(donnees)=="lat"), which(colnames(donnees)=="long"), which(colnames(donnees)=="date"))]
ncol(donnees2)
donnees2_test <- donnees2

#### ACP ####
library(FactoMineR)

#test log du prix dans ACP
donnees2_test$price <- log(donnees2$price)
acp_test <- PCA(donnees2_test,graph = F)
acp_test$eig

#vrai ACP
acp <- PCA(donnees2,graph = F)
acp$eig # valeurs propres, var expliquée et % var expliquée

#diagramme d'éboulis
library(factoextra)
fviz_screeplot(acp, ncp=20)

#Corrélation variables
cormat <- cor(donnees2,method = "pearson")
library(reshape2)
cormat.long <- melt(cormat)
ggplot(data = cormat.long, aes(Var2, Var1, fill = value))+geom_tile(aes(fill=value),color="grey3")+
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white')

#visualisation coordonnées Dim 1 et 2
donnees_acp <- cbind(donnees2, acp$ind$coord)
plot_ly <- ggplot() +
    geom_point(data = donnees_acp,
               aes(Dim.1, Dim.2, col = price)) +
    xlab("Dimension 1") +
    ylab("Dimension 2")  +
    theme_minimal() +
    scale_color_gradient(low="green", high="red", trans = "log")
library(plotly)
ggplotly(plot_ly)
#visualisation coordonnées Dim 3 et 4
ggplot() +
    geom_point(data = donnees_acp,
               aes(Dim.3, Dim.4, col = price)) +
    xlab("Dimension 3") +
    ylab("Dimension 4")  +
    theme_minimal() +
    scale_color_gradient(low="green", high="red", trans = "log")

## Autre façon de visualiser comportant la contribution pour Dims que 1 et 2
fviz_pca_var(acp,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)

# Visualisation contributions des 4 composantes
contrib <- data.frame(acp$var$coord[,1:4])
contrib$carac <- rownames(contrib)
contrib.long <- reshape2::melt(contrib)

ggplot(contrib.long, aes(x=carac, fill=variable, y=value))+
    geom_bar(stat="identity",position=PositionDodge)+
    facet_grid(~variable)+
    theme(legend.position="top",axis.text.x = element_text(angle = 90))+
    coord_flip()

# map des coordonnées de la 4e dimension
donnees_view <- cbind(donnees, acp$ind$coord)
library(leaflet)
pal4 <- colorNumeric(
    palette = "RdYlBu",
    domain = donnees_view$Dim.4,
    reverse = TRUE)

leaflet(donnees_view) %>% 
    addTiles() %>%
    addCircleMarkers(lng = ~long, 
                     lat = ~lat, 
                     color = ~pal4(Dim.4),
                     radius = 0.75,
                     opacity = 1, 
                     fill = TRUE, 
                     fillColor = ~pal4(Dim.4), 
                     fillOpacity = 1,
                     group = 'Composante 4'
    ) %>%
    addLegend("bottomleft", pal = pal4, values = ~Dim.4,
              title = "CP4",
              opacity = 1,
              group = 'Composante 4'
    )

####ACP corrigé####
donnees3 <- donnees[,-c(which(colnames(donnees)=="yr_built"), which(colnames(donnees)=="yr_renovated"), 
                        which(colnames(donnees)=="price"), which(colnames(donnees) == "date"))]
acp_corrige <- PCA(donnees3, graph = F)

#diagramme d'éboulis
library(factoextra)
fviz_screeplot(acp_corrige, ncp=20)

#Corrélation variables
cormat_corrige <- cor(donnees3,method = "pearson")
library(reshape2)
cormat.long_corrige <- melt(cormat_corrige)
ggplot(data = cormat.long_corrige, aes(Var2, Var1, fill = value))+geom_tile(aes(fill=value),color="grey3")+
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white')

#visualisation coordonnées Dim 1 et 2
donnees_acp_corrige <- cbind(donnees3, acp_corrige$ind$coord)
plotly_corrige <- ggplot() +
    geom_point(data = donnees_acp_corrige,
               aes(Dim.1, Dim.2)) +
    xlab("Dimension 1") +
    ylab("Dimension 2")  +
    theme_minimal() +
    scale_color_gradient(low="green", high="red", trans = "log")
library(plotly)
ggplotly(plotly_corrige)

#visualisation coordonnées Dim 3 et 4
ggplot() +
    geom_point(data = donnees_acp_corrige,
               aes(Dim.3, Dim.4)) +
    xlab("Dimension 3") +
    ylab("Dimension 4")  +
    theme_minimal() +
    scale_color_gradient(low="green", high="red", trans = "log")

## Autre façon de visualiser comportant la contribution pour Dims que 1 et 2
fviz_pca_var(acp_corrige,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)

# Visualisation contributions des 4 composantes
contrib_corrige <- data.frame(acp_corrige$var$coord[,1:4])
contrib_corrige$carac <- rownames(contrib_corrige)
contrib.long_corrige <- reshape2::melt(contrib_corrige)

ggplot(contrib.long_corrige, aes(x=carac, fill=variable, y=value))+
    geom_bar(stat="identity",position=PositionDodge)+
    facet_grid(~variable)+
    theme(legend.position="top",axis.text.x = element_text(angle = 90))+
    coord_flip()

# map des coordonnées de la 4e dimension
donnees_view_corrige <- cbind(donnees, acp_corrige$ind$coord)
library(leaflet)
pal4 <- colorNumeric(
    palette = "RdYlBu",
    domain = donnees_view_corrige$Dim.4,
    reverse = TRUE)

leaflet(donnees_view_corrige) %>% 
    addTiles() %>%
    addCircleMarkers(lng = ~long, 
                     lat = ~lat, 
                     color = ~pal4(Dim.4),
                     radius = 0.75,
                     opacity = 1, 
                     fill = TRUE, 
                     fillColor = ~pal4(Dim.4), 
                     fillOpacity = 1,
                     group = 'Composante 4'
    ) %>%
    addLegend("bottomleft", pal = pal4, values = ~Dim.4,
              title = "CP4",
              opacity = 1,
              group = 'Composante 4'
    )

####Donnees entrainement et test ####

donnees4 <- donnees[,-c(which(colnames(donnees)=="yr_built"), which(colnames(donnees)=="yr_renovated"), 
                        which(colnames(donnees) == "date"))] #Même chose que donnees pour l'acp corrigé, mais on enlève pas price ici

set.seed(69)
ind.train <- sample(1:nrow(donnees4), 0.8*nrow(donnees4), replace = F)
donnees.train <- donnees4[ind.train,]#Donnees d'entrainement
donnees.test <- donnees4[-ind.train,]#Donnees test
