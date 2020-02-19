############ TRAVAIL PRATIQUE

#Importation####

donnees <- read.csv("kc_house_data.csv")
donnees.init <- read.csv("kc_house_data.csv")

#Prétraitement####

str(donnees)
summary(donnees)

donnees <- donnees[!donnees$bathrooms == 0,]

donnees$bedrooms[which(donnees$bedrooms ==33)] <- as.integer(3)

donnees <- donnees[!donnees$bedrooms == 0,]

donnees$reno <- ifelse(donnees$yr_renovated != 0, as.integer(1), as.integer(0))

donnees <- donnees[,-c(1,17)] #enlève ID et zipcode

donnees$date <- substr(donnees$date,1,8)

annee <- substr(donnees$date,1,nchar(donnees$date)-4)
mois <- substr(donnees$date,nchar(donnees$date)-3,nchar(donnees$date)-2)
jour <- substr(donnees$date, nchar(donnees$date)-1, nchar(donnees$date))

donnees$date <- as.POSIXct(paste(annee,mois,jour,sep="-"), format="%Y-%m-%d", tz="UTC")

sam <- donnees[which(donnees$yr_built ==1900),] #sam vérifie

donnees$age <- ifelse(as.numeric(annee) - donnees$yr_built >= 0, as.numeric(annee) - donnees$yr_built, 0)

#lat et long, on prendra la heat map à matis

library(ggmap)
register_google(key = "AIzaSyDR2ob6a6HSgsBhZkN -k0QNVeJT3uio4Wg") 

map<-get_map(location = c(left = min(donnees$long), bottom = min(donnees$lat), right = max(donnees$long), top = max(donnees$lat)))
ggmap(map, extent = "device")

#heatmapdata <- data.frame(cbind(log(donnees$price), donnees$lat, donnees$long ))
#colnames(heatmapdata) <- c("logprice", "lat", "long")

ggmap(map, extent = "device") + stat_summary_2d(data = donnees ,
                                                aes(x = long, y = lat, z = log(price)),
                                                fun = mean, alpha = 0.6, bins = 30) + 
    scale_fill_gradient(name = "Log(Price)", low = "green", high = "red") +
    #annotate("segment", x=min(donnees$long), xend=max(donnees$long), y=47.52, yend = 47.52, colour="black", lty = 2, lwd = 1.3) +
    annotate("rect", xmin = -122.3, xmax = -122.15, ymin = 47.52, ymax = 47.7, colour="black", lty = 1, lwd = 1.3, alpha = 0) +
    annotate("text", x= -122, y = 47.6, label = "Centre-ville")

## manque le expensive data...
str(donnees)
summary(donnees)

#ANALYSE EXPLORATOIRE DES DONNEES####

library(ggplot2)
#Univariée
ggplot(donnees, aes(x=date)) + geom_density() + theme_bw()

ggplot(donnees, aes(x=price)) + geom_density() + theme_bw()

ggplot(donnees, aes(x=log(price))) + geom_density() + theme_bw()

ggplot(donnees, aes(x=factor(bedrooms))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=factor(bathrooms))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=sqft_living)) + geom_area(stat = "bin") + theme_bw()

ggplot(donnees[which(donnees$sqft_lot<300000),], aes(x=sqft_lot)) + geom_area(stat = "bin") + theme_bw()

ggplot(donnees, aes(x=factor(floors))) + geom_bar() + theme_bw()

ggplot(donnees, aes(x=factor(waterfront))) + geom_bar() + theme_bw()
    

#Bivariée

ggplot(donnees, aes(x=date, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(bedrooms), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(round(bathrooms)), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=sqft_living, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=sqft_lot, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(floors), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(waterfront), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(view), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(condition), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=factor(grade), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=sqft_above, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees[!donnees$sqft_basement ==0,], aes(x=sqft_basement, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=age, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=factor(reno), y=log(price))) + geom_boxplot() + theme_bw()

ggplot(donnees, aes(x=lat, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

ggplot(donnees, aes(x=long, y=log(price))) + geom_point(alpha=0.4) + theme_bw()

# ACP
library(FactoMineR)
acp <- PCA(donnees[,c(-1, -14, -15, -16, -17)])
acp$eig

library(factoextra)
fviz_screeplot(acp, ncp=20)

donnees <- cbind(donnees, acp$ind$coord)
ggplot() +
    geom_point(data = donnees,
               aes(Dim.1, Dim.2, col = price)) +
    xlab("Dimension 1") +
    ylab("Dimension 2")  +
    theme_minimal() +
    scale_color_gradient(low="green", high="red", trans = "log")

fviz_pca_var(acp,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

