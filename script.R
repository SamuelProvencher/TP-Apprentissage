############ TRAVAIL PRATIQUE

donnees <- read.csv("kc_house_data.csv")

str(donnees)
summary(donnees)

# ANALYSE EXPLORATOIRE DES DONNEES

library(ggplot2)

ggplot(donnees, aes(x=price, y=bedrooms)) + geom_boxplot()
