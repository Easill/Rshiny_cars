# Chargement des packages
library(shiny)
require(nnet)            # Multinomial logistic regression
require(leaps)           # For regsubsets
require(pls)             # For segments
require(groupdata2)      # For fold
require(boot)            # For cv.glm
require(FactoMineR)      # For PCA,MCA,...
require(lme4)            # For lmer,glmer
library(shiny)
require(shinydashboard)
require(plotly)
require(ggplot2)

# Chargement du jeu de donnees
cars <- read.table("https://raw.githubusercontent.com/Easill/Rshiny_cars/main/cars.csv",  sep = ";", dec = ".", header=TRUE, stringsAsFactors = TRUE)

# Renomme les colonnes
names(cars) <- c("Marque","Model","Classe","Moteur","Cylindre","Transmission","Fuel_Type","City_L_au_100","Hwy_L_au_100",
                 "Comb_L_au_100","Comb_mpg","CO2")

# retrait de la ligne de carburant N + retrait du facteur
cars <- subset(cars, Fuel_Type != "N")
cars$Fuel_Type<-factor(cars$Fuel_Type,exclude=NULL)

# Boxplots

















