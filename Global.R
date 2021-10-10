# Chargement du jeu de donnees
cars <- read.table("https://raw.githubusercontent.com/Easill/Rshiny_cars/main/cars.csv",  sep = ";", dec = ".", header=TRUE, stringsAsFactors = TRUE)

# Renomme les colonnes
names(cars) <- c("Marque","Model","Classe","Moteur","Cylindre","Transmission","Fuel_Type","City_L_au_100","Hwy_L_au_100",
                 "Comb_L_au_100","Comb_mpg","CO2")

# Chargement des packages
library(shiny)
library(shinyWidgets)
require(nnet)            # Multinomial logistic regression
require(leaps)           # For regsubsets
require(pls)             # For segments
require(groupdata2)      # For fold
require(boot)            # For cv.glm
require(FactoMineR)      # For PCA,MCA,...
require(lme4)            # For lmer,glmer
library(shiny)
require(shinydashboard)
require(ggplot2)  # plots
require(forcats)  # ordres boxplots
require(plotly)



# retrait de la ligne de carburant N + retrait du facteur
cars <- subset(cars, Fuel_Type != "N")
cars$Fuel_Type<-factor(cars$Fuel_Type,exclude=NULL)


newdata <- cars[,c(12,4:5,8:11)] #On met cars en premier dans le dataset

mod0 <- glm(CO2 ~ 1,data = newdata) # Mod?le null

select = summary(regsubsets(CO2~.,data=newdata,nvmax=6))

rss = select$rss
bic = select$bic                                   # BIC
aic = bic - (log(nrow(newdata))-2)*(c(2:7))            # AIC

get_scat <- function(sel_var=1){
  selected <- select$which[sel_var,]
  mod <- glm(CO2~.,data=newdata[,selected])
  fitted.co2_2 = fitted(mod) # Fitted LMP values
  observed.co2_2 = newdata$CO2   # Observed LMP values
  R2 <- cor(observed.co2_2,fitted.co2_2)^2
  plot(observed.co2_2,fitted.co2_2,type="p",pch=16,bty="n",xlab="Observed CO2",
       ylab="Fitted CO2",main="Fitted versus observed CO2 values",
       cex.lab=1.25,cex.axis=1.25,cex.main=1.25)
  abline(0,1,lwd = 3, col = 'red')
  text(175,450,paste("R2=",round(R2,3)),cex=1.25)
}



# Boxplots
# noms selection des variables
var_quali<-c("marque" = names(cars[1]),
             "modèle" = names(cars[2]),
             "classe" = names(cars[3]),
             "transmission" = names(cars[6]),
             "type d'essence" = names(cars[7]))

# Scatterplot



# noms selection des variables 
var_quanti<-c("taille du moteur (en L)" = names(cars[4]),
              "nombre de cylindres dans le moteur" = names(cars[5]),
              "consommation de carburant en ville (L/100 km)" = names(cars[8]),
              "consommation de carburant sur autoroute (L/100 km)" = names(cars[9]),
              "consommation de carburant combinée (55 % en ville, 45 % sur route) (L/100 km)" = names(cars[10]),
              "consommation de carburant combinée (55 % en ville, 45 % sur route) (miles per gallon)" = names(cars[11]))


# Matrice correlation

# Matrice de correlation des variables quantit
cormat<-cor(newdata) # matrice de correlation

meltcormat <- reshape2::melt(cormat)
colnames(meltcormat) <- c("x", "y", "value")


js <- c('.nav-tabs-custom .nav-tabs li.active {border-top-color: #00c0ef;}')