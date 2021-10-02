cars <- read.table("https://raw.githubusercontent.com/Easill/Rshiny_cars/main/cars.csv",  sep = ";", dec = ".", header=TRUE, stringsAsFactors = TRUE)

names(cars) <- c("Marque","Model","Classe","Moteur","Cylindre","Transmission","Fuel_Type","City_L_au_100","Hwy_L_au_100",
                 "Comb_L_au_100","Comb_mpg","CO2")

newdata <- cars[,c(12,4:5,8:11)] #On met cars en premier dans le dataset

mod0 <- glm(CO2 ~ 1,data = newdata) # Mod?le null

select = summary(regsubsets(CO2~.,data=newdata,nvmax=6))

bic = select$bic                                       # BIC
aic = bic - (log(nrow(newdata))-2)*(c(4:5,8:11))       # AIC

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