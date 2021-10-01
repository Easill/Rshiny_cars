cars <- read.table("https://raw.githubusercontent.com/Easill/Rshiny_cars/main/cars.csv",  sep = ";", dec = ".", header=TRUE, stringsAsFactors = TRUE)

names(cars) <- c("Marque","Model","Classe","Moteur","Cylindre","Transmission","Fuel_Type","City_L_au_100","Hwy_L_au_100",
                 "Comb_L_au_100","Comb_mpg","CO2")


require(nnet)            # Multinomial logistic regression
require(leaps)           # For regsubsets
require(pls)             # For segments
require(groupdata2)      # For fold
require(boot)            # For cv.glm
require(FactoMineR)      # For PCA,MCA,...
require(lme4)            # For lmer,glmer
library(shiny)
require(shinydashboard)

# # Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title = "Cars CO2"),
        #Sidebar content
        dashboardSidebar(
            sidebarMenu(
                menuItem("PCAs of Indiviuals and Variables", tabName = "pca", icon = icon("")),
                menuItem("Model Selection", tabName = "model", icon = icon("")),
                menuItem("Source Code for app", tabName = "code", icon=icon("file-code")),
                menuItem("Datas",tabName = "data", icon=icon("th"))
            )
        ),
        dashboardBody(
            tabItems(
                #First tab content
                tabItem(tabName = "pca",
                        fluidRow(
                            box(width = 12,
                                column(tabBox(id = "PCA_tab",
                                              tabPanel("Boxplot",
                                                       box((radioButtons(inputId = "VarBox", label= "Select variables for CO2 Boxplot", selected = names(cars[,1]),
                                                                         choices = names(cars[c(1:3,6:7)])))
                                                       )
                                              ),
                                              tabPanel("Nuage de points"),
                                              tabPanel("ACP",
                                                       box((checkboxGroupInput(inputId = "ACPCheck", label = "Select variables for ACP", selected = names(cars[,c(4:5,8:11)]),
                                                                               choices = names(cars[,c(4:5,8:11)]))),
                                                           tabPanel("Variables",plotOutput("graph_pca_var")),
                                                           tabPanel("Individuals",plotOutput("graph_pca_ind")),width = 12
                                                       )
                                              ),
                                              tabPanel("Matrice de Corrélation"),width=12), 
                                       width=12
                                )
                            )
                        )
                ),
                #Second tab content
                tabItem(tabName = "model",
                        fluidRow(
                            box(checkboxGroupInput(inputId = "Model Var", label = "Select the number of variables for Model", selected = names(cars[,c(4:5,8:11)]),
                                                   choices = names(cars[,c(1:11)])),
                            )
                        )
                ),
                tabItem(tabName="code",
                        fluidRow(
                        )
                )
            )
        )
    )
)