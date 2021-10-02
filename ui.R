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
                menuItem("Visual Analysis", tabName = "pca", icon = icon("")),
                menuItem("Model Selection", tabName = "model", icon = icon("")),
                menuItem("Datas",tabName = "data", icon=icon("th")),
                menuItem("Source Code for app", tabName = "code", icon=icon("file-code"))
            )
        ),
        dashboardBody(
            tabItems(
                #First tab content
                tabItem(tabName = "pca",
                        fluidRow(
                            column(width = 12, 
                                   tabBox(id = "plots_tab",
                                          tabPanel("Boxplot",
                                                   fluidRow(
                                                       column(width = 4,
                                                              (radioButtons(inputId = "VarBox", label= "Select variables for CO2 Boxplot", selected = names(cars[,1]),
                                                                            choices = names(cars[c(1:3,6:7)])))
                                                       ),
                                                       column(width = 8,
                                                              plotOutput("moustache")
                                                       )
                                                   )
                                          ),
                                          tabPanel("Nuage de points"),
                                          tabPanel("ACP",
                                                   fluidRow(
                                                       column(width=4,(checkboxGroupInput(inputId = "ACPCheck", label = "Select variables for ACP", selected = names(cars[,c(4:5,8:11)]),
                                                                                          choices = names(cars[,c(4:5,8:11)])))
                                                       ),
                                                       column(width=8,
                                                              tabBox(id="PCA_tab",
                                                                     tabPanel("Variables",plotOutput("graph_pca_var")),
                                                                     tabPanel("Individuals",plotOutput("graph_pca_ind"))
                                                              )
                                                       )
                                                   )
                                          ),
                                          tabPanel("Matrice de Corrélation"),width=12), 
                            )
                        )
                ),
                #Second tab content
                tabItem(tabName = "model",
                        fluidRow(
                            column(width = 12, 
                                   fluidRow(
                                       column(width = 3,radioButtons(inputId = "SubMod", label = "Select the criteria to get the best SubModel", selected = "RSS",
                                                                     choices = c("AIC","BIC","RSS")),
                                              sliderInput(inputId = "NbVar",label = "Best submodel with variables",min = 1,max = 6,value = 2,step = 1)
                                       ),
                                       column(width = 5, 
                                              ),#plot output
                                       column(width= 4, )
                                   )
                            )
                        )
                ),
                tabItem(tabName="code",
                        fluidRow(
                            column(width = 12,
                                   )
                        )
                ),
                tabItem(tabName="data",
                        fluidRow(
                        )
                )
            )
        )
    )
)