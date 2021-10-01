#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                            box(checkboxGroupInput(inputId = "ACPCheck", label = "Select variables for ACP", selected = names(cars[,c(4:5,8:11)]),
                                                   choices = names(cars[,c(4:5,8:11)])),width = 2,
                                actionButton(inputId = "Supp",label = "Display supplementary",icon = icon("bar-chart-o"))),
                            tabBox(title = "PCAs",
                                   id = "PCA_tab", height = "250px",
                                   tabPanel("Var",plotOutput("graph_pca_var")),
                                   tabPanel("Ind",plotOutput("graph_pca_ind")))
                        )
                ),
                #Second tab content
                tabItem(tabName = "model",
                        fluidRow(
                            box(box(checkboxGroupInput(inputId = "Model Var", label = "Select variables for Model", selected = names(cars[,c(4:5,8:11)]),
                                                       choices = names(cars[,c(1:11)])),
                            )
                            )
                        )
                ),
                tabItem(tabName="code",
                        fluidRow(
                            
                        ))
            )
        )
    )
)
# # Application title
# titlePanel("Etude du jeu de données cars.csv"),
# 
# # Sidebar with a slider input for number of bins
# sidebarLayout(
#     sidebarPanel(
#         checkboxGroupInput(inputId = "ACPCheck", label = "Select variables", selected = 6,
#                            choices = names(cars[,c(4:5,8:11)])),
#         actionButton(inputId = "Supp",label = "Display supplementary",icon = icon("bar-chart-o"))
#     ),

# Show a plot of the generated distribution

# navbarPage(
#     title = "Shiny App cars",
#     tabPanel(title = "Détail des données",
#              "PCA"),
#     tabPanel(title = "ACP",
#              sidebarPanel(
#                  checkboxGroupInput(inputId = "ACPCheck", label = "Select variables", selected = 6,
#                                     choices = names(cars[,c(4:5,8:11)])),
#                  actionButton(inputId = "Supp",label = "Display supplementary",icon = icon("bar-chart-o"))),
#              mainPanel(
#                  plotOutput("graph_pca_ind"),
#                  plotOutput("graph_pca_var"))
#              
#     )
# )