# # Define UI for application that draws a histogram
# User interface
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
                                                              (radioButtons(inputId = "VarBox", label= "Sélectionnez les variables qualitatives à représenter pour expliquer les emmissions de CO2", 
                                                                            selected = names(cars[,1]),
                                                                            choices = c("Marque" = names(cars[1]),
                                                                                        "Modèle" = names(cars[2]),
                                                                                        "Classe" = names(cars[3]),
                                                                                        "Transmission" = names(cars[6]),
                                                                                        "Type d'essence" = names(cars[7])
                                                                            )
                                                              )
                                                              )
                                                       ),
                                                       column(width = 8,
                                                              plotlyOutput("moustache")
                                                       )
                                                   )
                                          ),
                                          tabPanel("Nuage de points",
                                                   fluidRow(
                                                       column(width = 4,
                                                              (radioButtons(inputId = "VarScat", label= "Sélectionnez les variables quantitatives à représenter pour expliquer les emmissions de CO2", 
                                                                            selected = names(cars[,4]),
                                                                            choices = c("Taille du moteur (en L)" = names(cars[4]),
                                                                                        "Nombre de cylindres dans le moteur" = names(cars[5]),
                                                                                        "Consommation de carburant en ville (L/100 km)" = names(cars[8]),
                                                                                        "Consommation de carburant sur autoroute (L/100 km)" = names(cars[9]),
                                                                                        "Consommation de carburant combinée (55 % en ville, 45 % sur route) (L/100 km)" = names(cars[10]),
                                                                                        "Consommation de carburant combinée (55 % en ville, 45 % sur route) (miles per gallon)" = names(cars[11])
                                                                            )))
                                                       ),
                                                       column(width = 8,
                                                              plotlyOutput("scat")
                                                       )
                                                   )
                                          ),
                                          tabPanel("ACP",
                                                   fluidRow(
                                                       column(width=4,(checkboxGroupInput(inputId = "ACPCheck", label = "Sélectionnez les variables principales pour réaliser une ACP", selected = names(cars[,c(4:5,8:11)]),
                                                                                          choices = c("Taille du moteur (en L)" = names(cars[4]),
                                                                                                      "Nombre de cylindres dans le moteur" = names(cars[5]),
                                                                                                      "Consommation de carburant en ville (L/100 km)" = names(cars[8]),
                                                                                                      "Consommation de carburant sur autoroute (L/100 km)" = names(cars[9]),
                                                                                                      "Consommation de carburant combinée (55 % en ville, 45 % sur route) (L/100 km)" = names(cars[10]),
                                                                                                      "Consommation de carburant combinée (55 % en ville, 45 % sur route) (miles per gallon)" = names(cars[11])
                                                                                                      )
                                                                                          )
                                                                       )
                                                       ),
                                                       column(width=8,
                                                              tabBox(width = 12,
                                                                     id="PCA_tab",
                                                                     tabPanel("Variables",plotOutput("graph_pca_var")),
                                                                     tabPanel("Individuals",plotOutput("graph_pca_ind"))
                                                              )
                                                       )
                                                   )
                                          ),
                                          tabPanel("Matrice de Corrélation",
                                                   fluidRow(width = 12,
                                                            column(width = 12,
                                                                   align="center",
                                                                   plotOutput("matcorr",width = "90%")
                                                            )
                                                            
                                                   )
                                          ),width=12), 
                            )
                        )
                ),
                #Second tab content
                tabItem(tabName = "model",
                        fluidRow(
                            column(width = 12, 
                                   fluidRow( # fluidRow1 (selections + graph poids variables)
                                       column(width = 8, #colone graph
                                              box(width = 12,
                                                  title = "Poids des variables dans le modèle",
                                                  status = "info",
                                                  plotOutput("coef"),
                                                  sliderInput(inputId = "Nbvar", # Nb var dans le modele
                                                              label = "Nombre de variables à 
                                                   inclure dans le modèle", 
                                                              min = 1,
                                                              max = 6,
                                                              value = 6)
                                              )
                                       ),
                                       column(width = 4, # colone selection des criteres
                                              box(width = 12,
                                                  title = "Sélection exhaustive des variables", 
                                                  status = "info",
                                                  tabBox(width= 12,id = "Selection",
                                                         tabPanel("RSS",
                                                                  fluidRow(
                                                                      column(width = 12,
                                                                             plotOutput("Plrss"))
                                                                  )
                                                         ),
                                                         tabPanel("BIC",
                                                                  fluidRow(
                                                                      column(width=12,
                                                                             plotOutput("Plbic")
                                                                      )
                                                                  )
                                                         ),
                                                         tabPanel("AIC",
                                                                  fluidRow(
                                                                      column(width = 12,
                                                                             plotOutput("Plaic")
                                                                      )
                                                                  )
                                                         )
                                                  )
                                              )
                                       )
                                   ),
                                   fluidRow( #↑ fluidrow 2 (summary du modele + estimation de la perf)
                                       column(width = 6, # colonne summary
                                              box(width = 12,
                                                  title = "Summary",
                                                  status = "info",
                                                  verbatimTextOutput("suM")
                                              )
                                       ),
                                       column(width = 6, # colonne estimation de la performance
                                              box(width = 12,
                                                  title = "Estimation de la performance",
                                                  status = "info",
                                                  "RMSE",
                                                  br(),
                                                  "Scatter plots"
                                              )
                                       )
                                   ),
                                   
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
    )
)