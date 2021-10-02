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
                                                              (radioButtons(inputId = "VarBox", label= "Select variables for CO2 Boxplot", selected = names(cars[,1]),
                                                                            choices = names(cars[c(1:3,6:7)])))
                                                       ),
                                                       column(width = 8,
                                                              plotOutput("moustache")
                                                       )
                                                   )
                                          ),
                                          tabPanel("Nuage de points",
                                                   fluidRow(
                                                       column(width = 4,
                                                              (radioButtons(inputId = "VarScat", label= "Select variables for CO2 Scatter Plot", selected = names(cars[,4]),
                                                                            choices = names(cars[c(4:5,8:11)])))
                                                       ),
                                                       column(width = 8,
                                                              plotOutput("scat")
                                                       )
                                                   )
                                          ),
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