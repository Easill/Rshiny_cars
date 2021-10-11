# User interface

shinyUI(
    # type de l'interface
    dashboardPage(
        skin = "purple",
        dashboardHeader(title = HTML(paste0("Cars CO",tags$sub("2")," emissions")),
                        titleWidth = 250
        ),
        # Contenu de la sidebar
        dashboardSidebar(
            sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home", lib = "font-awesome")),
                menuItem("Visual Analysis", tabName = "pca", icon = icon("chart-pie")),
                menuItem("Model Selection", tabName = "model", icon = icon("wave-square")),
                menuItem("Datas",tabName = "data", icon=icon("th")),
                menuItem("Source Code for app", tabName = "code", icon=icon("file-code")),
                menuItem("About", tabName = "about", icon=icon("info-circle"))
            )
        ),
        # contenu du corps de la page
        dashboardBody(
            tags$style(HTML(
                '.nav-tabs-custom .nav-tabs li.active {border-top-color: #00c0ef;}'
            )), #pour la couleur des onglets
            tabItems(
                #First tab content 
                tabItem(tabName = "home",
                        fluidRow(
                            column(width = 7,
                                   h3("Contexte"),
                                   p(
                                       "L'accumulation de", tags$b("gaz à effet de serre"), "dans l'atmosphère est la principale
                                   cause du changement climatique. Or, les transports, en particulier les voitures
                                   sont une", tags$b("source importante"), "d'emmission de gaz à effet de serre.",
                                       br(),
                                       "Il est donc essentiel d'étudier les", tags$b("causes"), "d'emmission de CO",tags$sub("2"),
                                       " de ces véhicules afin de pouvoir réduire leur impact sur", tags$b("l'environnement"),".",
                                       br(),
                                       style = "background-color:#e1d2b8;padding:20px;border-radius:10px;text-align:justify;font-size:18px"),
                                   h3("Objectifs et données utilisées"),
                                   p(
                                       "Cette application a pour objectif de chercher les variables qui ont le 
                                   plus d'impact sur les emmissions de CO",tags$sub("2")," d'une voiture.",
                                       br(),
                                       "Les données utilisées pour répondre à cette question ont été prises et 
                                   compilées à partir du site officiel du",
                                       a("gouvernement Canadien", href="https://open.canada.ca/data/en/dataset/98f1a129-f628-4ce4-b24d-6f16bf24dd64#wb-auto-6"),
                                       "Le jeu de données recoupe ainsi des données sur une période de 7 ans.",
                                       br(),
                                       "La variable à expliquer est alors", tags$b("l'émission de CO2"),"d'un véhicule, une variable",tags$b("quantitative"),
                                       "Il y a",tags$b("11 variables explicatives"), "qui sont à la fois",tags$b("qualitatives"),"(la marque, le modèle, 
                                   la classe, le nombre de cylindres, la transmission, le type de carburant) et", tags$b("quantitatives"),"
                                   (la taille du réservoir, la consommation de carburant en ville, la consommation de carburant 
                                   sur l'autoroute, la consommation de carburant combinée).",
                                       style = "background-color:#e1d2b8;padding:15px;border-radius:10px;text-align:justify;font-size:18px")
                                   
                            ),
                            column(width=5,
                                   tags$img(src="pollution.jpg",width="459px",height="287px") # ajout de l'image
                            )
                        )
                ),
                tabItem(tabName = "pca",
                        fluidRow(
                            column(width = 12, 
                                   tabBox(id = "plots_tab",
                                          tabPanel("Boxplot",
                                                   fluidRow(
                                                       column(width = 4,
                                                              (prettyRadioButtons(inputId = "VarBox", label= "Sélectionnez les variables qualitatives à représenter pour expliquer les emissions de CO2", 
                                                                                  selected = names(cars[,1]),
                                                                                  choices = var_quali,
                                                                                  icon = icon("check"),
                                                                                  bigger = TRUE,
                                                                                  status = "info",
                                                                                  animation = "jelly"
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
                                                              (prettyRadioButtons(inputId = "VarScat", label= "Sélectionnez les variables quantitatives à représenter pour expliquer les emissions de CO2", 
                                                                                  selected = names(cars[,4]),
                                                                                  choices = var_quanti,
                                                                                  icon = icon("check"),
                                                                                  bigger = TRUE,
                                                                                  status = "info",
                                                                                  animation = "jelly"
                                                              )
                                                              ),
                                                       ),
                                                       column(width = 8,
                                                              plotlyOutput("scat")
                                                       )
                                                   )
                                          ),
                                          tabPanel("ACP",
                                                   fluidRow(
                                                       column(width=4,(prettyCheckboxGroup(inputId = "ACPCheck", label = "Sélectionnez les variables principales pour réaliser une ACP", selected = names(cars[,c(4:5,8:11)]),
                                                                                           choices = var_quanti,
                                                                                           icon = icon("check-square"),
                                                                                           animation = "jelly"
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
                                          ),width=12) 
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
                                                  plotlyOutput("coef"),
                                                  chooseSliderSkin("Shiny", color = "#00c0ef"
                                                  ),
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
                                   fluidRow( # fluidrow 2 (summary du modele + estimation de la perf)
                                       column(width = 6, # colonne summary
                                              box(width = 12,
                                                  title = "Summary",
                                                  status = "info",
                                                  h3("Coefficients du modèle selectionné :"),
                                                  verbatimTextOutput("suM")
                                              )
                                       ),
                                       column(width = 6, # colonne estimation de la performance
                                              box(width = 12,
                                                  title = "Estimation de la performance",
                                                  status = "info",
                                                  textOutput("rmse"),
                                                  br(),
                                                  plotlyOutput("scatter")
                                              )
                                       )
                                   )
                                   
                            )
                        )
                ),
                tabItem(tabName="data",
                        fluidRow(
                            column(width = 12,
                                   tabBox(width = 12,
                                          id="tableaux",
                                          tabPanel("Résumé",
                                                   verbatimTextOutput("summary")
                                          ),
                                          tabPanel("Données brutes",
                                                   dataTableOutput("tab")
                                          )
                                   )
                            )
                        )
                        
                ),
                tabItem(tabName="code",
                        fluidRow(
                            column(width=12,
                                   tabBox(width = 12,
                                          id="Codes",
                                          tabPanel(title = "ui.R",
                                                   fluidRow(
                                                       column(width = 12,
                                                              verbatimTextOutput("ui")
                                                       )
                                                   )
                                          ),
                                          
                                          tabPanel(title="server.R",
                                                   fluidRow(
                                                       column(width = 12,
                                                              verbatimTextOutput("serveur")
                                                       )
                                                   )
                                          ),
                                          tabPanel(title="Global.R",
                                                   fluidRow(
                                                       column(width = 12,
                                                              verbatimTextOutput("glbl")
                                                       )
                                                   )
                                          )
                                   )
                            )
                        )
                ),
                tabItem(tabName="about",
                        fluidRow(
                            column(width=7,
                                   h3("Cadre du Projet"),
                                   p("Cette application ",tags$b("Rshiny")," s'inscrit dans un projet de création Rshiny pour le cours ",tags$b("Analyse de données massives sous R, supervisé par Benoit Thieurmel."),
                                     br(),
                                     "Pour ce projet nous avons utilisé le jeu de données ",tags$b("cars.csv")," téléchargé sur le site ",tags$b("Kaggle,"),"accessible en cliquant",a("ici.",href="https://www.kaggle.com/debajyotipodder/co2-emission-by-vehicles"),
                                     style = "background-color:#e1d2b8;padding:15px;border-radius:10px;text-align:justify;font-size:18px"
                                   ),
                                   h3("A propos des auteurs"),
                                   p("Nous sommes des étudiants actuellement en master",tags$b("sciences des données"),"au sein de l'institut agro basé à Rennes.",
                                     "Ce projet est disponible sur",tags$b("github"),"sur lequel sera aussi disponible nos autres projets.",
                                     style = "background-color:#e1d2b8;padding:15px;border-radius:10px;text-align:justify;font-size:18px"),
                                   br(),
                                   h3("Liste des packages utilisés"),
                                   br(),
                                   p("-shiny",
                                     br(),
                                     "-shinyWidgets",
                                     br(),
                                     "-nnet",
                                     br(),
                                     "-leaps",
                                     br(),
                                     "-pls",
                                     br(),
                                     "-groupdata2",
                                     br(),
                                     "-boot",
                                     br(),
                                     "-FactoMineR",
                                     br(),
                                     "-lme4",
                                     br(),
                                     "-shiny",
                                     br(),
                                     "-shinydashboard",
                                     br(),
                                     "-ggplot2",
                                     br(),
                                     "-forcats",
                                     br(),
                                     "-plotly"
                                   ),
                                   br(),
                                   tags$b("Contact"),
                                   br(),
                                   a(icon("linkedin"),"@florence-ghestem-data-sciences/",href="https://www.linkedin.com/in/florence-ghestem-data-sciences/"),
                                   br(),
                                   a(icon("linkedin"),"@elias-hermance-datascience/",href="https://www.linkedin.com/in/elias-hermance-datascience/"),
                                   br(),
                                   a(icon("github"),"Easill",href="https://github.com/Easill"),
                                   br(),
                                   a(icon("github"),"floghes",href="https://github.com/floghes")
                            ),
                            column(width=5,
                                   a(tags$img(src="images.jpg",width="485px",height="104px"),href="https://www.agrocampus-ouest.fr/"), # ajout de l'image
                                   br(),
                                   br(),
                                   a(tags$img(src="Kaggle_logo.png",width="485px",height="154px"),href="https://www.kaggle.com/") # ajout de l'image
                            )
                        )
                )
            )
        )
    )
)

