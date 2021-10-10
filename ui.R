# # Define UI for application that draws a histogram
# User interface
shinyUI(
    dashboardPage(
        skin = "red",
        dashboardHeader(title = HTML(paste0("Cars CO",tags$sub("2")," emissions")),
                        titleWidth = 250
        ),
        #Sidebar content
        dashboardSidebar(
            sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home", lib = "font-awesome")),
                menuItem("Visual Analysis", tabName = "pca", icon = icon("chart-pie")),
                menuItem("Model Selection", tabName = "model", icon = icon("wave-square")),
                menuItem("Datas",tabName = "data", icon=icon("th")),
                menuItem("Source Code for app", tabName = "code", icon=icon("file-code"))
            )
        ),
        dashboardBody(
            tabItems(
                #First tab content 
                tabItem(tabName = "home",
                        fluidRow(
                            column(width = 7,
                                   br(),
                                   p("
                                   Ce jeu de données montre comment les émissions de CO2 d'un véhicule peuvent varier 
                                   en fonction de ses différentes variables Les données ont été prises et compilées à 
                                   partir du site officiel",
                                     a("gouvernement Canadien", href="https://open.canada.ca/data/en/dataset/98f1a129-f628-4ce4-b24d-6f16bf24dd64#wb-auto-6"),
                                     "Le jeu de données contient des données sur une période de 7 ans.
                                   La variable à expliquer est alors l'émission de CO2 d'un véhicule, une variable quantitative. 
                                   Il y a 11 variables explicatives qui sont à la fois qualitatives (la marque, le modèle, 
                                   la classe, le nombre de cylindres, la transmission, le type de carburant) et quantitatives 
                                   (la taille du réservoir, la consommation de carburant en ville, la consommation de carburant 
                                   sur l'autoroute, la consommation de carburant combinée).",
                                     style = "background-color:LightBlue;padding:15px;border-radius:10px")
                                   
                            ),
                            column(width=5,
                                   tags$img(src="pollution.jpg",width="459px",height="287px")
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
                                                                                  status = "warning",
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
                                                                                  status = "warning",
                                                                                  animation = "jelly"
                                                                                  )
                                                              ),
                                                              align = "text-align: justify"
                                                       ),
                                                       column(width = 8,
                                                              plotlyOutput("scat")
                                                       )
                                                   )
                                          ),
                                          tabPanel("ACP",
                                                   fluidRow(
                                                       column(width=4,(prettyCheckboxGroup(inputId = "ACPCheck", label = "Sélectionnez les variables principales pour réaliser une ACP", selected = names(cars[,c(4:5,8:11)]),
                                                                                           choices = c("Taille du moteur (en L)" = names(cars[4]),
                                                                                                       "Nombre de cylindres dans le moteur" = names(cars[5]),
                                                                                                       "Consommation de carburant en ville (L/100 km)" = names(cars[8]),
                                                                                                       "Consommation de carburant sur autoroute (L/100 km)" = names(cars[9]),
                                                                                                       "Consommation de carburant combinée (55 % en ville, 45 % sur route) (L/100 km)" = names(cars[10]),
                                                                                                       "Consommation de carburant combinée (55 % en ville, 45 % sur route) (miles per gallon)" = names(cars[11])
                                                                                           ),
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
                                                  textOutput("title"),
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
                                   ),
                                   
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
                            column(width = 12,
                                   tabBox(width = 12,
                                          id="tableaux",
                                          tabPanel("Résumé",verbatimTextOutput("summary")),
                                          tabPanel("Données brutes",tableOutput("tab"))
                                   )
                            )
                        )
                )
            )
        )
    )
)