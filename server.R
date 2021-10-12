# Serveur

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # premier onglet
    
    # graph explicatifs boxplot pour les variables quali
    output$moustache <- renderPlotly({
        maxtauxCO2<-cars[cars$CO2>150,] 
        p<-ggplot(maxtauxCO2, aes(x= fct_rev(fct_reorder(maxtauxCO2[,input$VarBox],CO2,.fun="mean")), y=CO2)) +
            geom_boxplot(fill="slateblue", alpha=0.2)+
            ggtitle(paste0("Boxplot des modalités de la variable ", names(var_quali[var_quali==input$VarBox])," les plus émettrices de CO2")) +
            xlab(names(var_quali[var_quali==input$VarBox]))+
            # règle le soucis des axes qui different en nombre de modalités
            if(length(levels(maxtauxCO2[,input$VarBox]))>100){
                theme(axis.text.x=element_blank(),
                      axis.ticks = element_blank())
            } else {
                theme(plot.title = element_text(hjust = 0.5),
                      axis.text.x = element_text(hjust = 0, vjust = 1))
                
                if(length(levels(maxtauxCO2[,input$VarBox]))>5){
                    theme(plot.title = element_text(hjust = 0.5),
                          axis.text.x = element_text(angle = 320, hjust = 0, vjust = 1))
                }
                
            }
        
        ggplotly(p) # pour le rendre interactif
        
    })
    
    # les nuages de points pour les variables quanti
    
    output$scat <- renderPlotly({
        p2<-ggplot(cars, aes(x= cars[,input$VarScat], y=CO2)) +
            geom_point(colour="black", alpha=0.5)+
            ggtitle(HTML(paste0("Emmissions de CO", tags$sub("2")," en fonction de la variable ", names(var_quanti[var_quanti==input$VarScat])))) +
            xlab(names(var_quanti[var_quanti==input$VarScat]))+
            ylab(HTML(paste0("Emmissions de CO", tags$sub("2"))))+
            theme(plot.title = element_text(hjust = 0.5))
        ggplotly(p2)
    })
    
    # les graphs de l'ACP
    
    res_pca <- reactive({
        PCA(cars[,input$ACPCheck], quali.sup = input$qualiCheck, graph = FALSE)
    })
    
    output$graph_pca_ind <- renderPlot({
        plot.PCA(res_pca(), choix = "ind", axes = c(1,2))
    })
    output$graph_pca_var <- renderPlot({
        plot.PCA(res_pca(), choix = "var", axes = c(1,2))
    })
    
    
    # plot de la matrice de corrélation des variables quanti
    output$matcorr<-renderPlot({
        ggplot(meltcormat, aes(x = x, y = y, fill = value)) +
            geom_tile(color = "black",
                      lwd = 1,
                      linetype = 1)+
            scale_fill_gradient(low = "blue", high = "red")+
            geom_text(aes(label = round(value,2)), color = "white", size = 4) +
            labs(x = "",
                 y ="",
                 title = "Matrice de corrélation") +
            theme(plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 320, hjust = 0, vjust = 1))
    })
    
    # Deuxième onget
    # poids des variables
    
    # modele
    Bestmod <- reactive({
        glm(CO2~.,data=newdata[,select$which[input$Nbvar,]])
    })
    
    # on reccupere les coeffs
    coeff<-reactive({
        as.data.frame(summary(Bestmod())$coefficients)
    })
    
    
    # plot du poids des variables (coefficients)
    output$coef <- renderPlotly({
        p<-ggplot(coeff()[-1,], aes(x= (rownames(coeff()[-1,])), y=coeff()[-1,1]))+
            geom_bar(stat="identity", fill="#00c0ef", alpha = 0.6)+
            ggtitle("Poids des variables dans le modèle") +
            xlab("Variables")+
            ylab("Scores")+
            theme(plot.title = element_text(hjust = 0.5, size = 10),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15)
            )
        ggplotly(p)
    })
    
    # optimisation du critère par cross validation
    cvmod <- reactive({
        cv.glm(newdata,Bestmod(),K=10)
    })
    
    # on sort le RMSE
    output$rmse <- renderText({
        c("RMSE Ajusté :",round(cvmod()$delta[2],2))
    })
    
    # plots optimisation des critères AIC, BIC et RSS en fonction du nb
    # de variables prises en compte dans le modèle
    output$Plbic <- renderPlot({
        plot(1:6,bic,pch=16,bty="l",type="b",xlab="Number of explanatory variables",
             ylab="Information criterion",ylim=range(bic),col="darkgray",
             main="Exhaustive model selection",cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
        legend("topleft",lwd=2,lty=1,pch=c(16,17),col=c("darkgray"),bty="n",cex=1.25,legend="BIC")
        grid()
    })
    output$Plrss <- renderPlot({
        plot(1:6,rss,pch=16,bty="l",type="b",xlab="Number of explanatory variables",
             ylab="RSS",ylim=range(rss),col="coral1",
             main="Exhaustive model selection",cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
        legend("topleft",lwd=2,lty=1,pch=c(16,17),col=c("coral1"),bty="n",cex=1.25,legend="RSS")
        grid()
    })
    output$Plaic <- renderPlot({
        plot(1:6,aic,pch=16,bty="l",type="b",xlab="Number of explanatory variables",
             ylab="Information criterion",ylim=range(aic),col="red",
             main="Exhaustive model selection",cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
        legend("topleft",lwd=2,lty=1,pch=c(16,17),col=c("red"),bty="n",cex=1.25,legend="AIC")
        grid()
    })
    
    output$suM <- renderPrint({
        summary(Bestmod())$coef
    }) #Résulats du modèle
    
    
    # préparation des données pour le scatterplot
    fitted.co2_2 <- reactive({
        fitted(Bestmod())
    }) # Fitted LMP values
    
    observed.co2_2 <- reactive({
        newdata$CO2
    }) # Observed LMP values
    
    # concaténation
    dataplot <- reactive({
        data.frame(observed.co2_2(),fitted.co2_2())
    }) 
    
    # scatter plot données fittées et observées
    output$scatter <- renderPlotly({
        p3 <- ggplot(dataplot(),aes(x=observed.co2_2(),y=fitted.co2_2())) +
            geom_point(colour="black",alpha=0.5)+
            ggtitle(HTML(paste0("Fitted versus observed CO2 values avec ",input$Nbvar," variables")))+
            xlab("Observed CO2") + ylab("Fitted CO2")+
            theme(plot.title=element_text((hjust=0.5)))+
            geom_abline(slope = 1,intercept = 0,col="red")
        ggplotly(p3)
    })
    
    
    # Onglet data 
    
    output$tab<-renderDataTable({
        cars
    })
    output$summary<-renderPrint({
        summary(cars)
    })
    
    #onglet code 
    
    output$serveur <- renderText({
        "# Serveur

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # premier onglet
    
    # graph explicatifs boxplot pour les variables quali
    output$moustache <- renderPlotly({
        maxtauxCO2<-cars[cars$CO2>150,] 
        p<-ggplot(maxtauxCO2, aes(x= fct_rev(fct_reorder(maxtauxCO2[,input$VarBox],CO2,.fun='mean')), y=CO2)) +
            geom_boxplot(fill='slateblue', alpha=0.2)+
            ggtitle(paste0('Boxplot des modalités de la variable ', names(var_quali[var_quali==input$VarBox]),' les plus émettrices de CO2')) +
            xlab(names(var_quali[var_quali==input$VarBox]))+
            # règle le soucis des axes qui different en nombre de modalités
            if(length(levels(maxtauxCO2[,input$VarBox]))>100){
                theme(axis.text.x=element_blank(),
                      axis.ticks = element_blank())
            } else {
                theme(plot.title = element_text(hjust = 0.5),
                      axis.text.x = element_text(hjust = 0, vjust = 1))
                
                if(length(levels(maxtauxCO2[,input$VarBox]))>5){
                    theme(plot.title = element_text(hjust = 0.5),
                          axis.text.x = element_text(angle = 320, hjust = 0, vjust = 1))
                }
                
            }
        
        ggplotly(p) # pour le rendre interactif
        
    })
    
    # les nuages de points pour les variables quanti
    
    output$scat <- renderPlotly({
        p2<-ggplot(cars, aes(x= cars[,input$VarScat], y=CO2)) +
            geom_point(colour='black', alpha=0.5)+
            ggtitle(HTML(paste0('Emmissions de CO', tags$sub('2'),' en fonction de la variable ', names(var_quanti[var_quanti==input$VarScat])))) +
            xlab(names(var_quanti[var_quanti==input$VarScat]))+
            ylab(HTML(paste0('Emmissions de CO', tags$sub('2'))))+
            theme(plot.title = element_text(hjust = 0.5))
        ggplotly(p2)
    })
    
    # les graphs de l'ACP
    
    res_pca <- reactive({
        PCA(cars[,input$ACPCheck], quali.sup = input$qualiCheck, graph = FALSE)
    })
    
    output$graph_pca_ind <- renderPlot({
        plot.PCA(res_pca(), choix = 'ind', axes = c(1,2))
    })
    output$graph_pca_var <- renderPlot({
        plot.PCA(res_pca(), choix = 'var', axes = c(1,2))
    })
    
    
    # plot de la matrice de corrélation des variables quanti
    output$matcorr<-renderPlot({
        ggplot(meltcormat, aes(x = x, y = y, fill = value)) +
            geom_tile(color = 'black',
                      lwd = 1,
                      linetype = 1)+
            scale_fill_gradient(low = 'blue', high = 'red')+
            geom_text(aes(label = round(value,2)), color = 'white', size = 4) +
            labs(x = '',
                 y ='',
                 title = 'Matrice de corrélation') +
            theme(plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 320, hjust = 0, vjust = 1))
    })
    
    # Deuxième onget
    # poids des variables
    
    # modele
    Bestmod <- reactive({
        glm(CO2~.,data=newdata[,select$which[input$Nbvar,]])
    })
    
    # on reccupere les coeffs
    coeff<-reactive({
        as.data.frame(summary(Bestmod())$coefficients)
    })
    
    
    # plot du poids des variables (coefficients)
    output$coef <- renderPlotly({
        p<-ggplot(coeff()[-1,], aes(x= (rownames(coeff()[-1,])), y=coeff()[-1,1]))+
            geom_bar(stat='identity', fill='#00c0ef', alpha = 0.6)+
        ggtitle('Poids des variables dans le modèle') +
            xlab('Variables')+
            ylab('Scores')+
            theme(plot.title = element_text(hjust = 0.5, size = 10),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15)
            )
        ggplotly(p)
    })
    
    # optimisation du critère par cross validation
    cvmod <- reactive({
        cv.glm(newdata,Bestmod(),K=10)
    })
    
    # on sort le RMSE
    output$rmse <- renderText({
        c('RMSE Ajusté :',round(cvmod()$delta[2],2))
    })
    
    # plots optimisation des critères AIC, BIC et RSS en fonction du nb
    # de variables prises en compte dans le modèle
    output$Plbic <- renderPlot({
        plot(1:6,bic,pch=16,bty='l',type='b',xlab='Number of explanatory variables',
             ylab='Information criterion',ylim=range(bic),col='darkgray',
             main='Exhaustive model selection',cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
        legend('topleft',lwd=2,lty=1,pch=c(16,17),col=c('darkgray'),bty='n',cex=1.25,legend='BIC')
        grid()
    })
    output$Plrss <- renderPlot({
        plot(1:6,rss,pch=16,bty='l',type='b',xlab='Number of explanatory variables',
             ylab='RSS',ylim=range(rss),col='coral1',
             main='Exhaustive model selection',cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
        legend('topleft',lwd=2,lty=1,pch=c(16,17),col=c('coral1'),bty='n',cex=1.25,legend='RSS')
        grid()
    })
    output$Plaic <- renderPlot({
        plot(1:6,aic,pch=16,bty='l',type='b',xlab='Number of explanatory variables',
             ylab='Information criterion',ylim=range(aic),col='red',
             main='Exhaustive model selection',cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
        legend('topleft',lwd=2,lty=1,pch=c(16,17),col=c('red'),bty='n',cex=1.25,legend='AIC')
        grid()
    })
    
    output$suM <- renderPrint({
        summary(Bestmod())$coef
    }) #Résulats du modèle
    
    
    # préparation des données pour le scatterplot
    fitted.co2_2 <- reactive({
        fitted(Bestmod())
    }) # Fitted LMP values
    
    observed.co2_2 <- reactive({
        newdata$CO2
    }) # Observed LMP values
    
    # concaténation
    dataplot <- reactive({
        data.frame(observed.co2_2(),fitted.co2_2())
    }) 
    
    # scatter plot données fittées et observées
    output$scatter <- renderPlotly({
        p3 <- ggplot(dataplot(),aes(x=observed.co2_2(),y=fitted.co2_2())) +
            geom_point(colour='black',alpha=0.5)+
            ggtitle(HTML(paste0('Fitted versus observed CO2 values avec ',input$Nbvar,' variables')))+
            xlab('Observed CO2') + ylab('Fitted CO2')+
            theme(plot.title=element_text((hjust=0.5)))+
            geom_abline(slope = 1,intercept = 0,col='red')
        ggplotly(p3)
    })
    
    
    # Onglet data 
    
    output$tab<-renderDataTable({
        cars
    })
    output$summary<-renderPrint({
        summary(cars)
    })"
    })
    
    output$ui <- renderText({"
        # User interface

shinyUI(
    # type de l'interface
    dashboardPage(
        skin = 'purple',
        dashboardHeader(title = HTML(paste0('Cars CO',tags$sub('2'),' emissions')),
                        titleWidth = 250
        ),
        # Contenu de la sidebar
        dashboardSidebar(
            sidebarMenu(
                menuItem('Home', tabName = 'home', icon = icon('home', lib = 'font-awesome')),
                menuItem('Visual Analysis', tabName = 'pca', icon = icon('chart-pie')),
                menuItem('Model Selection', tabName = 'model', icon = icon('wave-square')),
                menuItem('Datas',tabName = 'data', icon=icon('th')),
                menuItem('Source Code for app', tabName = 'code', icon=icon('file-code')),
                menuItem('About', tabName = 'about', icon=icon('info-circle'))
            )
        ),
        # contenu du corps de la page
        dashboardBody(
            tags$style(HTML(
                '.nav-tabs-custom .nav-tabs li.active {border-top-color: #00c0ef;}'
            )), #pour la couleur des onglets
            tabItems(
                #First tab content 
                tabItem(tabName = 'home',
                        fluidRow(
                            column(width = 7,
                                   h3('Contexte'),
                                   p(
                                       'L'accumulation de', tags$b('gaz à effet de serre'), 'dans l'atmosphère est la principale
        cause du changement climatique. Or, les transports, en particulier les voitures
        sont une', tags$b('source importante'), 'd'emmission de gaz à effet de serre.',
                                       br(),
                                       'Il est donc essentiel d'étudier les', tags$b('causes'), 'd'emmission de CO',tags$sub('2'),
                                       ' de ces véhicules afin de pouvoir réduire leur impact sur', tags$b('l'environnement'),'.',
                                       br(),
                                       style = 'background-color:#e1d2b8;padding:20px;border-radius:10px;text-align:justify;font-size:18px'),
            h3('Objectifs et données utilisées'),
        p(
            'Cette application a pour objectif de chercher les variables qui ont le 
                                   plus d'impact sur les emmissions de CO',tags$sub('2'),' d'une voiture.',
            br(),
            'Les données utilisées pour répondre à cette question ont été prises et 
                                   compilées à partir du site officiel du',
            a('gouvernement Canadien', href='https://open.canada.ca/data/en/dataset/98f1a129-f628-4ce4-b24d-6f16bf24dd64#wb-auto-6'),
            'Le jeu de données recoupe ainsi des données sur une période de 7 ans.',
            br(),
            'La variable à expliquer est alors', tags$b('l'émission de CO2'),'d'un véhicule, une variable',tags$b('quantitative'),
            'Il y a',tags$b('11 variables explicatives'), 'qui sont à la fois',tags$b('qualitatives'),'(la marque, le modèle, 
                                   la classe, le nombre de cylindres, la transmission, le type de carburant) et', tags$b('quantitatives'),'
                                   (la taille du réservoir, la consommation de carburant en ville, la consommation de carburant 
                                   sur l'autoroute, la consommation de carburant combinée).',
            style = 'background-color:#e1d2b8;padding:15px;border-radius:10px;text-align:justify;font-size:18px')
        
    ),
    column(width=5,
           tags$img(src='pollution.jpg',width='459px',height='287px') # ajout de l'image
    )
)
),
tabItem(tabName = 'pca',
        fluidRow(
            column(width = 12, 
                   tabBox(id = 'plots_tab',
                          tabPanel('Boxplot',
                                   fluidRow(
                                       column(width = 4,
                                              (prettyRadioButtons(inputId = 'VarBox', label= 'Sélectionnez les variables qualitatives à représenter pour expliquer les emissions de CO2', 
                                                                  selected = names(cars[,1]),
                                                                  choices = var_quali,
                                                                  icon = icon('check'),
                                                                  bigger = TRUE,
                                                                  status = 'info',
                                                                  animation = 'jelly'
                                              )
                                              )
                                       ),
                                       column(width = 8,
                                              plotlyOutput('moustache')
                                       )
                                   )
                          ),
                          tabPanel('Nuage de points',
                                   fluidRow(
                                       column(width = 4,
                                              (prettyRadioButtons(inputId = 'VarScat', label= 'Sélectionnez les variables quantitatives à représenter pour expliquer les emissions de CO2', 
                                                                  selected = names(cars[,4]),
                                                                  choices = var_quanti,
                                                                  icon = icon('check'),
                                                                  bigger = TRUE,
                                                                  status = 'info',
                                                                  animation = 'jelly'
                                              )
                                              ),
                                       ),
                                       column(width = 8,
                                              plotlyOutput('scat')
                                       )
                                   )
                          ),
                          tabPanel('ACP',
                                   fluidRow(
                                       column(width=4,(prettyCheckboxGroup(inputId = 'ACPCheck', label = 'Sélectionnez les variables principales pour réaliser une ACP', selected = names(cars[,c(4:5,8:11)]),
                                                                           choices = var_quanti,
                                                                           icon = icon('check-square'),
                                                                           animation = 'jelly'
                                       )
                                       )
                                       ),
                                       column(width=8,
                                              tabBox(width = 12,
                                                     id='PCA_tab',
                                                     tabPanel('Variables',plotOutput('graph_pca_var')),
                                                     tabPanel('Individuals',plotOutput('graph_pca_ind'))
                                              )
                                       )
                                   )
                          ),
                          tabPanel('Matrice de Corrélation',
                                   fluidRow(width = 12,
                                            column(width = 12,
                                                   align='center',
                                                   plotOutput('matcorr',width = '90%')
                                            )
                                            
                                   )
                          ),width=12) 
            )
        )
),
#Second tab content
tabItem(tabName = 'model',
        fluidRow(
            column(width = 12, 
                   fluidRow( # fluidRow1 (selections + graph poids variables)
                       column(width = 8, #colone graph
                              box(width = 12,
                                  title = 'Poids des variables dans le modèle',
                                  status = 'info',
                                  plotlyOutput('coef'),
                                  chooseSliderSkin('Shiny', color = '#00c0ef'
                                  ),
                                  sliderInput(inputId = 'Nbvar', # Nb var dans le modele
                                              label = 'Nombre de variables à 
                                                              inclure dans le modèle', 
                                              min = 1,
                                              max = 6,
                                              value = 6)
                              )
                       ),
                       column(width = 4, # colone selection des criteres
                              box(width = 12,
                                  title = 'Sélection exhaustive des variables', 
                                  status = 'info',
                                  tabBox(width= 12,id = 'Selection',
                                         tabPanel('RSS',
                                                  fluidRow(
                                                      column(width = 12,
                                                             plotOutput('Plrss'))
                                                  )
                                         ),
                                         tabPanel('BIC',
                                                  fluidRow(
                                                      column(width=12,
                                                             plotOutput('Plbic')
                                                      )
                                                  )
                                         ),
                                         tabPanel('AIC',
                                                  fluidRow(
                                                      column(width = 12,
                                                             plotOutput('Plaic')
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
                                  title = 'Summary',
                                  status = 'info',
                                  h3('Coefficients du modèle selectionné :'),
                                  verbatimTextOutput('suM')
                              )
                       ),
                       column(width = 6, # colonne estimation de la performance
                              box(width = 12,
                                  title = 'Estimation de la performance',
                                  status = 'info',
                                  textOutput('rmse'),
                                  br(),
                                  plotlyOutput('scatter')
                              )
                       )
                   )
                   
            )
        )
),
tabItem(tabName='data',
        fluidRow(
            column(width = 12,
                   tabBox(width = 12,
                          id='tableaux',
                          tabPanel('Résumé',
                                   verbatimTextOutput('summary')
                          ),
                          tabPanel('Données brutes',
                                   dataTableOutput('tab')
                          )
                   )
            )
        )
        
),
tabItem(tabName='code',
        fluidRow(
            column(width=12,
                   tabBox(width = 12,
                          id='Codes',
                          tabPanel(title = 'ui.R',
                                   fluidRow(
                                       column(width = 12,
                                              verbatimTextOutput('ui')
                                       )
                                   )
                          ),
                          
                          tabPanel(title='server.R',
                                   fluidRow(
                                       column(width = 12,
                                              verbatimTextOutput('serveur')
                                       )
                                   )
                          ),
                          tabPanel(title='Global.R',
                                   fluidRow(
                                       column(width = 12,
                                              verbatimTextOutput('glbl')
                                       )
                                   )
                          )
                   )
            )
        )
),
tabItem(tabName='about',
        fluidRow(
            column(width=7,
                   h3('Cadre du Projet'),
                   p('Cette application ',tags$b('Rshiny'),' s'inscrit dans un projet de création Rshiny pour le cours ',tags$b('Analyse de données massives sous R, supervisé par Benoit Thieurmel.'),
                     br(),
                     'Pour ce projet nous avons utilisé le jeu de données ',tags$b('cars.csv'),' téléchargé sur le site ',tags$b('Kaggle,'),'accessible en cliquant',a('ici.',href='https://www.kaggle.com/debajyotipodder/co2-emission-by-vehicles'),
                     style = 'background-color:#e1d2b8;padding:15px;border-radius:10px;text-align:justify;font-size:18px'
                   ),
                   h3('A propos des auteurs'),
                   p('Nous sommes des étudiants actuellement en master',tags$b('sciences des données'),'au sein de l'institut agro basé à Rennes.',
                     'Ce projet est disponible sur',tags$b('github'),'sur lequel sera aussi disponible nos autres projets.',
                     style = 'background-color:#e1d2b8;padding:15px;border-radius:10px;text-align:justify;font-size:18px'),
                   br(),
                   h3('Liste des packages utilisés'),
                   br(),
                   p('-shiny',
                     br(),
                     '-shinyWidgets',
                     br(),
                     '-nnet',
                     br(),
                     '-leaps',
                     br(),
                     '-pls',
                     br(),
                     '-groupdata2',
                     br(),
                     '-boot',
                     br(),
                     '-FactoMineR',
                     br(),
                     '-lme4',
                     br(),
                     '-shiny',
                     br(),
                     '-shinydashboard',
                     br(),
                     '-ggplot2',
                     br(),
                     '-forcats',
                     br(),
                     '-plotly'
                   ),
                   br(),
                   tags$b('Contact'),
                   br(),
                   a(icon('linkedin'),'@florence-ghestem-data-sciences/',href='https://www.linkedin.com/in/florence-ghestem-data-sciences/'),
                   br(),
                   a(icon('linkedin'),'@elias-hermance-datascience/',href='https://www.linkedin.com/in/elias-hermance-datascience/'),
                   br(),
                   a(icon('github'),'Easill',href='https://github.com/Easill'),
                   br(),
                   a(icon('github'),'floghes',href='https://github.com/floghes')
            ),
            column(width=5,
                   a(tags$img(src='images.jpg',width='485px',height='104px'),href='https://www.agrocampus-ouest.fr/'), # ajout de l'image
                   br(),
                   br(),
                   a(tags$img(src='Kaggle_logo.png',width='485px',height='154px'),href='https://www.kaggle.com/') # ajout de l'image
            )
        )
)
)
)
)
)

"
    })


output$glbl <- renderText({"
# Chargement du jeu de donnees
cars <- read.table('https://raw.githubusercontent.com/Easill/Rshiny_cars/main/cars.csv',  sep = ';', dec = '.', header=TRUE, stringsAsFactors = TRUE)

# Renomme les colonnes
names(cars) <- c('Marque','Model','Classe','Moteur','Cylindre','Transmission','Fuel_Type','City_L_au_100','Hwy_L_au_100',
                 'Comb_L_au_100','Comb_mpg','CO2')

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
cars <- subset(cars, Fuel_Type != 'N')
cars$Fuel_Type<-factor(cars$Fuel_Type,exclude=NULL)


newdata <- cars[,c(12,4:5,8:11)] #On met cars en premier dans le dataset

mod0 <- glm(CO2 ~ 1,data = newdata) # Mod?le null

select = summary(regsubsets(CO2~.,data=newdata,nvmax=6))

rss = select$rss
bic = select$bic                                   # BIC
aic = bic - (log(nrow(newdata))-2)*(c(2:7))            # AIC


# Boxplots
# noms selection des variables
var_quali<-c('marque' = names(cars[1]),
             'modèle' = names(cars[2]),
             'classe' = names(cars[3]),
             'transmission' = names(cars[6]),
             'type d'essence' = names(cars[7]))

# noms selection des variables 
var_quanti<-c('taille du moteur (en L)' = names(cars[4]),
              'nombre de cylindres dans le moteur' = names(cars[5]),
              'consommation de carburant en ville (L/100 km)' = names(cars[8]),
              'consommation de carburant sur autoroute (L/100 km)' = names(cars[9]),
              'consommation de carburant combinée (L/100 km)' = names(cars[10]),
              'consommation de carburant combinée (miles per gallon)' = names(cars[11]))


# Matrice correlation

# Matrice de correlation des variables quantit
cormat<-cor(newdata) # matrice de correlation

meltcormat <- reshape2::melt(cormat)
colnames(meltcormat) <- c('x', 'y', 'value')
    "})

})



