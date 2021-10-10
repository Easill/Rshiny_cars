# Serveur

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # premier onglet
    ####----
    # graph explicatifs boxplot pour les variables quali
    output$moustache <- renderPlotly({
        maxtauxCO2<-cars[cars$CO2>150,] 
        p<-ggplot(maxtauxCO2, aes(x= fct_rev(fct_reorder(maxtauxCO2[,input$VarBox],CO2,.fun="mean")), y=CO2)) +
            geom_boxplot(fill="slateblue", alpha=0.2)+
            ggtitle(paste0("Boxplot des modalités de la variable ", names(var_quali[var_quali==input$VarBox])," les plus émettrices de CO2")) +
            xlab(names(var_quali[var_quali==input$VarBox]))+
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
            # geom_smooth(method=lm,
            #             se=FALSE,
            #             linetype="dashed",
            #             color="red") +
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
    output$coef <- renderPlot({
        barplot(coef(glm(CO2~.,data=newdata[,select$which[input$Nbvar,]])),
                main = "Poids des variables dans le modèle",
                xlab = "Variables",
                ylab = "Scores",
                cex.names= 0.8)
    })
    
    Bestmod <- reactive({
        glm(CO2~.,data=newdata[,select$which[input$Nbvar,]])
    })
    cvmod <- reactive({
        cv.glm(newdata,Bestmod(),K=10)
    })
    output$rmse <- renderText({
        c("RMSE Ajusté :",round(cvmod()$delta[2],1))
    })
    
    output$title <- renderText({
        "Coefficients du modèle selectionné :"
    })
    output$suM <- renderPrint({
        Bestmod()$coef
    }) #Coefficients du modèles affichés
    
    fitted.co2_2 <- reactive({
        fitted(Bestmod())
    }) # Fitted LMP values
    
    observed.co2_2 <- reactive({
        newdata$CO2
    }) # Observed LMP values
    
    dataplot <- reactive({
        data.frame(observed.co2_2(),fitted.co2_2())
    })
    
    output$scatter <- renderPlotly({
        p3 <- ggplot(dataplot(),aes(x=observed.co2_2(),y=fitted.co2_2())) +
            geom_point(colour="black",alpha=0.5)+
            ggtitle(HTML(paste0("Fitted versus observed CO2 values avec ",input$Nbvar," variables")))+
            xlab("Observed CO2") + ylab("Fitted CO2")+
            theme(plot.title=element_text((hjust=0.5)))+
            geom_abline(slope = 1,intercept = 0,col="red")
        ggplotly(p3)
    })
    
    
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
    
    # Onglet data 
    
    output$tab<-renderDataTable({
        cars
    })
    output$summary<-renderPrint({
        summary(cars)
    })
    #Onglet code
    
    #####################################################################----
    
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
                    # geom_smooth(method=lm,
                    #             se=FALSE,
                    #             linetype='dashed',
                    #             color='red') +
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
            output$coef <- renderPlot({
                barplot(coef(glm(CO2~.,data=newdata[,select$which[input$Nbvar,]])),
                        main = 'Poids des variables dans le modèle',
                        xlab = 'Variables',
                        ylab = 'Scores',
                        cex.names= 0.8)
            })
            
            Bestmod <- reactive({
                glm(CO2~.,data=newdata[,select$which[input$Nbvar,]])
            })
            cvmod <- reactive({
                cv.glm(newdata,Bestmod(),K=10)
            })
            output$rmse <- renderText({
                c('RMSE Ajusté :',round(cvmod()$delta[2],2))
            })
            
            output$title <- renderText({
                'Coefficients du modèle selectionné :'
            })
            output$suM <- renderPrint({
                Bestmod()$coef
            }) #Coefficients du modèles affichés
            
            fitted.co2_2 <- reactive({
                fitted(Bestmod())
            }) # Fitted LMP values
            
            observed.co2_2 <- reactive({
                newdata$CO2
            }) # Observed LMP values
            
            dataplot <- reactive({
                data.frame(observed.co2_2(),fitted.co2_2())
            })
            
            output$scatter <- renderPlotly({
                p3 <- ggplot(dataplot(),aes(x=observed.co2_2(),y=fitted.co2_2())) +
                    geom_point(colour='black',alpha=0.5)+
                    ggtitle(HTML(paste0('Fitted versus observed CO2 values avec ',input$Nbvar,' variables')))+
                    xlab('Observed CO2') + ylab('Fitted CO2')+
                    theme(plot.title=element_text((hjust=0.5)))+
                    geom_abline(slope = 1,intercept = 0,col='red')
                ggplotly(p3)
            })
            
            
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
            
            # Onglet data 
            
            output$tab<-renderDataTable({
                cars
            })
            output$summary<-renderPrint({
                summary(cars)
            })
            #Onglet code
            
            output$ui <- renderText({
                
            })
            
        })"
    })
    
    ###############################################################----
    
    output$ui <- renderText({"
        # # Define UI for application that draws a histogram
        # User interface
        shinyUI(
            dashboardPage(
                skin = 'red',
                dashboardHeader(title = HTML(paste0('Cars CO',tags$sub('2'),' emissions')),
                                titleWidth = 250
                ),
                #Sidebar content
                dashboardSidebar(
                    sidebarMenu(
                        menuItem('Home', tabName = 'home', icon = icon('home', lib = 'font-awesome')),
                        menuItem('Visual Analysis', tabName = 'pca', icon = icon('chart-pie')),
                        menuItem('Model Selection', tabName = 'model', icon = icon('wave-square')),
                        menuItem('Datas',tabName = 'data', icon=icon('th')),
                        menuItem('Source Code for app', tabName = 'code', icon=icon('file-code'))
                    )
                ),
                dashboardBody(
                    tabItems(
                        #First tab content 
                        tabItem(tabName = 'home',
                                fluidRow(
                                    column(width = 7,
                                           br(),
                                           p('
                                   Ce jeu de données montre comment les émissions de CO2 d'un véhicule peuvent varier 
                                   en fonction de ses différentes variables Les données ont été prises et compilées à 
                                   partir du site officiel',
                                             a('gouvernement Canadien', href='https://open.canada.ca/data/en/dataset/98f1a129-f628-4ce4-b24d-6f16bf24dd64#wb-auto-6'),
                                             'Le jeu de données contient des données sur une période de 7 ans.
                                   La variable à expliquer est alors l'émission de CO2 d'un véhicule, une variable quantitative. 
                                   Il y a 11 variables explicatives qui sont à la fois qualitatives (la marque, le modèle, 
                                   la classe, le nombre de cylindres, la transmission, le type de carburant) et quantitatives 
                                   (la taille du réservoir, la consommation de carburant en ville, la consommation de carburant 
                                   sur l'autoroute, la consommation de carburant combinée).',
                                             style = 'background-color:LightBlue;padding:15px;border-radius:10px')
                                           
                                    ),
                                    column(width=5,
                                           tags$img(src='pollution.jpg',width='459px',height='287px')
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
                                                                                          status = 'warning',
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
                                                                                          status = 'warning',
                                                                                          animation = 'jelly'
                                                                      )
                                                                      ),
                                                                      align = 'text-align: justify'
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
                                                          plotOutput('coef'),
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
                                           fluidRow( #↑ fluidrow 2 (summary du modele + estimation de la perf)
                                               column(width = 6, # colonne summary
                                                      box(width = 12,
                                                          title = 'Summary',
                                                          status = 'info',
                                                          textOutput('title'),
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
                                                               )
                                                           )
                                                  ),
                                                  
                                                  tabPanel(title='server.R',
                                                           fluidRow(
                                                               column(width = 12,
                                                               )
                                                           )
                                                  ),
                                                  tabPanel(title='Global.R',
                                                           fluidRow(
                                                               column(width = 12,
                                                               )
                                                           )
                                                  )
                                           )
                                    )
                                )
                        )
                    )
                )
            )
        )"
    })
    
    ###############################################################----
    
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
        
        get_scat <- function(sel_var=1){
            selected <- select$which[sel_var,]
            mod <- glm(CO2~.,data=newdata[,selected])
            fitted.co2_2 = fitted(mod) # Fitted LMP values
            observed.co2_2 = newdata$CO2   # Observed LMP values
            R2 <- cor(observed.co2_2,fitted.co2_2)^2
            plot(observed.co2_2,fitted.co2_2,type='p',pch=16,bty='n',xlab='Observed CO2',
                 ylab='Fitted CO2',main='Fitted versus observed CO2 values',
                 cex.lab=1.25,cex.axis=1.25,cex.main=1.25)
            abline(0,1,lwd = 3, col = 'red')
            text(175,450,paste('R2=',round(R2,3)),cex=1.25)
        }
        
        
        # Boxplots
        # noms selection des variables
        var_quali<-c('marque' = names(cars[1]),
                     'modèle' = names(cars[2]),
                     'classe' = names(cars[3]),
                     'transmission' = names(cars[6]),
                     'type d'essence' = names(cars[7]))
        
        # Scatterplot
        # noms selection des variables 
        var_quanti<-c('taille du moteur (en L)' = names(cars[4]),
                      'nombre de cylindres dans le moteur' = names(cars[5]),
                      'consommation de carburant en ville (L/100 km)' = names(cars[8]),
                      'consommation de carburant sur autoroute <br> (L/100 km)' = names(cars[9]),
                      'consommation de carburant combinée <br> (55 % en ville, 45 % sur route) (L/100 km)' = names(cars[10]),
                      'consommation de carburant combinée <br> (55 % en ville, 45 % sur route) (miles per gallon)' = names(cars[11]))
        
        
        # Matrice correlation
        
        # Matrice de correlation des variables quantit
        cormat<-cor(newdata) # matrice de correlation
        
        meltcormat <- reshape2::melt(cormat)
        colnames(meltcormat) <- c('x', 'y', 'value')"
        
    })
    
})



