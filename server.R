# Serveur

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # premier onglet
    
    # graph explicatifs boxplot pour les variables quali
    output$moustache <- renderPlotly({
        maxtauxCO2<-cars[cars$CO2>150,] 
        p<-ggplot(maxtauxCO2, aes(x= fct_rev(fct_reorder(maxtauxCO2[,input$VarBox],CO2,.fun="mean")), y=CO2)) +
            geom_boxplot(fill="slateblue", alpha=0.2)+
            ggtitle(paste0("Boxplot des modalités de la variable ", req(input$VarBox)," les plus émettrices de CO2")) +
            xlab(req(input$VarBox))+
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
            geom_point(colour="slateblue", alpha=0.2)+
            geom_smooth(method=lm,
                        se=FALSE,
                        linetype="dashed",
                        color="black") +
            ggtitle(paste0("Emmissions de CO2 en fonction de la variable ", req(input$VarScat))) +
            xlab(req(input$VarScat))+
            theme(plot.title = element_text(hjust = 0.5))
        # plot(cars$CO2~cars[,input$VarScat],
        #      main = c("Nuage de points CO2 en fonction de", input$VarScat),
        #      xlab = input$VarScat, ylab= "CO2")
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
        barplot(coef(glm(CO2~.,data=newdata[,select$which[input$Nbvar,bic]])),
                main = "Poids des variables dans le modèle",
                xlab = "Variables",
                ylab = "Scores",
                cex.names= 0.8)
    })
    
    Bestmod <- reactive({
        glm(CO2~.,data=newdata[,select$which[input$Nbvar,bic]])
        })
    
    output$suM <- renderPrint(summary(Bestmod()))
    
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
    
})

