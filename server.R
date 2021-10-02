# Serveur




# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # premier onglet
    
    output$scat <- renderPlot({plot(cars$CO2~cars[,input$VarScat],
                                    main = c("Nuage de points CO2 en fonction de", input$VarScat),
                                    xlab = input$VarScat, ylab= "CO2")})
    
    res_pca <- reactive({
        PCA(cars[,input$ACPCheck], quali.sup = input$qualiCheck, graph = FALSE)
    })
    
    output$graph_pca_ind <- renderPlot({
        plot.PCA(res_pca(), choix = "ind", axes = c(1,2))
    })
    output$graph_pca_var <- renderPlot({
        plot.PCA(res_pca(), choix = "var", axes = c(1,2))
    })
    
    
    output$moustache <- renderPlot({ggplot(cars, aes(x=cars[,input$VarBox], y=CO2)) +
            geom_boxplot()
        #boxplot(CO2 ~ cars[,input$VarBox], data = cars,xlab = input$VarBox, ylab ="CO2", main = c("Boxplot of CO2 by",input$VarBox))
    })
    
    # Deuxième onget
    
    output$coef <- renderPlot({
        barplot(coef(glm(CO2~.,data=newdata[,select$which[input$Nbvar,bic]])))
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

