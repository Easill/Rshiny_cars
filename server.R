# Serveur




# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # premier onglet
    
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
    
    observe({
        input$lancer
    })
    
})
