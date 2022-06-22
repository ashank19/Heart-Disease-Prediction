#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  output$tabres <- renderText({
    paste("You've selected:", input$tabs)
  })  
  output$rawdata <- renderTable(heart_data)
  output$method1 <- renderPlot({
    fviz_nbclust(heart[,1:11], kmeans, method = "wss") +
      geom_vline(xintercept = 4, linetype = 2)+
      labs(subtitle = "Elbow method")
  })
  selectedData <- reactive({
    heart[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  output$method2 <- renderPlot({
    fviz_nbclust(heart[,1:11], kmeans, method = "silhouette") +
      labs(subtitle = "Silhouette Method")
  })
  output$method3 <- renderPlot({
    fviz_nbclust(heart[,1:11], hcut, method = "silhouette") +
      labs(subtitle = "Silhouette Method")
  })
  output$clusterchart <- renderPlot({
    fviz_cluster((eclust(heart[,1:11], "kmeans", k = input$clustnum, nstart = 25, graph = FALSE)), geom = "point", ellipse.type = "norm",
                 palette = "jco", ggtheme = theme_minimal())
    
  })
  output$stat1 <- renderPrint({
    cluster.stats(dist(heart[,1:11]),(eclust(heart[,1:11], "kmeans", k = input$clustnum, nstart = 25, graph = FALSE))$cluster)
  })
  output$stat2 <- renderPrint({
    (cluster.stats(dist(heart[,1:11]),(eclust(heart[,1:11], "kmeans", k = input$clustnum, nstart = 25, graph = FALSE))$cluster))$dunn
  })
  output$method5 <- renderPlot({
    plot(dd,xlab='Patients',
         ylab='Euclidean distance')
  })

  output$method7 <- renderPlot({
    plot(hc2, cex = 0.6, hang = -1)
  })
  output$method8 <- renderPlot({
    pltree(hc5, cex = 0.6, hang = -1, main = "Dendrogram of DIANA")
    rect.hclust(hc5, k=2, border='red')
  })
  output$clustdata <- renderTable(kmeans_basic_df)
  
})