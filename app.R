library(dplyr)#minería
library(ggplot2)#gráficos
library(plotly)#gráficos 3D
library(shinydashboard)#app
library(shiny)#app
library(shinyWidgets)#app
library(Rtsne)#Reducción de la dimensionialidad

d <- read.csv('data_unsupervised.csv')

d$Lider.Generacion_num <- ifelse(d$Lider.Generacion=="ARC",1,ifelse(d$Lider.Generacion=="GAP",2,3))
d$Interventoria_num <- ifelse(d$Interventoria=="COLSERING",1,2)
d$Valor=as.numeric(ifelse(d$DLI_VALOR_TOTAL.x<1000000,1000000, d$DLI_VALOR_TOTAL.x))
d_num <- d[,c(-12,-17:-18)]

ui <- 
  fluidPage(
    setBackgroundColor(color = "WhiteSmoke  ", gradient = c("linear",
                                                          "radial"), direction = c("bottom", "top", "right", "left")),
    h1(tags$b("Algoritmos de Aprendizaje no Supervizado t-SNE y PCA")),
    h4("Por favor espere mientras se realiza el computo en tiempo real"),
    
    tabsetPanel(
      
      tabPanel("t-SNE",
               selectInput('col', "Color", colnames(d_num),selected = "Valor"),
               selectInput('cex', "Tamaño", colnames(d_num),selected = "ONI"),
               
               
  plotOutput("plot3",height = '600px'),
  h5("Agrupación resultante por el algorimo k-means"),
  numericInput("cluster","Número de clusters (k)",value = 10),
  textOutput("plot1"),
  textOutput("plot2")),
  
  tabPanel("PCA",
           selectInput('col2', "Color", colnames(d_num[,c(1:15,20)]),selected = "ONI"),
           sliderInput("size","Tamaño",0.1,1,value = 0.3),
           plotlyOutput("plot4",height = '800px'),
           selectInput('cex2', "Tamaño", colnames(d_num[,c(1:15,20)]),selected = "DLI_PESO_A_PAGAR.x")
  
  )
    ),
  
 
  h5("Resultado de los tres primeros componentes o dimensiones del Algoritmo 'PCA'. Además, se muestran dos dimensiones más, expresadas como color y tamaño de los puntos"),
  
  tags$head(tags$style("#plot1{color: green;
                                 font-size: 20px;
                                 font-style: bold;  
                                 }"
  )
  ),
  
  tags$head(tags$style("#plot2{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
  )
  )
  
  )
    
server <- function(input, output) {

  output$plot1<-    renderText({
    
    set.seed(7)
    
    k <- kmeans(d_num,input$cluster)

    
paste("Datos agrupados:",round( (k$betweenss/k$totss)*100,2), "%")
   
   })
  
  output$plot2<-    renderText({
    
    set.seed(7)
    
    k <- kmeans(d_num,input$cluster)
    
    paste("Datos no agrupados:", round( 100-((k$betweenss/k$totss)*100),2), "%")
    
  })
  
  output$plot3<-    renderPlot({
    

    set.seed(7)
    
    d_tsne_1 <- Rtsne(d_num, pca = T, pca_center = T,pca_scale = T)
    
    set.seed(7)
    km <- kmeans(d_tsne_1$Y,input$cluster)
    
    col <- d_num[,input$col]
    cex <- d_num[,input$cex]
    Año <- as.factor(d$Año)
    Cluster<- as.factor(km$cluster)
   
    g<- ggplot(as.data.frame(d_tsne_1$Y), aes(x=d_tsne_1$Y[,1], y=d_tsne_1$Y[,2])) 
  
    g+ geom_point(aes(col=as.numeric(col),cex=cex,pch=Cluster)) +
     guides(colour=guide_legend(override.aes=list(size=6))) +
     xlab("") + ylab("") +
     ggtitle("t-SNE") +
     theme(axis.text.x=element_blank(),
           axis.text.y=element_blank()) +
    labs(col=input$col,Año='Año',cex=input$cex)+ 
     scale_color_gradient(low = "red", high = "green")+
     scale_shape_manual(values=1:11)
   
   })
  
  output$plot4<-    renderPlotly({
   
    
      set.seed(7)
      
      d_tsne_1 <- princomp(d_num[,c(1:15,20)], cor = T)
   
      set.seed(7)
      
      p <- plot_ly(as.data.frame(d_tsne_1$scores), x = ~d_tsne_1$scores[,1], 
                   y = ~d_tsne_1$scores[,2], z = ~d_tsne_1$scores[,3],
                     marker = list(line=list(color=0,width=0),
                      color = ~d_num[,input$col2], colorscale = c('#FFE1A1', '#683531'), 
                      showscale = TRUE, 
                      size=d_num[,input$cex2]*input$size)) %>%
                      add_markers() %>%
                      layout(scene = list(xaxis = list(title = 'Comp. 1 (46 %)'),
                            yaxis = list(title = 'Comp. 2 (21 %)'),
                            zaxis = list(title = 'Comp. 3 (13 %)')))%>%
                      layout(autosize = T,margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
      p
        
  })
 
}

shinyApp(ui, server)
