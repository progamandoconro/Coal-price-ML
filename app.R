library(dplyr)#minería y exploración
library(ggplot2)#gráficos
library(plotly)#gráficos 3D
library(shinydashboard)#app
library(shiny)#app
library(shinyWidgets)#app
library(Rtsne)#Reducción de la dimensionialidad con algoritmo ML

d <- read.csv('/termoelectric/data_unsupervised.csv')


d$Lider.Generacion_num <- ifelse(d$Lider.Generacion=="ARC",1,ifelse(d$Lider.Generacion=="GAP",2,3))
d$Interventoria_num <- ifelse(d$Interventoria=="COLSERING",1,2)
d$Valor=as.numeric(ifelse(d$DLI_VALOR_TOTAL.x<1000000,1000000, d$DLI_VALOR_TOTAL.x))

d_num <- d[,c(-13,-18:-19)]


 ui <- 

  fluidPage(theme = "bootstrap.min.css",
      h1(tags$b("Algoritmos de Aprendizaje no Supervizado t-SNE y PCA")),
    tabsetPanel(
      
      tabPanel("Algortimos t-SNE y k-means",
               div(style="display:inline-block",  selectInput('col', "Variable para el color", colnames(d_num),selected = "Valor")),
               div(style="display:inline-block",selectInput('cex', "Variable para el tamaño", colnames(d_num),selected = "ONI")),
               
               
               
  plotOutput("plot3",height = '550px'),
  h5("Agrupación resultante por el algorimo k-means"),
  div(style="display:inline-block",numericInput("cluster","Número de clusters (k)",value = 10)),
  
  textOutput("plot1"),
  textOutput("plot2")
  
  ),

  
  tabPanel("Análisis de Componentes Principales 3D",
           div(style="display:inline-block", selectInput('col2', "Variable para el color", colnames(d_num[,c(1:16,21)]),selected = "ONI")),
           div(style="display:inline-block",selectInput('cex2', "Variable para el tamaño", colnames(d_num[,c(1:16,21)]),selected = "DLI_PESO_A_PAGAR.x")),
           plotlyOutput("plot4",height = '550px'),
           div(style="display:inline-block",sliderInput("size","Tamaño del punto",0.1,1,value = 0.3)),
           h5("Resultado de los tres primeros componentes o dimensiones del Algoritmo 'PCA'. Además, se muestran dos dimensiones más, expresadas como color y tamaño de los puntos")
           
  
  ),
  
  tabPanel("Referencias y notas",
           br(),
           br(),
           br(),
         h4("PCA: Análisis de Componentes Principales para cuyo computo se utilizaron los valores 'eigen' de la matriz de correlación de los datos. Se logró reducir la dimensionalidad de la varianza total de la data en un 80%"),
         br(),
         h4("tsne: Se construyó para obtener una representación en dos dimensiones de los datos utilizando distancias obtenidas con PCA. Esta técnica es superior para la reducción de la dimensionalidad."),
         br(),
         h4("Forgy, E. W. (1965). Cluster analysis of multivariate data: efficiency vs interpretability of classifications. Biometrics, 21, 768–769. "),
         br(),
         h4("Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: A K-means clustering algorithm. Applied Statistics, 28, 100–108. doi: 10.2307/2346830. "),
         br(),
         h4("Lloyd, S. P. (1957, 1982). Least squares quantization in PCM. Technical Note, Bell Laboratories. Published in 1982 in IEEE Transactions on Information Theory, 28, 128–137."),
         br(),
         h4("MacQueen, J. (1967). Some methods for classification and analysis of multivariate observations. In Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability, eds L. M. Le Cam & J. Neyman, 1, pp. 281–297. Berkeley, CA: University of California Press."),
         br(),
         h4("Maaten, L. Van Der, 2014. Accelerating t-SNE using Tree-Based Algorithms. Journal of Machine Learning Research, 15, p.3221-3245."),
         br(),
         h4("van der Maaten, L.J.P. & Hinton, G.E., 2008. Visualizing High-Dimensional Data Using t-SNE. Journal of Machine Learning Research, 9, pp.2579-2605."),
         br(),
         h4("Mardia, K. V., J. T. Kent and J. M. Bibby (1979). Multivariate Analysis, London: Academic Press.Venables")
  )
  
  
    ),
  
  
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
      
      d_tsne_1 <- princomp(d_num[,c(1:16,21)], cor = T)
      
     
     
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

