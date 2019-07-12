library(dplyr)#minería
library(ggplot2)#gráficos
library(shinydashboard)#app
library(shiny)#app


d <- read.csv('data_unsupervised.csv')

d$Lider.Generacion_num <- ifelse(d$Lider.Generacion=="ARC",1,ifelse(d$Lider.Generacion=="GAP",2,3))
d$Interventoria_num <- ifelse(d$Interventoria=="COLSERING",1,2)

d_num <- d[,c(-1,-17:-18)]


ui <- dashboardPage(
  dashboardHeader(title = "K-means"),
  dashboardSidebar(numericInput("cluster","Número de clusters",value = 10)
                   
                   
                   
                   ),
  
  
  
  dashboardBody(
    
  textOutput("plot1"),
  textOutput("plot2"),
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
  
  
  
  
}

shinyApp(ui, server)
