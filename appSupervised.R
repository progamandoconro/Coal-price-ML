# Shiny App with Supervised ML for Coal Price Prediction:

library(shiny)
library(shinydashboard)
library(randomForest)
library(caret)
library(zoo)
library(dplyr)
library(groupdata2)
library(ggplot2)
library(DT)
library(e1071)

normalize <- function(x) { 
    return((x - min(x)) / (max(x) - min(x)))
}

df=read.csv('carbon_3.csv')

df$Toneladas_diarias<- df[,11]
df$Valor_diario_a_pagar<- df[,13]

df<-df[,c(-11,-13)]

df <- select (df,-starts_with("date"))%>%
    select(-starts_with("date."))%>%
    na.aggregate(by='Anio')%>%
    na.aggregate(by='Mes')%>%
    na.aggregate(by='Dia')%>%
    na.aggregate()

df$cat= paste(df[,1], df[,2])
df$cat=as.factor(df$cat)
levels(df$cat) <- 1:118
df$cat <- as.numeric(df$cat)

df <- balance(df,size='max', cat_col='cat')%>%
    lapply(normalize)%>%
    as.data.frame()

ui <- dashboardPage(
    dashboardHeader(title="Predicción Carbón"),
    dashboardSidebar(
        selectInput("target","Variable a predecir",c("Toneladas_diarias","Valor_diario_a_pagar")),
        h5("Parámetros de afinación del Algoritmo"),
        numericInput("p1","ntree",534),
        numericInput("p2","mtry",sqrt(200)),
        h5("*El valor de mtry es llevado a su raíz cuadrada"),
        numericInput("p3","seed",777),
        h5("Predicciones"),
        sliderInput("p4","Ajuste de probabilidad de clases",min=0.1,max=0.9,value=0.499),
        sliderInput("p5","Número de meses a futuro",min=1,max=12,value=1)
        
    ),
    dashboardBody(tabItem('item',tabsetPanel(tabPanel('Validación',
                 h5('Predicciones para las subidas y bajadas en el peso del Carbón ingresado y del valor facturado'),
                 h5("Matríz de Validación (Verde = éxito en la predicción, rojo = falla en la predicción)"),
                 plotOutput('plot'),
                 h5('VN = Verdaderos Negativos, FN = Falsos Negativos, FP = Falsos Positivos, VP = Verdaderos Positivos')
    ),
    tabPanel('Evaluación',
             h5('Resultados de las predicciones para los últimos 10 meses registrados (desconocidos para el algoritmo)'),
             DT::dataTableOutput("table")  
    ),
    
    tabPanel('Predicciones',
             h5("El algoritmo Random Forest, con los parámeros afinados, arroja que: "),
             textOutput("results"),
             plotOutput('plot2')
             )
    
    ))))

server <- function(input, output) {
    output$plot <- renderPlot({
        
        m=input$p5
        n=28*m
        var_expl<- df
        target<-df[,input$target]
        df_cruz <- data.frame(target,var_expl)
        target<- vector()
        
        for (i in 1:NROW(df_cruz[,1])) {
            
            target[i]<-ifelse( df_cruz[i,1]<df_cruz[i+n,1],1,0 )
            
        }
        
        df_cruz$target<- target
        df_cruz<-df_cruz[1:(nrow(df_cruz)-n),]
        df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
        
        set.seed(input$p3)
        index <- sample(1:nrow(df_cruz),nrow(df_cruz))
        train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]
        val <- df_cruz[(floor(nrow(df_cruz)*0.7)+1):nrow(df_cruz),]
       
        set.seed(input$p3)
        rF <- randomForest (train$target~.,ntree=input$p1, 
                            mtry=sqrt(input$p2) ,data=train[,-1], 
                            scale=T, importance=T,replace=T)
        
        p2 <- predict(rF, val[,-1])
        p2<- ifelse(p2<input$p4,0,1)
        cM <- confusionMatrix(as.factor(p2),as.factor(val[,1]))
        
        l<- as.data.frame(cM[2])
        
        l<-l[3]
        l<-as.vector(l)
        l<-c(l)
        
        ctable <- as.table(matrix(as.vector(unlist(l)), nrow = 2, byrow = TRUE))
        fourfoldplot(ctable, color = c( "#CC6666","#99CC99"),
                     conf.level = 0, margin = 2,main="") + 
            text(-0.4,0.4, "VN", cex=1) + 
            text(0.4, -0.4, "VP", cex=1) + 
            text(0.4,0.4, "FN", cex=1) + 
            text(-0.4, -0.4, "FP", cex=1)
        
    })
    
    output$table = DT::renderDataTable({
        
        m=input$p5
        n=28*m
        var_expl<- df
        target<-df[,input$target]
        df_cruz <- data.frame(target,var_expl)
        
        target<- vector()
        
        for (i in 1:NROW(df_cruz[,1])) {
            
            target[i]<-ifelse( df_cruz[i,1]<df_cruz[i+n,1],1,0 )  
        }
        
        df_fut <-df_cruz[(nrow(df_cruz)-(n-1)):nrow(df_cruz),]
        df_cruz$target<- target
        df_cruz<-df_cruz[1:(nrow(df_cruz)-n),]
        
        test= df_cruz[(nrow(df_cruz)-220):(nrow(df_cruz)),]
        
        df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
        set.seed(input$p3)
        
        index <- sample(1:nrow(df_cruz),nrow(df_cruz))
        
        train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]
        
        set.seed(input$p3)
        rF <- randomForest (train$target~.,ntree=input$p1, 
                            mtry=sqrt(input$p2) ,data=train[,-1],
                            scale=T, importance=T,replace=T)
        
        p2 <- predict(rF, test[,-1])
        p2<- ifelse(p2<input$p4,0,1)
        
        cM=confusionMatrix(as.factor(p2),as.factor(test[,1]))
        as.data.frame(unlist(cM))
        
    })
    
    output$results = renderText({
        
        m=input$p5
        n=28*m
        var_expl<- df
        target<-df[,input$target]
        df_cruz <- data.frame(target,var_expl)
        
        target<- vector()
        for (i in 1:NROW(df_cruz[,1])) {
            
            target[i]<-ifelse( df_cruz[i,1]<df_cruz[i+n,1],1,0 )
        }
        
        df_fut <-df_cruz[(nrow(df_cruz)-n):nrow(df_cruz),]
        df_cruz$target<- target
        df_cruz<-df_cruz[1:(nrow(df_cruz)-n),]
        df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
        
        set.seed(input$p3)
        index <- sample(1:nrow(df_cruz),nrow(df_cruz))
        train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]
        
        set.seed(input$p3)
        rF <- randomForest (train$target~.,ntree=input$p1, 
                            mtry=sqrt(input$p2) ,data=train[,-1], 
                            scale=T, importance=T,replace=T)
        
        p2 <- predict(rF, df_fut[,-1])
        p2<- ifelse(p2<input$p4,0,1)
        
        paste("La probabilidad de aumento de", input$target,"dentro de", 
              input$p5, "mes es de:",   ( sum( as.numeric(as.vector(p2))) / nrow(df_fut) ),
              " +- ",( sd( as.numeric(as.vector(p2))) / nrow(df_fut) )) 
        
    })
    
    output$plot2 = renderPlot({
        
        m=input$p5
        n=28*m
        var_expl<- df
        target<-df[,input$target]
        df_cruz <- data.frame(target,var_expl)
        
        target<- vector()
        for (i in 1:NROW(df_cruz[,1])) {
            
        target[i]<-ifelse( df_cruz[i,1]<df_cruz[i+n,1],1,0 )
            
        }
        
        df_fut <-df_cruz[(nrow(df_cruz)-n):nrow(df_cruz),]
        df_cruz$target<- target
        df_cruz<-df_cruz[1:(nrow(df_cruz)-n),]
        df_cruz <- df_cruz[1:(nrow(df_cruz)-220),]
        
        set.seed(input$p3)
        index <- sample(1:nrow(df_cruz),nrow(df_cruz))
        train <- df_cruz[1:floor(nrow(df_cruz)*0.7),]
        
        set.seed(input$p3)
        rF <- randomForest (train$target~.,ntree=input$p1, mtry=sqrt(input$p2) ,
                            data=train[,-1], scale=T, importance=T,replace=T)
        
        p2 <- predict(rF, df_fut[,-1])
        p2<- ifelse(p2<input$p4,0,1)
        p <- as.vector(as.numeric(p2))
        g<-ggplot(data = as.data.frame(p),aes(x=1:NROW(p),y=p))
        
        g+geom_line()+geom_point()+xlab("Días de transacciones (28 días / mes)") + 
            ylab(paste("Valor escalado del",input$target))+
            geom_line(aes(col="Predicciones"))+geom_point()+
            geom_line(aes(y=df_fut[,input$target]))+geom_point(aes(y=df_fut[,input$target]))
        
    })
    
}

shinyApp(ui, server)
