
# Cliente: Oscar Gonzales Valencia 
# Fin: Tesis de maestría. Industria privada. Planta termoeléctrica Termopaipa
# Actividad: Carbón a energía eléctrica. 
# Negocio:  Proveedores de carbón ----> termoeléctrica -----> energía eléctrica al cliente. Relación de gananacia en el proceso. 
# Objetivo: Predecir la venta a mediano plazo para afinar el funcionamiento de la planta y mejorar las ventas. 
# Entrada y salida del material en
#22-06-2019
# Análisis por: Rodrigo Diaz-Lupanow

  # Análisis general
  
  df = read.csv("FD-04_minero_21062019.csv")
  summary(df)
  str (df)
  any(is.na(df))
  View(df)
  any(is.na(df[,-6]))

  #Gráficas de variables en el tiempo utilizando ggplot2 (install.packages("ggplot2"))
 
library(ggplot2)
 
g <- ggplot (data = df,aes(x=1:nrow(df),y=df$DLI_VALOR_TOTAL))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("Valor Total ($ COL)")

g <- ggplot (data = df,aes(x=1:nrow(df),y=df$DLI_PRECIO_BASE))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("Precio base ($ COL)")

g <- ggplot (data = df,aes(x=1:nrow(df),y=df$DLI_KILOCALORIAS))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("DLI_Kilocalorías(kcal)")

g <- ggplot (data = df,aes(x=1:nrow(df),y=df$DLI_REGALIAS))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("Regalías ($ COL)")

g <- ggplot (data = df,aes(x=1:nrow(df),y=df$ENT_PESO_NETO))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("Peso neto (kg)")

g <- ggplot (data = df,aes(x=1:nrow(df),y=df$DLI_PESO_A_PAGAR))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("Peso a pagar (kg)")

g <- ggplot (data = df,aes(x=1:nrow(df),y=df$DLI_HUMEDAD))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("Humedad (%)")

#quitar los valores de cenizas mayores a 100

cenizas <- replace( df$DLI_CENIZAS,df$DLI_CENIZAS > 100, 100)
df_g <- data.frame(x=1:NROW(cenizas),cenizas=cenizas)

g <- ggplot (data =df_g ,aes(df_g[,1],df_g[,2]))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("Cenizas (%)")

cenizas_humedad <- replace( df$DLI_CENIZASMASHUMEDAD,df$DLI_CENIZASMASHUMEDAD > 100, 100)
df_g <- data.frame(x=1:NROW(cenizas_humedad),cenizas=cenizas_humedad)

g <- ggplot (data =df_g ,aes(df_g[,1],df_g[,2]))
g + geom_point(cex=0.005) + geom_smooth()+ xlab ( "Registros desde 2008 al 2019") + ylab ("Cenizas y humedad (%)")
