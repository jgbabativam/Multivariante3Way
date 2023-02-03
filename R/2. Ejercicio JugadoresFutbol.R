######========================================================
######   Script por Giovany Babativa-Márquez, PhD. 
######========================================================
#==  Use el conjunto de datos sobre el rendimiento de los jugadores 
#==  profesionales de fútbol para realizar un análisis multivariante 
#==  y un análisis clúster. Utilice los paquetes
#==
#==  Utilice los paquetes
#==      1. FactoMineR y factoextra
#==      2. explor
#==      3. Factoshiny
#============================================================

rm(list = ls())


library(pacman)

p_load(readxl, tidyverse, MultBiplotR, BiplotML, janitor, here, skimr, foreign,
       factoextra, FactoMineR, Factoshiny, FactoInvestigate,
       explor, BiplotGUI,
       corrplot, patchwork,
       RSpectra)

#----- Ejemplo jugadores.

jugadores <- read_excel(here::here("data/jugadores.xlsx")) |> 
             clean_names()

glimpse(jugadores)

skim(jugadores)

datosJ <- jugadores |> 
          column_to_rownames(var = "nombre") |> 
          select(where(is.numeric), -id)

############################  Análisis de componentes principales

comprin <- PCA(datosJ , scale.unit=TRUE, ncp=5, graph = FALSE)

fviz_screeplot(comprin, ncp=10, title="Valores propios", ylab="Porcentaje de varianza", xlab="Eje")

comprin$eig

g3 <- fviz_pca_ind(comprin, col.ind="cos2", title="Proyección de los jugadores") +
      scale_color_gradient2(low="white", mid="blue", 
                            high="red", midpoint=0.50)
g3


g4 <- fviz_pca_var(comprin, col.var="contrib", axes=c(1,2), title="Proyección de las habilidades")+
      scale_color_gradient2(low="white", mid="blue", 
                             high="red", midpoint=2)+theme_bw()

g4

jugadores$posicion <- as.factor(jugadores$posicion)

dev.new()
fviz_pca_ind(comprin,  label="none", habillage=jugadores$posicion, title="Jugadores por posición")


fviz_pca_biplot(comprin)

View(comprin$ind$coord)
View(comprin$var$coord)

library(explor)
explor(comprin)

