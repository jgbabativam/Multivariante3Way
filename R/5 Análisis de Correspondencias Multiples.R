######========================================================
######   Script por Giovany Babativa-Márquez, PhD. 
######========================================================
#==  Use el conjunto de datos sobre hobbies incluido en el paquete 
#==  FactoMineR y realice un análisis multivariante para los primeros 
#==  8 hobbies. Use las variables de estado civil, profesión y cantidad 
#==  de hobbies como suplementarias.
#==
#==  Utilice los paquetes
#==      1. FactoMineR
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

#---- Inicio, cargue del conjunto de datos.

data(hobbies)
?hobbies

glimpse(hobbies)
names(hobbies)

datosMCA <- hobbies[1:1000, c(1:8,21:23)]
resmca <- MCA(datosMCA,
              quali.sup = 9:10, 
              quanti.sup = 11, ind.sup = 1:100)

corrplot(resmca$var$coord)
corrplot(resmca$quali.sup$coord)

explor(resmca)

#### Ejercicio: Resolver usando Factoshiny.
