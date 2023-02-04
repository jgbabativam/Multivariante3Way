######========================================================
######   Script por Giovany Babativa-Márquez, PhD. 
######========================================================
#==  El conjunto de datos de marcas y atributos de los carros contiene 
#==  la percepción de 1000 personas mayores de 25 años propietarias de 
#==  vehículos. Realice un análisis multivariante que permita identificar 
#==  el posicionamiento de las marcas.
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

#----- Ejemplo marcas de carros.
#------ CA Simple

datos2 <- read.spss("data/CarrosREES.sav", to.data.frame = T)

glimpse(datos2)

datos <- datos2 |> 
         select(MARCAR, Atributo, Frecuencia) |> 
         pivot_wider(names_from = Atributo, values_from = Frecuencia) |> 
         column_to_rownames(var = "MARCAR")

dev.off()
res <- CA(datos, graph = F)

res$eig
corrplot(res$row$cos2)
corrplot(res$row$coord, is.corr = FALSE)

corrplot(res$col$cos2)
corrplot(res$col$coord, is.corr = FALSE)


out <- HCPC(res, nb.clust = 3)

out$desc.var


sout <- Factoshiny(datos)

library(explor)
explor(res)
