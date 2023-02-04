######========================================================
######   Script por Giovany Babativa-Márquez, PhD. 
######========================================================
#--- El paquete MultBiplotR contiene el conjunto de datos “spiders” el cual 
#--- contiene 28 sitios de muestreo donde se identifica la presencia o ausencia 
#--- de algunas especies de arañas. Realice un biplot logístico para identificar 
#--- las especies que son más probables en los mismos sitios y concluya sobre las 
#--- diferencias que se observen.
#============================================================

rm(list = ls())


library(pacman)

p_load(readxl, tidyverse, MultBiplotR, BiplotML, janitor, here, skimr, foreign,
       factoextra, FactoMineR, Factoshiny, FactoInvestigate,
       explor, BiplotGUI,
       corrplot, patchwork,
       RSpectra)


#-------.
data("spiders")
?spiders

datos <- spiders |> 
         mutate(across(everything(), ~ifelse(. == "Absent", 0, 1)))


glimpse(datos)

outLB <- LogBip(x = datos, method = "MM")

View(outLB$Ahat)
