######========================================================
######   Script por Giovany Babativa-Márquez, PhD. 
######========================================================
#==  Use el conjunto de datos sobre consumo de proteínas en varios   
#==  países para realizar un análisis HJ-Biplot,
#==  Utilice los paquetes
#==      1. MultBiplotR
#==      2. BiplotGUI
#============================================================

rm(list = ls())

#.... Paquetes.

library(pacman)

p_load(readxl, tidyverse, MultBiplotR, BiplotML, janitor, here, skimr, foreign,
       factoextra, FactoMineR, Factoshiny, FactoInvestigate,
       explor, BiplotGUI,
       corrplot, patchwork,
       RSpectra)


######-------------------- Ejemplo Proteinas

proteinas <- read_excel(here::here("data/Consumo Proteinas.xlsx")) |> 
             clean_names()

glimpse(proteinas)

datos <- proteinas |> 
         column_to_rownames(var = "pais") |> 
         select(where(is.numeric))


skim(datos)

#-- Análisis HJ-Biplot usando el paquete MultBiplotR


hjProteinas <-  MultBiplotR::HJ.Biplot(datos)

hjProteinas$Inertia


data.frame(value=hjProteinas$Inertia) |> 
  mutate(Eje = 1:n()) |> 
  ggplot(aes(x = Eje, y = value)) +
  geom_col(fill = "lightblue") +
  labs(y = "% Inercia") +
  geom_text(aes(label=round(value, 1)), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_bw() 


hjProteinas$ColContributions

corrplot(hjProteinas$ColContributions, is.corr =  F) # method = "pie"

hjProteinas$RowContributions


corrplot(hjProteinas$RowContributions, is.corr =  F) 


summary(hjProteinas)

plot(hjProteinas)
CorrelationCircle(hjProteinas)

#####---- Biplot Interactivo usando el paquete BiplotGUI

BiplotGUI::Biplots(datos)

