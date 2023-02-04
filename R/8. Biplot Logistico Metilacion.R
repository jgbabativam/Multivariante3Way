######========================================================
######   Script por Giovany Babativa-Márquez, PhD. 
######========================================================

# Iorio y col. (2016) realizaron una investigación en el marco del Genomic Determinants 
# of Sensitivity in Cancer 1000 (GDSC1000).  De la investigación se pueden extraer diferentes 
# tipos de información sobre líneas celulares de cáncer provenientes de más de 11 mil tumores 
# para 30 tipos de cáncer que integran mutaciones somáticas, copia del número de alteraciones (CNA), 
# metilaciones del ADN y cambios de expresión de genes. Las primeras tres son obtenidas como datos 
# binarios mientras que la expresión genética está medida con variables cuantitativas que son 
# continuas.

# El archivo contiene todos los datos unidos en una sola matriz en un formato diferente al requerido y
# por esta razón fue necesario realizar un preprocesamiento que permitió organizar los datos y adecuarlos para 
# aplicar los métodos. 

# Para facilitar los análisis obtenidos se incluyeron solo tres tipos de cáncer: 
#   - carcinoma invasivo de mama (BRCA), 
#   - adenocarcinoma de pulmón (LUAD) y 
#   - melanoma cutáneo de piel (SKCM). 
# 
# Realice un análisis a partir de un biplot logístico para los datos de metilación del ADN.

#============================================================

rm(list = ls())


library(pacman)

p_load(readxl, tidyverse, MultBiplotR, BiplotML, janitor, here, skimr, foreign,
       factoextra, FactoMineR, Factoshiny, FactoInvestigate,
       explor, BiplotGUI,
       corrplot, patchwork,
       RSpectra)


#-------.
  
load("data/xMethy.rda")


cvMet_MM <- cv_LogBip(data = xMethy |> 
                        select(-`Cancer Type`), k=0:5, method = "MM")

bipMethy_MM <- LogBip(x = xMethy |> 
                        select(-`Cancer Type`),
                      k = 2,
                      method = "MM",
                      col.ind = xMethy$`Cancer Type`)

PlotMet_MM <- plotBLB(bipMethy_MM, xylim = c(-100,100),
                      col.ind = xMethy$`Cancer Type`, repel = T )+
              labs(title = "Biplot Logístico",
                   subtitle = "Estimación con el algoritmo MM-BCD",
                   caption = "Gráfico elaborado por Giovany Babativa") +
              theme_void() 


PlotMet_MM


Pi <- fitted_LB(bipMethy_MM, type = "response")

View(Pi)
