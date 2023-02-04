######========================================================
######   Script por Giovany Babativa-Márquez, PhD. 
######========================================================

# Los vinos elaborados en áreas específicas y reconocidos con denominación de origen (DO) 
# son de importancia significativa en las diferentes regiones productoras de vinos. La DO 
# reconoce y garantiza calidad de los vinos fabricados. Consecuentemente, son necesarios 
# una serie de parámetros específicos que permitan a los analistas clasificar distintos vinos 
# en sus correspondientes denominaciones de origen. Entre las características que pueden usarse 
# están la composición en ciertos metales, ácidos orgánicos, ciertos componentes polifenólicos, 
# etc...   Los valores de  estas características dependen de diversos factores, tales como las 
# variedades de uva empleadas en el proceso de elaboración, o la edad del vino.

# Se ha realizado un estudio sobre las dos denominaciones de origen de vinos castellanos 
# (Ribera de Duero y Toro) en dos años diferentes (1986, 1987), con el fin de distinguir 
# las características diferenciales entre las dos denominaciones, mediante medidas objetivas 
# obtenidas en laboratorio, de forma que pueda evitarse el fraude en las etiquetas de la denominación 
# sustituyendo ambos vinos debido a su proximidad espacial.

# Se han considerado 4 grupos diferentes procedentes de la combinación de denominaciones 
# y años (RD1986, RD1987, T1986, T1987). Se ha considerado el año como posible factor de 
# confusión en la clasificación de los vinos de las dos denominaciones. 


# Objetivo: Caracterizar las diferencias entre los vinos de dos denominaciones de origen 
# (Ribera del Duero y Toro) según algunas variables objetivo.

#-------------------------------------------

rm(list = ls())


library(pacman)

p_load(readxl, tidyverse, MultBiplotR, BiplotML, janitor, here, skimr, foreign,
       factoextra, FactoMineR, Factoshiny, FactoInvestigate,
       explor, BiplotGUI,
       corrplot, patchwork,
       RSpectra)


#----- MANOVA Biplot

vinos <- read.spss("data/vinos.sav", to.data.frame = T) |> 
         janitor::clean_names()

table(vinos$denomina, vinos$a_o)

glimpse(vinos)

X <- as.matrix(vinos[,4:21])

#---- Primero haremos una análisis multivariante de la varianza
?manova

manvin <- manova(X ~ vinos$grupo)
summary(manvin)   # Se rechaza Ho

summary(manvin, test = "Wilks")
summary(manvin, test = "Hotelling")
summary(manvin, test = "Roy")

#---- Representación multivariante -- Manova Biplot
?CanonicalBiplot
canbip <- CanonicalBiplot(X, group=vinos$grupo)
summary(canbip)

plot(canbip)
plot(canbip, mode="s")
