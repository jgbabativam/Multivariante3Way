
rm(list = ls())

library(FactoMineR)
library(factoextra)
library(tidyverse)

data(wine)
colnames(wine)

glimpse(wine)

res.mfa <- MFA(wine,  group = c(2, 5, 3, 10, 9, 2), 
               type = c("n", "s", "s", "s", "s", "s"),
               name.group = c("origen","olor.pre.agi","asp.visual",
                              "olor.pos.agi", "sabor","desemp.gral"),
               num.group.sup = c(1, 6),
               graph = FALSE)

print(res.mfa)
eig.val = get_eigenvalue(res.mfa)
head(eig.val)

fviz_screeplot(res.mfa)

grupos <- get_mfa_var(res.mfa, "group")
grupos

# Coordenadas de los grupos
head(grupos$coord)

# Cos2: calidad de la representación en el mapa factorial
head(grupos$cos2)

# Contribución a las dimensiones
head(grupos$contrib)

# Grafico para el grupo de variables
fviz_mfa_var(res.mfa, "group")

quanti.var = get_mfa_var(res.mfa, "quanti.var")
quanti.var

# Coordinates
head(quanti.var$coord)
# Cos2: quality on the factore map
head(quanti.var$cos2)
# Contributions to the dimensions
head(quanti.var$contrib)

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE)

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

# Contributions to dimension 1
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")

# Contributions to dimension 2
fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")


g1 =fviz_mfa_var(res.mfa, "quanti.var", col.var = "cos2", 
                 gradient.cols = c("white", "blue", "red"), 
                 col.var.sup = "violet", repel = TRUE,
                 geom = c("point", "text"))

g1

#### Grafica de los individuos

ind = get_mfa_ind(res.mfa)

g2 = fviz_mfa_ind(res.mfa, col.ind = "cos2", 
                  gradient.cols = c("white", "blue", "red"),
                  repel = TRUE)

g2

library(gridExtra)

dev.new()
grid.arrange(g1, g2, nrow=1)


###La primera dimensión representa a la intensidad... 
###1 DAM es el más intenso y armonioso, contrario a 1VAU y 2ING

#### El eje 2 esta asociado a los vinos T2 y T1 caracterizados
#### fuerte valor en Spice antes de agitar

### la mayoría de variables cualitativas están sobre el origen
### mostrando que no hay asociación con la intensidad y armonia,o
### con el segundo eje

### Aunque la categoria Soil=REFERENCE se relaciona con vinos
### de calidad, frutas, armoniosos como 1ING o 1POY


table(wine$Label)

fviz_mfa_ind(res.mfa, 
             habillage = "Label", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) 

table(wine$Soil)

#### Grafico para multiples variables categoricas
fviz_ellipses(res.mfa, c("Label", "Soil"), repel = TRUE)


##### Grafica parcial de individuos
fviz_mfa_ind(res.mfa, partial = "all") 

### Solo algunos

fviz_mfa_ind(res.mfa, partial = c("1DAM", "1VAU", "2ING"))

###El vino 1DAM ha sido descrito  
###como particularmente "intenso" y "armonioso", 
### note el grupo de olores pre: tiene una alta coordenada 
#### en el primer eje desde el punto de vista 


### Desde el grupo de olores, 2ING fue más "intenso" y "armonioso" 
## que 1VAU, pero desde el punto de vista del grupo de sabor
### 1VAU fue más "intenso" y "armonioso" que 2ING.

##### Grafic de ejes parciales

fviz_mfa_axes(res.mfa)


#### Ahora hagamos el ejercicio con Factoshiny

library(Factoshiny)
out <- Factoshiny::Factoshiny(wine)
