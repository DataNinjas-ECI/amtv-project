library(ggplot2)
library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(lawstat)
library(psych)
library(corrr)
library(correlation)
library(patchwork)

convert_to_numeric <- function(data) {
  data$sex <- ifelse(data$sex == "m", 0, ifelse(data$sex == "w", 1, NA))
  data$nation <- ifelse(data$nation == "A", 0, ifelse(data$nation == "CH", 1, NA))
  return(data)
}

data <- read.csv("szbv-lu-2020-2050-tief.csv", sep = ";", fileEncoding = "latin1")
head(data)

color_var <- as.character(unique(data[["sex"]]))

Graf_dispersion_facet_wrap <- function(df, var1, var2, color = "sex"){
  ggplot(df, aes_string(x = var1, y = var2, color = color)) +
    geom_point(show.legend = FALSE) +
    labs(x = var1, y = var2) +
    facet_wrap(vars(Species))
}

#Graf_dispersion(iris, "Sepal.Length", "Petal.Width")


Graf_dispersion <- function(df, var1, var2, color = "sex"){
  ggplot(df, aes_string(x = var1, y = var2, color = color)) +
    geom_point(show.legend = FALSE) +
    labs(x = var1, y = var2) 
}

#Graf_dispersion(iris, "Sepal.Length", "Petal.Width")



comb_disp_facet <- function(df, var1, var2, color = "sex"){
  p1 <- Graf_dispersion_facet_wrap(df, var1, var2, color = color)
  p2 <- Graf_dispersion(df, var1, var2, color = color)
  p1
}

comb_disp_facet(data, "data$swb0", "data$ein", "data$sex")



# Función que toma un data y un número como entrada
generar_matriz_correlaciones <- function(data, año_numero) {
  
  # Filtrar registros según el año proporcionado
  data_filtrado <- subset(data, jahr == año_numero)
  data_filtrado <- subset(data_filtrado, select = -c(jahr))
  
  # Obtener los nombres de las variables numéricas
  nombres_variables <- names(data_filtrado)
  
  # Crear una matriz de correlaciones con nombres de variables
  matriz_correlaciones <- matrix(NA, nrow = length(nombres_variables), ncol = length(nombres_variables))
  rownames(matriz_correlaciones) <- nombres_variables
  colnames(matriz_correlaciones) <- nombres_variables
  
  # Llenar la matriz de correlaciones
  for (i in 1:length(nombres_variables)) {
    for (j in 1:length(nombres_variables)) {
      # Calcular la correlación si ambas variables son numéricas
      if (is.numeric(data_filtrado[, i]) && is.numeric(data_filtrado[, j])) {
        matriz_correlaciones[i, j] <- round(cor(data_filtrado[, i], data_filtrado[, j]), 2)
      } 
      else if (names(data_filtrado)[i] == "sex" && names(data_filtrado)[j] != "nation"
               && names(data_filtrado)[j] != "sex"){
        matriz_correlaciones[i, j] <- round(biserial.cor(data_filtrado[,j], data_filtrado[,i], use = c("all.obs"), level = 2), 2)
      }
      else if (names(data_filtrado)[j] == "sex" && names(data_filtrado)[i] != "nation"
               && names(data_filtrado)[i] != "sex"){
        matriz_correlaciones[i, j] <- round(biserial.cor(data_filtrado[,i], data_filtrado[,j], use = c("all.obs"), level = 2), 2)
      }
      else if (names(data_filtrado)[i] == "nation" && names(data_filtrado)[j] != "sex"
               && names(data_filtrado)[j] != "nation"){
        matriz_correlaciones[i, j] <- round(biserial.cor(data_filtrado[,j], data_filtrado[,i], use = c("all.obs"), level = 2), 2)
      }
      else if (names(data_filtrado)[j] == "nation" && names(data_filtrado)[i] != "sex"
               && names(data_filtrado)[i] != "nation"){
        matriz_correlaciones[i, j] <- round(biserial.cor(data_filtrado[,i], data_filtrado[,j], use = c("all.obs"), level = 2), 2)
      }
      else if (names(data_filtrado)[i] == names(data_filtrado)[j]){
        matriz_correlaciones[i, j] <- c(1)
      }
      
      else {
        matriz_correlaciones[i, j] <- c(0)
      } 
    }
  }
  # Retornar la matriz de correlaciones con nombres de variables
  return(matriz_correlaciones)
}

data <- convert_to_numeric(data)
selected_vars <- c("jahr", "gnr", "alter", "sex", "nation", "swb0", "geb", "tod", "ws", "ein", "swb")
data <- data[, selected_vars]
sample_size <- min(nrow(data), 10000)
data <- data[sample(nrow(data), sample_size),]
numeric_vars <- data[, sapply(data, is.numeric)]  # seleccionar solo las variables numéricas

generar_matriz_correlaciones(data, "2026")

library(correlation)
library(bayestestR)
library(see)
library(datawizard)
library(poorman)

# Función para generar resultados
generate_results <- function(data, r, n = 100, transformation = "none") {
  if (transformation != "none") {
    var <- ifelse(grepl("(", transformation, fixed = TRUE), "data$swb)", "data$swb")
    transformation <- paste0(transformation, var)
    data$swb <- eval(parse(text = transformation))
  }
  
  out <- data.frame(n = n, transformation = transformation, r = r)
  
  out$Pearson <- cor_test(data, "sex", "swb", method = "pearson")$r
  out$Spearman <- cor_test(data, "sex", "swb", method = "spearman")$rho
  out$Kendall <- cor_test(data, "sex", "swb", method = "kendall")$tau
  out$Biweight <- cor_test(data, "sex", "swb", method = "biweight")$r
  out$Distance <- cor_test(data, "sex", "swb", method = "distance")$r
  
  out
}

# Efecto del tipo de relación
data_results <- data.frame()
for (r in seq(0, 0.999, length.out = 200)) {
  for (n in 100) {
    for (transformation in c(
      "none",
      "exp(",
      "log10(1+max(abs(data$swb))+",
      "1/",
      "tan(",
      "sin(",
      "cos(",
      "cos(2*",
      "abs(",
      "data$swb*",
      "data$swb*data$swb*",
      "ifelse(data$swb>0, 1, 0)*("
    )) {
      data_results <- rbind(data_results, generate_results(data, r, n, transformation = transformation))
    }
  }
}

# Gráfico
data_results %>%
  datawizard::reshape_longer(
    select = -c("n", "r", "transformation"),
    names_to = "Type",
    values_to = "Estimation"
  ) %>%
  mutate(Type = relevel(as.factor(Type), "Pearson", "Spearman", "Kendall", "Biweight", "Distance")) %>%
  ggplot(aes(x = r, y = Estimation, fill = Type)) +
  geom_smooth(aes(color = Type), method = "loess", alpha = 0, na.rm = TRUE) +
  geom_vline(aes(xintercept = 0.5), linetype = "dashed") +
  geom_hline(aes(yintercept = 0.5), linetype = "dashed") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  see::theme_modern() +
  scale_color_flat_d(palette = "rainbow") +
  scale_fill_flat_d(palette = "rainbow") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~transformation)

# Modelo de regresión lineal
model <- data_results %>%
  datawizard::reshape_longer(
    select = -c("n", "r", "transformation"),
    names_to = "Type",
    values_to = "Estimation"
  ) %>%
  lm(r ~ Type / Estimation, data = .) %>%
  parameters::parameters()

# Ordenar los coeficientes de manera descendente
arrange(model[6:10, ], desc(Coefficient))


head(data)

# EDA Descriptivo
# Resumen estadístico de data
summary(data)

# Contar datos faltantes por variable
sum(is.na(data))

# Distribución de Nacimientos y Muertes
ggplot(data, aes(x=geb)) + geom_histogram(bins=30, fill="blue", alpha=0.7) + 
  ggtitle("Distribución de Nacimientos") + xlab("Nacimientos") + ylab("Frecuencia")
ggplot(data, aes(x=tod)) + geom_histogram(bins=30, fill="red", alpha=0.7) + 
  ggtitle("Distribución de Muertes") + xlab("Muertes") + ylab("Frecuencia")

# Distribución de la Población Total
ggplot(data, aes(x=swb)) + geom_histogram(bins=30, fill="green", alpha=0.7) + 
  ggtitle("Distribución de la Población Total") + xlab("Población Total") + ylab("Frecuencia")

# Distribución por Edad
ggplot(data, aes(x=alter)) + geom_histogram(bins=30, fill="orange", alpha=0.7) + 
  ggtitle("Distribución por Edad") + xlab("Edad") + ylab("Frecuencia")

# Distribuciones de cómo la migración y la inmigración afectan los números de población.
# Histograma para 'ws'
ggplot(data, aes(x=ws)) + geom_histogram(binwidth = 1, fill="blue", alpha=0.7) + 
  ggtitle("Cambio en la población debido a movimientos espaciales") +
  xlab("Movimientos Espaciales") + ylab("Frecuencia")

# Histograma para 'ein'
ggplot(data, aes(x=ein)) + geom_histogram(binwidth = 1, fill="green", alpha=0.7) + 
  ggtitle("Inmigración durante el año") +
  xlab("Inmigración") + ylab("Frecuencia")

# EDA Multivariado
# Calculando la matriz de correlación para las variables numéricas
numeric_vars <- data[, sapply(data, is.numeric)]  # seleccionar solo las variables numéricas
cor_matrix <- cor(numeric_vars)
cor_matrix
corrplot(cor_matrix, method = "circle")

# Tabla de contingencia para sexo y nacionalidad
sex_nation_table <- table(data$sex, data$nation)
sex_nation_table

# Gráfico de dispersión para inmigración vs. población al final del año
# Scatter Plot para 'swb' vs 'ein'
ggplot(data, aes(x=swb, y=ein)) + geom_point(alpha=0.6) + 
  ggtitle("Población al Final del Año vs Inmigración") +
  xlab("Población al Final del Año") + ylab("Inmigración")

# Scatter Plot para 'swb' vs 'ws'
ggplot(data, aes(x=swb, y=ws)) + geom_point(alpha=0.6, color="red") + 
  ggtitle("Población al Final del Año vs Cambio por Movimientos Espaciales") +
  xlab("Población al Final del Año") + ylab("Movimientos Espaciales")

# Gráfico de dispersión para nacimientos vs. movimientos espaciales
ggplot(data, aes(x = ws, y = geb)) +
  geom_point(alpha = 0.6, color = "blue") +
  ggtitle("Relación entre Nacimientos y Movimientos Espaciales") +
  xlab("Movimientos Espaciales") +
  ylab("Nacimientos")

# PCA
# Aplicación del Análisis de Componentes Principales
set.seed(123) # Fijar la semilla para reproducibilidad
data_sample <- numeric_vars[sample(nrow(numeric_vars), 1000), ]
# Aplicar PCA
res.pca <- PCA(data_sample, ncp = 5, scale.unit=TRUE)

# Ver resultados del PCA
res.pca

# Visualizar los resultados del PCA: Gráfico de varianza explicada
fviz_eig(res.pca)

# Visualización del biplot de individuos y variables
fviz_pca_biplot(res.pca, repel = TRUE)

# Para la clasificación jerárquica, primero determinamos el número óptimo de clusters
fviz_nbclust(data_sample, FUN = hcut, method = "wss")

# Ejecutar la clasificación jerárquica
set.seed(123)
res.hcpc <- HCPC(res.pca, nb.clust = -1, graph = FALSE)  # nb.clust = -1 determina el número automáticamente

# Ver resultados de la clasificación jerárquica
res.hcpc

# Visualizar los clusters
fviz_cluster(res.hcpc, data = data_sample)


# MCA
data_cat <- data[, c("jahr", "gemeinde", "alter", "sex", "nation")]
head(data_cat)
# Convertir todas las columnas a factores
data_cat <- data.frame(lapply(data_cat, factor))
data_cat <- data_cat[sample(nrow(data_cat), 1000), ]
# Realizar MCA
res.mca <- MCA(data_cat)

# Visualizar el gráfico de sedimentación para ver la varianza explicada por cada dimensión
fviz_screeplot(res.mca)

# Visualización de las variables en el espacio de las dos primeras dimensiones
fviz_mca_var(res.mca, axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, ggtheme = theme_minimal())

# Otra visualización, por ejemplo, usando las dimensiones 1 y 3
fviz_mca_var(res.mca, axes = c(1, 3), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, ggtheme = theme_minimal())

kmMCA <- kmeans(res.mca$ind$coord, 3) 
kmMCA
kmMCA$cluster
kmMCA$centers

table(data_cat$nation, kmMCA$cluster)
plot(res.mca$ind$coord, col=kmMCA$cluster,pch=20)
points(kmMCA$centers,col=1:3, pch=8, cex=2)

# AF
head(data)
numeric_vars <- data[, c("gnr", "geb", "tod", "ws", "ein", "swb")]

mydata <- numeric_vars[sample(nrow(numeric_vars), 1000), ]
cor(mydata)
# Verificar la adecuación de la muestra para el AF
KMO(mydata)
cortest.bartlett(mydata)

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values

scree(mydata, pc=FALSE)

fa.parallel(mydata, fa="fa")


Nfacs <- 2  # This is for four factors. You can change this as needed.

fit <- factanal(mydata, Nfacs, rotation="promax")


print(fit, digits=2, cutoff=0.3, sort=TRUE)

load <- fit$loadings[,2]
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)


loads <- fit$loadings

fa.diagram(loads)
