library(ggplot2)
library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(lawstat)

data <- read.csv("szbv-lu-2020-2050-tief.csv", sep = ";")

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
data_sample <- numeric_vars[sample(nrow(numeric_vars), 10000), ]

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