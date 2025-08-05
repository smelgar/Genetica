# Instalar y cargar paquetes necesarios si no están instalados
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("ade4", quietly = TRUE)) {
  install.packages("ade4")
}

library(MASS)
library(FactoMineR)
library(ade4)
library(factoextra)


set.seed(12) # Para reproducibilidad

## Parámetros comunes a los tres grupos
# Matriz de covarianza (misma estructura de correlación para todos)
sigma <- matrix(c(
  4.0, 3.8, 3.6, 3.5,     # Longitud
  3.8, 9.0, 6.3, 6.0,     # Anchura
  3.6, 6.3, 16.0, 12.0,  # Altura
  3.5, 6.0, 12.0, 25.0    # Peso
), nrow = 4, byrow = TRUE)

# Nombres de las variables
colnames(sigma) <- rownames(sigma) <- c("Longitud", "Anchura", "Altura", "Distancia")

## Medias para cada grupo (distintos promedios)
mu_grupo1 <- c(5, 10, 20, 15)
mu_grupo2 <- c(10, 20, 40, 30)
mu_grupo3 <- c(20, 15, 20, 35)

## Simular datos para cada grupo
n <- 100 # Tamaño muestral por grupo

grupo1 <- as.data.frame(mvrnorm(n, mu = mu_grupo1, Sigma = sigma))
grupo2 <- as.data.frame(mvrnorm(n, mu = mu_grupo2, Sigma = sigma))
grupo3 <- as.data.frame(mvrnorm(n, mu = mu_grupo3, Sigma = sigma))

# Añadir identificador de grupo
grupo1$Grupo <- "Grupo 1"
grupo2$Grupo <- "Grupo 2"
grupo3$Grupo <- "Grupo 3"

# Combinar todos los datos
datos <- rbind(grupo1, grupo2, grupo3)
datos$Grupo <- factor(datos$Grupo)

# 1. Transformación logarítmica de los datos
datos_log <- datos
# Aplicar logaritmo a las variables numéricas
datos_log[, 1:4] <- log(datos_log[, 1:4])
print("Datos después de la transformación logarítmica (primeras 6 filas):")
print(head(datos_log))

# 2. Creación de la columna "T" (promedio de todas las variables)
datos_log$T <- rowMeans(datos_log[, 1:4])
print("\nDatos con la nueva columna 'T' (primeras 6 filas):")
print(head(datos_log))

# 3. Creación de un nuevo dataframe con cada variable dividida entre "T"
datos_normalizados_por_T <- datos_log
datos_normalizados_por_T[, 1:4] <- datos_normalizados_por_T[, 1:4] / datos_normalizados_por_T$T
print("\nDatos normalizados por 'T' (primeras 6 filas):")
print(head(datos_normalizados_por_T))

# 4. Análisis de Componentes Principales (PCA)
# Excluir la columna 'Grupo' y 'T' para el PCA
datos_pca <- datos_normalizados_por_T[, 1:4] # Usar las variables transformadas y normalizadas

# Realizar el PCA
pca_resultado <- PCA(datos_pca, graph = FALSE)
print("\nResultados del Análisis de Componentes Principales:")
print(pca_resultado)

# Gráfico de las variables en el PCA
plot(pca_resultado, choix = "var", title = "Círculo de Correlaciones PCA")
# Gráfico de los individuos en el PCA, coloreados por grupo
plot(pca_resultado, choix = "ind", #habillage = datos_normalizados_por_T$Grupo,
     title = "Individuos en el PCA por Grupo")

fviz_pca_ind(pca_resultado,
             col.ind = datos_normalizados_por_T$Grupo, # Aquí pasas el vector del grupo directamente
             addEllipses = TRUE, # Opcional: añade elipses de confianza alrededor de los grupos
             ellipse.type = "convex", # Tipo de elipse
             legend.title = "Grupo", # Título de la leyenda
             repel = TRUE, # Evita la superposición de etiquetas
             title = "Individuos en el PCA por Grupo") +
  scale_color_discrete(name = "Grupo") # Personaliza el título de la leyenda de color


# 5. Análisis Discriminante con base en los resultados del PCA
# Extraer las coordenadas de los individuos en los componentes principales
# Se suelen usar los primeros componentes que explican la mayor parte de la varianza.
# Para este ejemplo, usaremos los 2 primeros componentes.
# Puedes ajustar el número de componentes (k) según la varianza explicada.
componentes_principales <- as.data.frame(pca_resultado$ind$coord)

# Preparar los datos para el análisis discriminante lineal (LDA)
# Se necesitan las componentes principales y la variable de grupo.
datos_lda <- data.frame(Grupo = datos_normalizados_por_T$Grupo, componentes_principales)

# Realizar el Análisis Discriminante Lineal (LDA)
# Es importante asegurarse de que haya al menos dos grupos y que cada grupo
# tenga suficientes observaciones para estimar las matrices de covarianza.
lda_resultado <- lda(Grupo ~ Dim.1 + Dim.2 + Dim.3, data = datos_lda)
print("\nResultados del Análisis Discriminante Lineal (LDA):")
print(lda_resultado)

# Predicciones y visualización del LDA
lda_predicciones <- predict(lda_resultado, newdata = datos_lda)
print("\nClasificación de los primeros 10 individuos por LDA:")
print(head(lda_predicciones$class, 10))

# Visualización de los resultados del LDA (si hay al menos 2 funciones discriminantes)
if (ncol(lda_predicciones$x) >= 2) {
  plot(lda_predicciones$x, col = datos_lda$Grupo, pch = 19,
       main = "Análisis Discriminante Lineal (LDA) de Componentes Principales",
       xlab = paste0("LD1 (", round(lda_resultado$svd[1]^2 / sum(lda_resultado$svd^2) * 100, 2), "%)"),
       ylab = paste0("LD2 (", round(lda_resultado$svd[2]^2 / sum(lda_resultado$svd^2) * 100, 2), "%)"))
  legend("topright", legend = levels(datos_lda$Grupo), fill = 1:length(levels(datos_lda$Grupo)))
} else if (ncol(lda_predicciones$x) == 1) {
  # Si solo hay una función discriminante, se puede hacer un histograma o densidad
  plot(density(lda_predicciones$x[datos_lda$Grupo == "Grupo 1"]), col = "red", main = "LDA scores por Grupo", xlab = "LD1",
       ylim = c(0, max(density(lda_predicciones$x[datos_lda$Grupo == "Grupo 1"])$y,
                       density(lda_predicciones$x[datos_lda$Grupo == "Grupo 2"])$y,
                       density(lda_predicciones$x[datos_lda$Grupo == "Grupo 3"])$y)))
  lines(density(lda_predicciones$x[datos_lda$Grupo == "Grupo 2"]), col = "blue")
  lines(density(lda_predicciones$x[datos_lda$Grupo == "Grupo 3"]), col = "green")
  legend("topright", legend = levels(datos_lda$Grupo), fill = c("red", "blue", "green"))
}

