library(MASS)
library(ggplot2)
library(GGally)

set.seed(12) # Para reproducibilidad

## Parámetros comunes a los tres grupos
# Matriz de covarianza (misma estructura de correlación para todos)
sigma <- matrix(c(
  4.0, 3.8, 3.6, 3.5,    # Longitud
  3.8, 9.0, 6.3, 6.0,    # Anchura
  3.6, 6.3, 16.0, 12.0,  # Altura
  3.5, 6.0, 12.0, 25.0    # Peso
), nrow = 4, byrow = TRUE)

# Nombres de las variables
colnames(sigma) <- rownames(sigma) <- c("Longitud", "Anchura", "Altura", "Peso")

## Medias para cada grupo (distintos promedios)
mu_grupo1 <- c(5, 10, 20, 15)
mu_grupo2 <- c(15, 20, 25, 30)
mu_grupo3 <- c(40, 55, 20, 35)

## Simular datos para cada grupo
n <- 50 # Tamaño muestral por grupo

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

## Visualización de pares de variables
ggpairs(datos, 
        columns = 1:4,
        mapping = aes(color = Grupo),
        title = "Matriz de gráficos de dispersión por grupo",
        upper = list(continuous = "points"),
        lower = list(continuous = "cor"),
        diag = list(continuous = "densityDiag")) +
  theme_bw()

## Gráfico PCA para ver la separación de grupos
library(factoextra)

pca_result <- prcomp(datos[,1:4], scale = TRUE)

fviz_pca_ind(pca_result,
             habillage = datos$Grupo,
             addEllipses = TRUE,
             palette = c("#E7B800", "#00AFBB", "#FC4E07"),
             title = "Análisis de Componentes Principales") +
  theme_minimal()

## Verificar correlaciones por grupo (deben ser similares)
cat("Correlaciones Grupo 1:\n")
print(cor(grupo1[,1:4]))

cat("\nCorrelaciones Grupo 2:\n")
print(cor(grupo2[,1:4]))

cat("\nCorrelaciones Grupo 3:\n")
print(cor(grupo3[,1:4]))
