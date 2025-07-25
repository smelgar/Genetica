# instalación del paquete CPCA
# Asegúrate de tener devtools instalado
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
library(devtools)

# Instala el paquete usando la sintaxis correcta
install_github("variani/cpca")

# El siguiente es el ejemplo
require("plyr")
require("abind")

data(iris)

C <- daply(iris, "Species", function(x) cov(x[, -ncol(x)]))

C <- aperm(C, c(2, 3, 1)) # put the 1st dimension to the end
dim(C)
dimnames(C)

mod <- cpc(C)
str(mod)

round(mod$CPC, 2)
# See Trendafilov (2010). Stepwise estimation of common principal components. 
# Computational Statistics & Data Analysis, 54(12), 3446-3457. 
# doi:10.1016/j.csda.2010.03.010
# p. 10, Example 2
#
#     [,1]  [,2]  [,3]  [,4]
#[1,] 0.75 -0.09  0.63  0.20
#[2,] 0.44  0.79 -0.33 -0.26
#[3,] 0.47 -0.60 -0.54 -0.34
#[4,] 0.15  0.02 -0.45  0.88
#
# The eigenvectors must be the same, as the default method in `cpc` function
# is the power algorithm proposed by Trendafilov.
# para la prueba de homogenidad de matrices de varianza-covarianzas, se emplea la prueba M de Box:
# O usando biotools::boxm
 library(biotools) # Asegúrate de que esté cargado
 boxm_test_biotools <- boxM(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")], iris$Species)
 print(boxm_test_biotools)
 