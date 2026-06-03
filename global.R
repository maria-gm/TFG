# =====================================================
# LIBRERÍAS
# =====================================================
library(shiny)# app web interactiva (Shiny)
library(ggplot2) # visualización avanzada de datos
library(corrplot)# gráficos de matrices de correlación
library(readr)# importación eficiente de datos
library(palmerpenguins)# dataset penguins
library(mlbench) # datasets clásicos (ej. Breast Cancer)
library(rattle) # dataset Wine
library(MASS) # Boston
library(psych)# análisis factorial y descriptivos multivariantes
library(factoextra)# visualización e interpretación de PCA y clustering
library (cluster) # métodos clásicos de agrupamiento
library(dbscan)# clustering basado en densidad
library(polycor) # correlaciones para variables ordinales/no normales
library(ggcorrplot)  #visualización de matrices de correlación
library(GPArotation)  # rotaciones factoriales para AF
library(plotly) # gráficos interactivos
library(dplyr) # manipulación de datos
library(tidyr) # reestructuración de datos (wide/long)
library(pls)# regresión por mínimos cuadrados parciales
library(glmnet)# regresión regularizada (Lasso, Ridge)
library(pROC)# curvas ROC y evaluación de modelos
library(rpart) #arboles de decision 
library(rpart.plot)
library(ggsci)# paletas de color científicas
library(bslib) #estilo
library(shinycssloaders) # menu principal
library(DT)
library(ggdendro)
# =====================================================
# DATASETS DE EJEMPLO
# =====================================================

data(BreastCancer, package = "mlbench")
breast_cancer <- BreastCancer

data(penguins, package = "palmerpenguins")

data(wine, package = "rattle")
wine <- rattle::wine

data(Boston, package = "MASS")

datos_ejemplo <- list(
  PCA = wine,
  AF = wine,
  BIPLOT= wine,
  MDS = wine,
  Jerarquicos = penguins,
  K_means = penguins,
  DBSCAN=penguins,
  Regresion_multiple=Boston,
  Regresion_logistica=breast_cancer, 
  Regularizacion=wine,
  PCR=wine,
  LDA=wine,
  Arboles=breast_cancer 
)
