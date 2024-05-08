library(here)
library(DescTools)
library(ggplot2)
library(e1071)
library(corrplot)
library(cluster)
library(factoextra)

# Semilla
set.seed(0)

# Funcion para convertir datos categoricos a numericos
categorical_to_numeric <- function(dataset, categorical_columns) {
    for (i in 1:length(categorical_columns)) {
        uniqueValues <- unique(dataset[,categorical_columns[i]])
        dataset[, categorical_columns[i]] <- sapply(dataset[, categorical_columns[i]], 
                                                function(x) match(x, uniqueValues) - 1)
    }
    return(dataset)
}

# Funcion para calcular el coeficiente de silueta para un modelo de k-means
silhouette_score <- function(k, data, features) {
  kmeans_model <- kmeans(data[, features], centers = k, nstart = 25)
  #kmeans_model <- cmeans(data[, features], centers = k, m=2.5)
  silhouette_score <- silhouette(kmeans_model$cluster, dist(data[, features]))
  
  return (mean(silhouette_score[, 3]))
}

# Funcion para calcular el coeficiente de silueta para un modelo de Fuzzy k-means
silhouette_score_fuzzy <- function(k, data, features, b) {
    kmeans_model <- cmeans(data[, features], centers = k, m=b)
    silhouette_score <- silhouette(kmeans_model$cluster, dist(data[, features]))
  
    return (mean(silhouette_score[, 3]))
}

# Graficar el score de la silueta
plot_silhouette_score <- function(k, scores, title) {
  x11()
  plot(k, type = "b", scores, xlab = "Número de clusters", 
       ylab = "Score de la silueta", frame=FALSE, main=title, 
       col="#33608C")
  #abline(v=k[which.max(scores)], col="#9A68A4", lty=2)
}

# ---------------------------------------------------------------------

# Ruta al dataset, Estimation of Obesity Levels Based On Eating Habits and Physical Condition
dataset_path <- here("Datasets", "Avance1_B.csv")

# Cargar dataset
dataset <- read.table(dataset_path, header = TRUE, sep = ",")

# Vector con los indices de las columnas con datos continuos
continuous_columns <- c(2, 3, 4, 7, 8, 11, 13, 14)

# Vector con los indices de las columnas con datos categoricos
categorical_columns <- c(1, 5, 6, 9, 10, 12, 15, 16)

# Variable con el indice de la columna clase
class_column <- 17

# ---------------------------------------------------------------------
# 1. Preprocesamiento de datos

# --/ Eliminamos la columna class_column
dataset <- dataset[,-class_column]
View(dataset, title = "Dataset original")

# --/ Convertir datos categóricos a numericos
dataset_numeric <- categorical_to_numeric(dataset, categorical_columns)

# --/ Convertir todas las columnas a numericas
dataset_numeric <- as.data.frame(lapply(dataset_numeric, as.numeric))
View(dataset_numeric, title = "Dataset con todos los datos numericos")

# --/ Normalizamos los datos
dataset_norm <- dataset_numeric

dataset_norm <- lapply(dataset_numeric, 
                       function(x) (x - mean(x))/sd(x) )

dataset_normDF <- as.data.frame(dataset_norm)

# --/--/ Resumen de los datos normalizados
mean_values_norm <- sapply(dataset_normDF, mean)
sd_values_norm <- sapply(dataset_normDF, sd)
features_names <- names(dataset_normDF)

# --/--/ Creamos un dataframe con los valores de la media y 
#        desviacion estandar para cada caracteristica
summary_norm <- data.frame(Media=mean_values_norm, 
                            Desviacion_estandar =sd_values_norm)
rownames(summary_norm) <- features_names
View(summary_norm, title = "Resumen de los datos normalizados")

# Eliminamos las valores extremos por medio de la media y la varianza
# --/--/ Se eliminan filas que tengan al menos 
#        una caracteristica con valor extremo
dataset_normDF <- dataset_normDF[apply(dataset_normDF, 1, 
                                function(x) all(abs(x) < 3)),]

# --/--/ Imprimir cantidad de filas eliminadas por valores extremos
cat("Cantidad de filas eliminadas por valores extremos: ", 
    nrow(dataset_numeric) - nrow(dataset_normDF), "\n")

dataset_normDF <- as.data.frame(dataset_normDF)
# ---------------------------------------------------------------------
# 2. Agrupamiento de datos con K-means y Fuzzy K-means

# --/ K-means
# --/--/ Grupos a evaluar K=2,3,4,5,6,7,8,9,10
k <- 2:10

# --/--/ Calcular el score de la silueta para k = 2:10
scoresKmenas <- sapply(k, function(x) silhouette_score(x, data=dataset_normDF, features=features_names))

# --/--/ Guardar el score de la silueta en un dataframe
silhouette_scoresKmenas <- data.frame(k=k, scores=scoresKmenas)
View(silhouette_scoresKmenas, title = "Score de la silueta para K-means")

# --/--/ Graficar el score de la silueta
plot_silhouette_score(k, scoresKmenas, "Score de la silueta para K-means")

# --/ Fuzzy K-means
# --/--/ Valor de b a evaluar
b <- 1.5
# --/--/ Calcular el score de la silueta para k = 2:10
scoresFuzzyKmenas <- sapply(k, function(x) silhouette_score_fuzzy(x, data=dataset_normDF, features=features_names, b=b))

# --/--/ Guardar el score de la silueta en un dataframe
silhouette_scoresFuzzyKmenas <- data.frame(k=k, scores=scoresFuzzyKmenas)
View(silhouette_scoresFuzzyKmenas, title = "Score de la silueta para Fuzzy K-means")

# --/--/ Graficar el score de la silueta 
plot_silhouette_score(k, scoresFuzzyKmenas, "Score de la silueta para Fuzzy K-means")

# ---------------------------------------------------------------------
# 3. Selección de características por el método de envolvente
idx_features <- seq(from=1, to=16)
k_i <- 2



features_selected <- c()
avg_sil_selected <- c()

avg_sil <- sapply(idx_features, function(x) 
	silhouette_score(k_i, data = dataset_normDF, features = features_names[x]))