library(here)
library(ggplot2)
library(cluster)
library(e1071)

# Funcion para calcular el coeficiente de silueta para un modelo de k-means
silhouette_score <- function(k, data, features) {
  	kmeans_model <- kmeans(data[, features], centers = k, nstart = 25)
  	silhouette_score <- silhouette(kmeans_model$cluster, dist(data[, features]))
  
	return (mean(silhouette_score[, 3]))
}

# Funcion para calcular el coeficiente de silueta para un modelo de Fuzzy k-means
silhouette_score_fuzzy <- function(k, data, features, b) {
    kmeans_model <- cmeans(data[, features], centers = k, m=b)
    silhouette_score <- silhouette(kmeans_model$cluster, dist(data[, features]))
  
    return (mean(silhouette_score[, 3]))
}

# Funcion para encontrar la mejor caracteristica para un modelo de k-means
best_kmeans <- function(k, data, features, selected_features=c()) {
	actual_scores <- sapply(features, function(f) 
		silhouette_score(k, data, c(selected_features, f)))
	best_feature <- which.max(actual_scores)

	return (list(feature=best_feature, score=actual_scores[best_feature]))
}

# Funcion para encontrar la mejor caracteristica para un modelo de Fuzzy k-means
best_fkmeans <- function(k, data, features, selected_features=c()) {
	actual_scores <- sapply(features, function(f) 
		silhouette_score_fuzzy(k, data, c(selected_features, f), b))
	best_feature <- which.max(actual_scores)

	return (list(feature=best_feature, score=actual_scores[best_feature]))
}

# --/ Funcion para encontrar el mejor subconjunto de caracteristicas por cada valor de K
#     utilizando el metodo envolvente (K-means)
best_kfeatures <- function(k, data, features) {
	selected_features <- c()
	selected_avg_score <- c()
	
	# Se obtiene la 1ra caracteristica
	actual_features <- features
	best <- best_kmeans(k, data, actual_features)
	selected_features <- c(selected_features, actual_features[best$feature])
	selected_avg_score <- c(selected_avg_score, best$score)

	# Se obtienen las demas caracteristicas
	for (i in 2:length(features)) {
		actual_features <- features[!features %in% selected_features]
		best <- best_kmeans(k, data, actual_features, selected_features)
		selected_features <- c(selected_features, actual_features[best$feature])
		selected_avg_score <- c(selected_avg_score, best$score)
	}

	return (list(features=selected_features, score=selected_avg_score))
}

# --/ Funcion para encontrar el mejor subconjunto de caracteristicas por cada valor de K
#     utilizando el metodo envolvente (Fuzzy k-means)
best_fkfeatures <- function(k, data, features) {
	selected_features <- c()
	selected_avg_score <- c()
	
	# Se obtiene la 1ra caracteristica
	actual_features <- features
	best <- best_fkmeans(k, data, actual_features)
	selected_features <- c(selected_features, actual_features[best$feature])
	selected_avg_score <- c(selected_avg_score, best$score)

	# Se obtienen las demas caracteristicas
	for (i in 2:length(features)) {
		actual_features <- features[!features %in% selected_features]
		best <- best_fkmeans(k, data, actual_features, selected_features)
		selected_features <- c(selected_features, actual_features[best$feature])
		selected_avg_score <- c(selected_avg_score, best$score)
	}

	return (list(features=selected_features, score=selected_avg_score))
}

# Graficar el score de la silueta
plot_silhouette_score <- function(k, scores, title) {
	x11()
	plot(k, type = "b", scores, xlab = "Número de clusters", 
    	ylab = "Score de la silueta", frame=FALSE, main=title, 
    	col="#33608C")
}

# ---------------------------------------------------------------------

# Semilla
set.seed(0)

# Ruta al dataset, Estimation of Obesity Levels Based On Eating Habits and Physical Condition
dataset_path <- here("Datasets", "Avance1_B.csv")

# Cargar dataset
dataset <- read.table(dataset_path, header = TRUE, sep = ",")
summary(dataset)

# Vector con los indices de las columnas con datos continuos
continuous_columns <- c(2, 3, 4, 7, 8, 11, 13, 14)

# Vector con los indices de las columnas con datos categoricos
categorical_columns <- c(1, 5, 6, 9, 10, 12, 15, 16)

# Variable con el indice de la columna clase
class_column <- 17

# ---------------------------------------------------------------------
# 1. Preprocesamiento de datos

# Eliminar la columna 'NObeyesdad' (columna 17)
dataset <- dataset[, -class_column]

# Convertir columnas a numericas
dataset[, continuous_columns] <- lapply(dataset[, continuous_columns], as.numeric)

# Mapear las columnas categoricas a numericas (si son 2 categorias, se mapean a 0 y 1)
for (i in categorical_columns) {
	dataset[, i] <- sapply(dataset[, i], function(x) match(x, unique(dataset[, i])) - 1)
}

# Normalizar los datos
dataset <- scale(dataset)

# Eliminar valores extremos
dataset <- dataset[apply(dataset, 1, function(x) all(abs(x) < 3)), ]

# ---------------------------------------------------------------------
# 2. Clustering

kclusters <- 2:10

# A. K-means para cada valor de k
kscores <- sapply(kclusters, function(x) silhouette_score(x, dataset, 1:ncol(dataset)))
plot_silhouette_score(kclusters, kscores, "K-means")

# B. Fuzzy k-means para cada valor de k
b <- 1.5
kscores_fuzzy <- sapply(kclusters, function(x) silhouette_score_fuzzy(x, dataset, 1:ncol(dataset), b))
plot_silhouette_score(kclusters, kscores_fuzzy, "Fuzzy k-means")

# C. Encontrar caracteristicas por método envolvente

# Eliminar columnas con datos categoricos
dnew <- dataset[, -categorical_columns]

# Obtenemos los names de las columnas
nm = colnames(dnew)

# --/ Encontrar el mejor subconjunto de características con K-means
best_features_kmeans <- lapply(kclusters, function(kc) best_kfeatures(kc, dnew, 1:ncol(dnew)))

# --/ Imprimir las caracteristicas seleccionadas, su score y el centro de los clusters
for (i in 1:length(kclusters)) {
  cat("K-means, K=", kclusters[i], "\n")
  cat("Caracteristicas seleccionadas: ", nm[best_features_kmeans[[i]]$features], "\n")
  cat("Score: ", best_features_kmeans[[i]]$score, "\n")
  
  kmeans_model <- kmeans(dnew[, best_features_kmeans[[i]]$features], centers = kclusters[i], nstart = 25)
  cat("Centros:\n")
  print(kmeans_model$centers)
}

# Graficar los scores de cada subconjunto de caracteristicas
for (i in 1:length(kclusters)) {
	x11()
	plot(1:ncol(dnew), best_features_kmeans[[i]]$score, type = "b", xlab = "Número de características", 
		ylab = "Score de la silueta", frame=FALSE, main=paste("K-means, K=", kclusters[i]), 
		col="#33608C")
}

# --/ Encontrar el mejor subconjunto de características con Fuzzy k-means
best_features_fkmeans <- lapply(kclusters, function(kc) best_fkfeatures(kc, dnew, 1:ncol(dnew)))

# --/ Imprimir las caracteristicas seleccionadas, su score y el centro de los clusters
for (i in 1:length(kclusters)) {
  cat("K=", kclusters[i], ":", nm[best_features_fkmeans[[i]]$features], "\n")

  cat("Score:", best_features_fkmeans[[i]]$score, "\n")

  kmeans_model <- cmeans(dnew[, best_features_fkmeans[[i]]$features], centers = kclusters[i], m=b)
  cat("Centros:\n")
  print(kmeans_model$centers)

  cat("\n")
}


# Graficar los scores de cada subconjunto de caracteristicas
for (i in 1:length(kclusters)) {
	x11()
	plot(1:ncol(dnew), best_features_fkmeans[[i]]$score, type = "b", xlab = "Número de características", 
		ylab = "Score de la silueta", frame=FALSE, main=paste("Fuzzy k-means, K=", kclusters[i]), 
		col="#33608C")
}

# --/ A partir de los resultados obtenidos, se grafica el conteo de las caracteristicas seleccionadas
#     por cada valor de K desde la caracteristica 1 hasta el score maximo
#     (K-means)
x11()
plot(kclusters, sapply(best_features_kmeans, function(x) which.max(x$score)), 
	type = "b", xlab = "Número de clusters", ylab = "Número de características seleccionadas", 
	frame=FALSE, main="Seleccionadas K-means", col="#33608C")

#     (Fuzzy k-means)
x11()
plot(kclusters, sapply(best_features_fkmeans, function(x) which.max(x$score)), 
	type = "b", xlab = "Número de clusters", ylab = "Número de características seleccionadas", 
	frame=FALSE, main="Seleccionadas Fuzzy k-means", col="#33608C")
