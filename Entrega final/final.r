# Ruta al dataset
library(here)
library(DescTools)

# Graficas 
library(ggplot2)

# Agrupamiento
library(cluster)
library(e1071)

# Clasificadores
library(GGally)
library(rpart)
library(rpart.plot)
library(caTools)
library(class)

# ---------------------------------------------------------------------
 
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

	#print(selected_features)

	# Se obtienen las demas caracteristicas
	for (i in 2:length(features)) {
		actual_features <- features[!features %in% selected_features]
		best <- best_kmeans(k, data, actual_features, selected_features)
		selected_features <- c(selected_features, actual_features[best$feature])
		selected_avg_score <- c(selected_avg_score, best$score)
		#print(selected_features)
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
plot_silhouette_score <- function(k, scores, title, file) {
	png(file=paste0(here("plots", "agrupamiento"),"/", file, ".png"))
	plot(k, type = "b", scores, xlab = "Número de clusters", 
    	ylab = "Score de la silueta", frame=FALSE, main=title, 
    	col="#33608C")
	dev.off()
}

# Función para calcular el factor de Fisher, donde se tienen los siguientes parámetros:
# - dataset: dataset con las características
# - classColumn: columna de la clase
# - meansGlobalClass: media global de cada característica
# - feature: índice de la característica

fisherFactor <- function(dataset, classColumn, meansGlobalClass, feature) {
    # Columna de la clase
    classColumn <- dataset[, classColumn]

    # Columna de la característica a evaluar
    featureColumn <- dataset[, feature]

    # Media condicional de la característica de acuerdo a la clase
    meansClass <- tapply(featureColumn, classColumn, mean)

	# Si media de la clase es NA, se asigna 0
	meansClass[is.na(meansClass)] <- 0

    # Desviación estandar condicional de la característica de acuerdo a la clase
    devestsClass <- tapply(featureColumn, classColumn, sd)

    # Proporción de registros por clase de forma condicional
    proportionsClass <- table(classColumn) / length(classColumn)

    # Factor de Fisher
    fisherFactor <- sum(proportionsClass * (meansClass - meansGlobalClass[feature])^2) / 
                    sum(proportionsClass * (devestsClass^2))

    return(fisherFactor)
}

# Función del método Escalar hacia delante, donde se tienen los siguientes parámetros:
# - dataset: dataset con las características
# - classColumn: columna de la clase
# - numFeatures: número de características a seleccionar
# - correlationMatrix: matriz de correlación de Pearson
# - fisherFactors: factores de Fisher
# - alf1: 
# - alf2:

forwardSelection <- function(dataset, numFeatures, correlationMatrix, fisherFactors, alf1, alf2) {
    fish <- c()
    seledtedFeatures <- c()

    # Primera característica seleccionada

    # indice de la característica con mayor factor de Fisher
    s1 <- which.max(fisherFactors)
    s1nn <- names(fisherFactors)[s1]

    # Guardamos en fish la característica con mayor factor de Fisher
    fish <- c(fish, max(fisherFactors))

    # Agregamos la característica a la lista de características seleccionadas
    seledtedFeatures <- c(seledtedFeatures, s1nn)

	# Selección de la segunda característica
	# Se obtiene la correlación de Pearson
	corr <- correlationMatrix[s1nn, correlationMatrix[s1nn, ] != 1]

	# Se obtiene el factor de Fisher de las características no seleccionadas
	fisher <- fisherFactors[fisherFactors != fisherFactors[s1]]

	# Se aplica la formula de selección: s2 = maxj{α1F(Aj) − α2|ρAs1,Aj|}, j ̸= s1
	s2 <- which.max(alf1 * fisher - alf2 * corr)
	s2nn <- names(fisherFactors)[s2]

    fish <- c(fish, max(alf1 * fisher - alf2 * corr))
	# Agregamos la característica a la lista de características seleccionadas
	seledtedFeatures <- c(seledtedFeatures, s2nn)

	# Resto de las características seleccionadas
	for (i in 3:numFeatures) {
		not_selected <- names(fisherFactors)[!names(fisherFactors) %in% seledtedFeatures]
		corrs <- correlationMatrix[seledtedFeatures, not_selected]
		
		sk_values <- c()
		for (ns in not_selected) {
			ns_fisher <- fisherFactors[ns]
			corr_sum <- sum(abs(corrs[, ns]))

			sk_values <- c(sk_values, alf1 * ns_fisher - (alf2 / (i - 1)) * corr_sum)
		}

        cat("Correlación multi-variable en la iteración", i, ":", max(sk_values), "\n")
        fish <- c(fish, max(sk_values))

		# Obtenemos la mejor caracteristica
		sk_index <- which.max(sk_values)
		sk <- names(sk_values)[sk_index]

		# Agregamos la característica a la lista de características seleccionadas
		seledtedFeatures <- c(seledtedFeatures, sk)

        # Analisis de la correlación multi-variable
        # corrs <- correlationMatrix[seledtedFeatures, seledtedFeatures]
        # corrs <- corrs[upper.tri(corrs, diag = FALSE)]

        # # Imprimimos la correlación multi-variable, por iteración
        # cat("Correlación multi-variable en la iteración", i, ":", max(corrs), "\n")
        
	}

    list(selectedFeatures = seledtedFeatures, fish = fish)
}

# ---------------------------------------------------------------------

# Semilla
set.seed(123)

# Ruta al dataset, Estimation of Obesity Levels Based On Eating Habits and Physical Condition
dataset_path <- here("Datasets", "Avance1_B.csv")

# Cargar dataset
dataset <- read.table(dataset_path, header = TRUE, sep = ",")
summary(dataset)

data2 <- dataset

# Vector con los indices de las columnas con datos continuos
continuous_columns <- c(2, 3, 4, 7, 8, 11, 13, 14)

# Vector con los indices de las columnas con datos categoricos
categorical_columns <- c(1, 5, 6, 9, 10, 12, 15, 16)

# Variable con el indice de la columna clase
class_column <- 17

# Convertir columnas a numericas
dataset[, continuous_columns] <- lapply(dataset[, continuous_columns], as.numeric)

# Convertir columnas a factores
dataset[, categorical_columns] <- lapply(dataset[, categorical_columns], factor)

# Convertir columna clase a factor
dataset[, class_column] <- factor(dataset[, class_column])

# Resumen de los datos
summary(dataset)


# ---------------------------------------------------------------------
# 1. Descripción de los datos

# --/ Para datos continuos:

# --/--/ MAX
max_values <- sapply(dataset[, continuous_columns], max)

# --/--/ MIN
min_values <- sapply(dataset[, continuous_columns], min)

# --/--/ Promedio
mean_values <- sapply(dataset[, continuous_columns], mean)

# --/--/ Desviación estándar
sd_values <- sapply(dataset[, continuous_columns], sd)


j <- 1
# --/ Histogramas de las caracteristicas continuas
for (feature in continuous_columns) {
	col_name <- colnames(dataset[])[feature]

	p <- ggplot(dataset, aes(x=dataset[, feature])) + 
		geom_histogram(binwidth = 1, color="grey", fill=colors[j], alpha=0.2) + 
		labs(title = paste("Histograma de:", col_name), x=col_name, y="Frecuencia")+
        theme(plot.title = element_text(hjust=0.5, size=18, face="bold"),
              axis.title=element_text(size=13, face="italic"),
              axis.text=element_text(size=12))

	# x11()
	# print(p)

    j <- j+1

	# --/--/ Guardar PNG con la gráfica
	ggsave(paste("Histograma_", col_name, ".png"), plot = p, path = here("plots", "description"), 
    width=10, height=8, units="in")
}


# --/ Para datos categóricos:

# --/--/ Moda
mode_values <- sapply(dataset[, categorical_columns], Mode)
# --/--/ Frecuencia de valores (y la columna de clase)
categorical_class_columns <- c(categorical_columns, class_column)

colors <- rainbow(ncol(dataset[, categorical_columns]))
print(colors)
i <- 1
for (feature in categorical_class_columns) {
	col_name <- colnames(dataset)[feature]

	dataframe <- as.data.frame(table(dataset[, feature]))
	names(dataframe) <- c(col_name, "Freq")

	p <- ggplot(data = dataframe, aes(x=dataframe[, 1], y=dataframe[, 2])) + 
		geom_bar(stat = "identity", fill=colors[i], alpha=0.2) + 
		labs(title = paste("Frecuencias para la caracteristica: ", col_name), x=col_name, y="Frecuencia")+
        theme(plot.title = element_text(hjust=0.5, size=18, face="bold"),
              axis.title=element_text(size=13, face="italic"),
              axis.text=element_text(size=12))
	#x11()
	#print(p)
    i <- i+1

	# --/--/ Guardar PNG con la gráfica
	ggsave(paste("Frecuencias_", col_name, ".png"), plot = p, path = here("plots", "description"), 
    width=10, height=8, units="in")
    
	# --/--/ Imprimir descripción
	cat("Feature: ", col_name, "\tMode: ", mode_values[col_name], "\n")
}

# ---------------------------------------------------------------------
# 2. Preprocesamiento

# --/ Imputación de datos:

# --/--/ Verificamos si existen datos con NA
data_na <- sum(is.na(dataset))

cat("Existen ", data_na, " datos faltantes.", "\n")

# --/ Convertir datos categóricos a numéricos: 
dataset[, continuous_columns] <- lapply(dataset[, continuous_columns], as.numeric)

# --/--/ Mapear las columnas categoricas a numericas (si son 2 categorias, se mapean a 0 y 1)
for (i in categorical_columns) {
	dataset[, i] <- sapply(dataset[, i], function(x) match(x, unique(dataset[, i])) - 1)
}

# --/ Normalización de datos continuos:

# --/--/ Normalización
dataset_norm <- dataset

cc <- c(categorical_columns, class_column)
dataset_norm[, -cc] <- scale(dataset_norm[, -cc])

View(dataset_norm)

# --/ Eliminación de valores extremos:

# --/--/ Se eliminan filas que tengan al menos 
#        una caracteristica con valor extremo
dataset_norm <- dataset_norm[apply(dataset_norm[, continuous_columns], 1, function(x) all(abs(x) < 3)), ]

# --/--/ Imprimir cantidad de filas eliminadas por valores extremos
cat("Filas eliminadas por valores extremos: ", nrow(dataset) - nrow(dataset_norm), "\n")


# ---------------------------------------------------------------------
# 3. Agrupamiento

kclusters <- 2:10

indice <- match("NObeyesdad", colnames(dataset_norm))
dataset_clustering <- dataset_norm[,-indice]

# --/ K-means para cada valor de k: 
kscores <- sapply(kclusters, function(x) silhouette_score(x, dataset_clustering, 1:ncol(dataset_clustering)))
plot_silhouette_score(kclusters, kscores, "K-means", "kmeans")

# --/ Fuzzy k-means para cada valor de k:
b <- 1.5
kscores_fuzzy <- sapply(kclusters, function(x) silhouette_score_fuzzy(x, dataset_clustering, 1:ncol(dataset_clustering), b))
plot_silhouette_score(kclusters, kscores_fuzzy, "Fuzzy k-means", "fuzzykmeans")


# Eliminar columnas con datos categoricos
dnew <- dataset_clustering[, -categorical_columns]

# Obtenemos los names de las columnas
nm = colnames(dnew)

# --/ Encontrar el mejor subconjunto de características con K-means
best_features_kmeans <- lapply(kclusters, function(kc) best_kfeatures(kc, dnew, 1:ncol(dnew)))

# --/--/ Imprimir las caracteristicas seleccionadas, su score y el centro de los clusters
for (i in 1:length(kclusters)) {
  cat("K-means, K=", kclusters[i], "\n")
  cat("Caracteristicas seleccionadas: ", nm[best_features_kmeans[[i]]$features], "\n")
  cat("Score: ", best_features_kmeans[[i]]$score, "\n")
  
  kmeans_model <- kmeans(dnew[, best_features_kmeans[[i]]$features], centers = kclusters[i], nstart = 25)
  cat("Centros:\n")
  print(kmeans_model$centers)
}

# --/--/ Graficar los scores de cada subconjunto de caracteristicas
for (i in 1:length(kclusters)) {
	png(file=paste0(here("plots", "agrupamiento","kmeans"),"/km_", i+1, ".png"))
	plot(1:ncol(dnew), best_features_kmeans[[i]]$score, type = "b", xlab = "Número de características", 
		ylab = "Score de la silueta", frame=FALSE, main=paste("K-means, K=", kclusters[i]), 
		col="#33608C")
	dev.off()
}

# --/ Encontrar el mejor subconjunto de características con Fuzzy k-means
best_features_fkmeans <- lapply(kclusters, function(kc) best_fkfeatures(kc, dnew, 1:ncol(dnew)))

# --/--/ Imprimir las caracteristicas seleccionadas, su score y el centro de los clusters
for (i in 1:length(kclusters)) {
  cat("K=", kclusters[i], ":", nm[best_features_fkmeans[[i]]$features], "\n")

  cat("Score:", best_features_fkmeans[[i]]$score, "\n")

  kmeans_model <- cmeans(dnew[, best_features_fkmeans[[i]]$features], centers = kclusters[i], m=b)
  cat("Centros:\n")
  print(kmeans_model$centers)

  cat("\n")
}

# --/--/ Graficar los scores de cada subconjunto de caracteristicas
for (i in 1:length(kclusters)) {
	png(file=paste0(here("plots", "agrupamiento","fkmeans"),"/fkm_", i+1, ".png"))
	plot(1:ncol(dnew), best_features_fkmeans[[i]]$score, type = "b", xlab = "Número de características", 
		ylab = "Score de la silueta", frame=FALSE, main=paste("Fuzzy k-means, K=", kclusters[i]), 
		col="#33608C")
	dev.off()
}

# --/ A partir de los resultados obtenidos, se grafica el conteo de las caracteristicas seleccionadas
#     por cada valor de K desde la caracteristica 1 hasta el score maximo

# --/--/ (K-means)
png(file=paste0(here("plots", "agrupamiento"),"/fkm_caracteristicas",".png"))
plot(kclusters, sapply(best_features_kmeans, function(x) which.max(x$score)), 
	type = "b", xlab = "Número de clusters", ylab = "Número de características seleccionadas", 
	frame=FALSE, main="Seleccionadas K-means", col="#33608C")
dev.off()

# --/--/ (Fuzzy k-means)
png(file=paste0(here("plots", "agrupamiento"),"/km_caracteristicas",".png"))
plot(kclusters, sapply(best_features_fkmeans, function(x) which.max(x$score)), 
	type = "b", xlab = "Número de clusters", ylab = "Número de características seleccionadas", 
	frame=FALSE, main="Seleccionadas Fuzzy k-means", col="#33608C")
dev.off()

# ---------------------------------------------------------------------
# 4. Selección de características

# Rankeo de las características con el factor de Fisher
meansGlobalClass <- colMeans(dataset_norm[, -class_column])
View(meansGlobalClass)

fisherFactors <- sapply(1:ncol(dataset_norm), function(x) fisherFactor(dataset_norm, class_column, meansGlobalClass, x))

# Imprimir el factor de Fisher de cada característica y el nombre de la característica
cat("Factor de Fisher de cada característica:\n")

for (i in 1:length(fisherFactors)) {
	cat(colnames(dataset_norm)[i], ": ", fisherFactors[i], "\n")
}