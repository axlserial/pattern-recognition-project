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

# Vector con los indices de las columnas con datos continuos
continuous_columns <- c(2, 3, 4, 7, 8, 11, 13, 14)

# Vector con los indices de las columnas con datos categoricos
categorical_columns <- c(1, 5, 6, 9, 10, 12, 15, 16)

# Variable con el indice de la columna clase
class_column <- 17

# Nombre de las columnas
names <- colnames(dataset)

# Resumen de los datos
summary(dataset)

# ---------------------------------------------------------------------
# 2. Preprocesamiento

# --/ Imputación de datos:

# --/--/ Verificamos si existen datos con NA
data_na <- sum(is.na(dataset))

cat("Existen ", data_na, " datos faltantes.", "\n")

# Convertir columnas a numericas
dataset[, continuous_columns] <- lapply(dataset[, continuous_columns], as.numeric)

# Mapear las columnas categoricas a numericas (si son 2 categorias, se mapean a 0 y 1)
for (i in categorical_columns) {
	dataset[, i] <- sapply(dataset[, i], function(x) match(x, unique(dataset[, i])) - 1)
}

dataset[, categorical_columns] <- lapply(dataset[, categorical_columns], as.numeric)

# --/ Normalización de datos continuos:

# --/--/ Normalización
dataset_norm <- dataset

cc <- c(categorical_columns, class_column)
dataset_norm[, -cc] <- scale(dataset_norm[, -cc])

# --/ Eliminación de valores extremos:

# --/--/ Se eliminan filas que tengan al menos 
#        una caracteristica con valor extremo
dataset_norm <- dataset_norm[apply(dataset_norm[, continuous_columns], 1, function(x) all(abs(x) < 3)), ]

# --/--/ Imprimir cantidad de filas eliminadas por valores extremos
cat("Filas eliminadas por valores extremos: ", nrow(dataset) - nrow(dataset_norm), "\n")

# ---------------------------------------------------------------------
# 4. Selección de características

# --/ Selección individual (características numéricas), con factor de Fisher

# --/--/ Media global de cada característica
meansGlobalClass <- colMeans(dataset_norm[, -class_column])

# --/--/ Calcular factor de Fisher para cada característica, indicando el nombre de la característica
fisherFactors <- sapply(1:ncol(dataset_norm), function(x) fisherFactor(dataset_norm, class_column, meansGlobalClass, x))

# --/--/ Imprimimos nombre de las características y su factor de Fisher
cat("Características y su factor de Fisher:\n")
for (i in 1:length(fisherFactors)) {
	cat(names[i], ": ", fisherFactors[i], "\n")
}

# --/--/ Características con factor de Fisher NaN (no se pudo calcular)
nanFeatures <- names(dataset[is.nan(fisherFactors)])
print(nanFeatures)

# --/--/ Agregamos los nombres de las características a fisherFactors
names(fisherFactors) <- names

# Ordenamos de mayor a menor
fisherFactors <- sort(fisherFactors, decreasing = TRUE)

# --/--/ Graficamos los factores de Fisher agregando el valor de cada característica 
g1 <- ggplot(data = data.frame(names = names(fisherFactors), fisherFactors = fisherFactors), aes(x = reorder(names, fisherFactors), y = fisherFactors)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Factor de Fisher de las características", x = "Características", y = "Factor de Fisher") +
    geom_text(aes(label = round(fisherFactors, 2)), vjust = -0.5, size = 3)

# --/--/ Guardar PNG con la gráfica
ggsave("fisher_factors_rank_ind.png", plot = g1, path = here("plots", "seleccion"), 
       width=10, height=8, units="in")

# --/ Selección de subconjuntos de características por el metodo Escalar hacia delante

# --/--/ Buscamos características con factor de Fisher NaN
nanFeatures <- names(fisherFactors[is.nan(fisherFactors)])

# Dataset con las características que no presentan problemas y la columna  de la clase
dataset_escalar <- dataset_norm

# --/--/ Correlación de Pearson entre las características
correlationMatrix <- round(cor(dataset_escalar[, -class_column]),2)
View(correlationMatrix)

# --/--/ Graficamos la matriz de correlación de datos
g2 <- ggcorr(correlationMatrix, label = TRUE, label_size = 3, color = "grey50", nbreaks = 6) +
    labs(title = "Correlación de los datos", center = "center")

# --/--/ Guardamos el gráfico anterior
ggsave("correlation_data.png", plot = g2, path = here("plots", "seleccion"), 
	   width=10, height=8, units="in")

# --/--/ Selección de caracteristicas para el 50% y 75% de los datos
c50 <- floor(length(dataset_escalar) * 0.50)
c75 <- floor(length(dataset_escalar) * 0.75)

# --/--/ Parámetros para el método Escalar hacia delante
alf1 <- 0.5
alf2 <- 0.5

# --/--/ Selección de características para el 50%
p50 <- forwardSelection(dataset_escalar, c50, correlationMatrix, fisherFactors, alf1, alf2)
print(p50$selectedFeatures)

# --/--/ Guardamos las características seleccionadas en un archivo .txt, cara característica en una línea
write.table(p50$selectedFeatures, file = here('Entrega final',"selected_features_50.txt"), row.names = FALSE, col.names = FALSE)

# --/--/ Fishers de las características seleccionadas
t50 <- p50$fish

# --/--/ Grfico de los factores de Fisher en t50
g3 <- ggplot(data = data.frame(fish = t50), aes(x = 1:length(t50), y = t50)) +
    geom_line(color = "skyblue") +
    labs(title = "Factores de Fisher en la selección de características 50%", x = "Iteración", y = "Factor de Fisher")

# --/--/ Guardamos el gráfico anterior
ggsave("fisher_factors_selection_50.png", plot = g3, path = here("plots", "seleccion"), 
	   width=10, height=8, units="in")

# --/--/ Selección de características para el 75%
p75 <- forwardSelection(dataset_escalar, c75, correlationMatrix, fisherFactors, alf1, alf2)
print(p75$selectedFeatures)

# --/--/ Guardamos las características seleccionadas en un archivo .txt, cara característica en una línea
write.table(p75$selectedFeatures, file = here('Entrega final',"selected_features_75.txt"), row.names = FALSE, col.names = FALSE)

# --/--/ Fishers de las características seleccionadas
t75 <- p75$fish

# --/--/ Grfico de los factores de Fisher en t75
g4 <- ggplot(data = data.frame(fish = t75), aes(x = 1:length(t75), y = t75)) +
	geom_line(color = "skyblue") +
	labs(title = "Factores de Fisher en la selección de características 75%", x = "Iteración", y = "Factor de Fisher")

# --/--/ Guardamos el gráfico anterior
ggsave("fisher_factors_selection_75.png", plot = g4, path = here("plots", "seleccion"), 
	   width=10, height=8, units="in")

# ---------------------------------------------------------------------

# 5.  Aprendizaje de clasificadores:  Aplicando K-fold cross validation con K=5

# --/ Convertir la columna de la clase de la siguiente forma:
#      1: Insufficient Weight
#      2: Normal Weight
#      3: Overweight Level I y Overweight Level II
#      4: Obesity Type I, Obesity Type II y Obesity Type III

dataset_clf <- dataset_norm

# --/--/ Imprimir la cantidad de registros por clase
cat("Cantidad de registros por clase:\n")
table(dataset_clf[, class_column])

# --/ Mapear los valores de la columna clase a los valores indicados
dataset_clf[, class_column] <- sapply(dataset_clf[, class_column], function(x) {
	if (x == "Insufficient_Weight") {
		return(1)
	} else if (x == "Normal_Weight") {
		return(2)
	} else if (x == "Overweight_Level_I" || x == "Overweight_Level_II") {
		return(3)
	} else {
		return(4)
	}
})

# --/--/ Imprimir la cantidad de registros por clase
cat("Cantidad de registros por clase:\n")
table(dataset_clf[, class_column])


# --/ Clasificador de árbol de decisión utilizando todas las características
NObeyesdad <- dataset_clf$NObeyesdad
model.classification <- rpart(NObeyesdad ~ ., data = dataset_clf, method = "class", control = rpart.control(minsplit = 1, cp = 0.01))

# --/--/ Graficar el árbol de decisión
rpart.plot(model.classification, extra = 1, box.palette= "GnYlRd" ,type = 5, fallen.leaves = TRUE, 
            under = TRUE, faclen = 4, cex = 1, split.cex = 1, split.prefix = " ", cex.main=1.5,
            main="Árbol de decisión con todas las características", branch.lty = 3, under.cex=1)

nombres_caracteristicas <- model.classification$frame$var
nombres_caracteristicas <- nombres_caracteristicas[nombres_caracteristicas != "<leaf>"]

# Imprimimos los valores unicos de nombres_caracteristicas
print(unique(nombres_caracteristicas))

all_features <- unique(nombres_caracteristicas)

# Guardamos las características seleccionadas en un archivo txt
write.table(all_features, file = here('Entrega final',"all_features.txt"), row.names = FALSE, col.names = FALSE)


# --/ Clasificador KNN utilizando cross validation con K=5

# --/--/ Dividir el dataset en 5 partes
set.seed(215)

# --/--/ Shuffle de las filas
dataset_clf <- dataset_clf[sample(nrow(dataset_clf)), ]

# --/--/ k-fold cross validation
k <- 5
folds <- cut(seq(1, nrow(dataset_clf)), breaks = k, labels = FALSE)

# --/--/ Lista para guardar los resultados
results <- list()

# --/--/ Por cada fold, obtener Exactitud, precision, recall y f-score por clase
# --/--/ Para todos los folds, desempeño promedio y desviación estándar de Exactitud, precision, recall y f-score
for (i in 1:k) {
	# --/--/ Dividir el dataset en entrenamiento y prueba
	train <- dataset_clf[folds != i, ]
	test <- dataset_clf[folds == i, ]

	# --/--/ Clasificador KNN
	model <- knn(train = train[, -class_column], test = test[, -class_column], cl
		= train[, class_column], k = 5)

	# --/--/ Matriz de confusión
	confusionMatrix <- table(test[, class_column], model)

	# --/--/ Exactitud
	accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
	
	# --/--/ Por cada clase (1, 2, 3, 4), precision, recall y f-score
	precision <- c()
	recall <- c()
	f_score <- c()

	for (j in 1:4) {
		# --/--/ Precision
		precision <- c(precision, confusionMatrix[j, j] / sum(confusionMatrix[, j]))

		# --/--/ Recall
		recall <- c(recall, confusionMatrix[j, j] / sum(confusionMatrix[j, ]))

		# --/--/ F-score
		f_score <- c(f_score, 2 * precision[j] * recall[j] / (precision[j] + recall[j]))
	}

	# --/--/ Guardar resultados
	results[[i]] <- list(accuracy = accuracy, precision = precision, recall = recall, f_score = f_score)

	# --/--/ Imprimir resultados
	cat("Fold ", i, "\n")
	cat("Exactitud: ", accuracy, "\n")
	cat("Precision: ", precision, "\n")
	cat("Recall: ", recall, "\n")
	cat("F-score: ", f_score, "\n")
}

# --/--/ Desempeño promedio y desviación estándar de Exactitud, precision, recall y f-score
accuracy <- sapply(results, function(x) x$accuracy)
accuracy_mean <- mean(accuracy)
accuracy_sd <- sd(accuracy)

cat("Exactitud promedio: ", accuracy_mean, "\n")
cat("Exactitud desviación estándar: ", accuracy_sd, "\n")

# --/--/ Precision por clase
precision <- sapply(results, function(x) x$precision)
precision_mean <- apply(precision, 1, mean)
precision_sd <- apply(precision, 1, sd)

cat("Precision promedio por clase: ", precision_mean, "\n")
cat("Precision desviación estándar por clase: ", precision_sd, "\n")

# --/--/ Recall por clase
recall <- sapply(results, function(x) x$recall)
recall_mean <- apply(recall, 1, mean)
recall_sd <- apply(recall, 1, sd)

cat("Recall promedio por clase: ", recall_mean, "\n")
cat("Recall desviación estándar por clase: ", recall_sd, "\n")

# --/--/ F-score por clase
f_score <- sapply(results, function(x) x$f_score)
f_score_mean <- apply(f_score, 1, mean)
f_score_sd <- apply(f_score, 1, sd)

cat("F-score promedio por clase: ", f_score_mean, "\n")
cat("F-score desviación estándar por clase: ", f_score_sd, "\n")