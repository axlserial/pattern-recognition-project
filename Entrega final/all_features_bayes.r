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
library(naivebayes)


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
#dataset_norm <- dataset

#cc <- c(categorical_columns, class_column)
#dataset_norm[, -cc] <- scale(dataset_norm[, -cc])

# --/ Eliminación de valores extremos:

# --/--/ Se eliminan filas que tengan al menos 
#        una caracteristica con valor extremo
#dataset <- dataset[apply(dataset[, continuous_columns], 1, function(x) all(abs(x) < 3)), ]

# --/--/ Imprimir cantidad de filas eliminadas por valores extremos
#cat("Filas eliminadas por valores extremos: ", nrow(dataset) - nrow(dataset_norm), "\n")

# ---------------------------------------------------------------------

# 5.  Aprendizaje de clasificadores:  Aplicando K-fold cross validation con K=5

# --/ Convertir la columna de la clase de la siguiente forma:
#      1: Insufficient Weight
#      2: Normal Weight
#      3: Overweight Level I y Overweight Level II
#      4: Obesity Type I, Obesity Type II y Obesity Type III

dataset_clf <- dataset

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

# --/ Convertir en factor la columna clase
dataset_clf[, class_column] <- as.factor(dataset_clf[, class_column])

class_column <- match("NObeyesdad", colnames(dataset_clf))

# --/ Clasificador naive bayes utilizando cross validation con K=5

# --/ Todas las caracteristicas
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

	# --/--/ Clasificador Naive Bayes gaussiano

	# --/--/ Probabilidades a priori
	prior <- table(train[, class_column])
	prior <- prior / sum(prior)

	# --/--/ Modelo gaussiano
	model <- gaussian_naive_bayes(x = as.matrix(train[, -class_column]),
								  y = train[, class_column],
								  levels = levels(train[, class_column]),
								  prior = prior)

	# --/--/ Clasificar
	newClass <- predict(model, newdata = as.matrix(test[, -class_column]), type = "class")

	# --/--/ Matriz de confusión
	confusionMatrix <- table(test[, class_column], newClass)

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

#--/--/ Desempeño promedio y desviación estándar de Exactitud, precision, recall y f-score
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
