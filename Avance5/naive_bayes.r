library(here)
library(ggplot2)
library(caTools)
library(naivebayes)

# ----------------------------------------------------------

# ----------------------------------------------------------

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

names <- colnames(dataset)

# ----------------------------------------------------------
# 1. Preprocesamiento de datos

# Convertir columnas a numericas
dataset[, continuous_columns] <- lapply(dataset[, continuous_columns], as.numeric)

# Mapear las columnas categoricas a numericas (si son 2 categorias, se mapean a 0 y 1)
for (i in categorical_columns) {
	dataset[, i] <- sapply(dataset[, i], function(x) match(x, unique(dataset[, i])) - 1)
}

# Volvemos categorica la columna clase
dataset[, class_column] <- as.factor(dataset[, class_column])

# Normalizar los datos, sin considerar la columna 'NObeyesdad'
#dataset[, -class_column] <- scale(dataset[, -class_column])
#View(dataset)

# Eliminar valores extremos, sin considerar la columna 'NObeyesdad' , mas de 3 desviaciones estandar
#dataset <- dataset[apply(dataset[, -class_column], 1, function(x) all(abs(x) < 3)),]

#summary(dataset)
# ----------------------------------------------------------
# 2. Division de datos en entrenamiento y prueba, 75% y 25% respectivamente
set.seed(215)
split <- sample.split(dataset[, class_column], SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
#View(training_set)
test_set <- subset(dataset, split == FALSE)
#View(test_set)

# Imprimir el tamaño de los conjuntos de entrenamiento y prueba
print(paste("Tamaño del conjunto de entrenamiento: ", nrow(training_set)))
print(paste("Tamaño del conjunto de prueba: ", nrow(test_set)))



# ----------------------------------------------------------
# 3. Entrenamiento del modelo Naive Bayes Gaussiano

# Entrenamiento del modelo para todas las caracteristicas
ppriori <- table(training_set$NObeyesdad)
ppriori <- ppriori / sum(ppriori)

print("Probabilidades a priori:")
print(ppriori)

# Entrenamiento del modelo
classiffier_gnb <- gaussian_naive_bayes(
    x = as.matrix(training_set[,1:16]),
    y = training_set$NObeyesdad,
    levels = levels(training_set$NObeyesdad), 
    prior = ppriori)

newClass <- predict(classiffier_gnb, 
					newdata = as.matrix(test_set[,1:16]),
					type = "class")


# Matriz de confusion
clase_true <- test_set$NObeyesdad
clase_pred <- newClass
cm <- table(Predicted = clase_pred, True = clase_true)
cm <- as.data.frame.table(cm)

print("Matriz de confusion:")
ggplot(data = cm, aes(x = True, y = Predicted, fill = Freq)) +
	geom_tile() +
	geom_text(aes(label = Freq), vjust = 1) +
	scale_fill_gradient(low = "white", high = "blue") +
	labs(x = "Clase real", y = "Clase predicha", fill = "Frecuencia", 
	     title="Bayes Ingenuo con todas las caracteriticas") +
	
x11()

# ----------------------------------------------------------

# Entrenamos el modelo con el 50% de las caracteristicas

# Cargar las caracteristicas seleccionadas, son los nombres de las columnas a considerar
features <- scan("Avance5/features_50.txt", what = character())

# Cerrar la conexion al archivo
close("Avance5/features_50.txt")

# Obtener los indices de las columnas seleccionadas
selected_features_50 <- match(features, names)

# Impresion de las caracteristicas seleccionadas
print(paste("Caracteristicas seleccionadas: ", names[selected_features_50]))

# Indice de las caracteristicas seleccionadas
selected_features_50 <- names[selected_features_50]

# Entrenamiento del modelo
classiffier_gnb_50 <- gaussian_naive_bayes(
	x = as.matrix(training_set[,selected_features_50]),
	y = training_set$NObeyesdad,
	levels = levels(training_set$NObeyesdad), 
	prior = ppriori)

newClass_50 <- predict(classiffier_gnb_50, 
					newdata = as.matrix(test_set[,selected_features_50]),
					type = "class")

# Matriz de confusion
clase_true <- test_set$NObeyesdad
clase_pred <- newClass_50
cm <- table(Predicted = clase_pred, True = clase_true)
cm <- as.data.frame.table(cm)

print("Matriz de confusion:")
ggplot(data = cm, aes(x = True, y = Predicted, fill = Freq)) +
	geom_tile() +
	geom_text(aes(label = Freq), vjust = 1) +
	scale_fill_gradient(low = "white", high = "blue") +
	labs(x = "Clase real", y = "Clase predicha", fill = "Frecuencia", 
	     title="Bayes Ingenuo con el 50% de las caracteriticas") +

x11()

# ----------------------------------------------------------

# Entrenamos el modelo con el 75% de las caracteristicas

# Cargar las caracteristicas seleccionadas, son los nombres de las columnas a considerar
features <- scan("Avance5/features_75.txt", what = character())

# Cerrar la conexion al archivo
close("Avance5/features_75.txt")

# Obtener los indices de las columnas seleccionadas
selected_features_75 <- match(features, names)

# Impresion de las caracteristicas seleccionadas
print(paste("Caracteristicas seleccionadas: ", names[selected_features_75]))

# Indice de las caracteristicas seleccionadas
selected_features_75 <- names[selected_features_75]

# Entrenamiento del modelo
classiffier_gnb_75 <- gaussian_naive_bayes(
	x = as.matrix(training_set[,selected_features_75]),
	y = training_set$NObeyesdad,
	levels = levels(training_set$NObeyesdad), 
	prior = ppriori)

newClass_75 <- predict(classiffier_gnb_75, 
					newdata = as.matrix(test_set[,selected_features_75]),
					type = "class")

# Matriz de confusion
clase_true <- test_set$NObeyesdad
clase_pred <- newClass_75
cm <- table(Predicted = clase_pred, True = clase_true)
cm <- as.data.frame.table(cm)

print("Matriz de confusion:")
ggplot(data = cm, aes(x = True, y = Predicted, fill = Freq)) +
	geom_tile() +
	geom_text(aes(label = Freq), vjust = 1) +
	scale_fill_gradient(low = "white", high = "blue") +
	labs(x = "Clase real", y = "Clase predicha", fill = "Frecuencia", 
	     title="Bayes Ingenuo con el 75% de las caracteriticas") +

x11()

# ----------------------------------------------------------

# Entrenamos el modelo con las caracteristicas seleccionadas del archivo 'selected_features_dtree.txt'

# Cargar las caracteristicas seleccionadas, son los nombres de las columnas a considerar
features <- scan("Avance5/selected_features_dtree.txt", what = character())

# Cerrar la conexion al archivo
close("Avance5/selected_features_dtree.txt")

# Obtener los indices de las columnas seleccionadas
selected_features_tree <- match(features, names)

# Impresion de las caracteristicas seleccionadas
print(paste("Caracteristicas seleccionadas: ", names[selected_features_tree]))

# Indice de las caracteristicas seleccionadas
selected_features_tree <- names[selected_features_tree]

# Entrenamiento del modelo
classiffier_gnb_tree <- gaussian_naive_bayes(
	x = as.matrix(training_set[,selected_features_tree]),
	y = training_set$NObeyesdad,
	levels = levels(training_set$NObeyesdad), 
	prior = ppriori)

newClass_tree <- predict(classiffier_gnb_tree, 
					newdata = as.matrix(test_set[,selected_features_tree]),
					type = "class")

# Matriz de confusion
clase_true <- test_set$NObeyesdad
clase_pred <- newClass_tree
cm <- table(Predicted = clase_pred, True = clase_true)
cm <- as.data.frame.table(cm)

print("Matriz de confusion:")
ggplot(data = cm, aes(x = True, y = Predicted, fill = Freq)) +
	geom_tile() +
	geom_text(aes(label = Freq), vjust = 1) +
	scale_fill_gradient(low = "white", high = "blue") +
	labs(x = "Clase real", y = "Clase predicha", fill = "Frecuencia", 
	     title="Bayes Ingenuo con las caracteristicas seleccionadas por el árbol de decisión") +

x11()

# ----------------------------------------------------------