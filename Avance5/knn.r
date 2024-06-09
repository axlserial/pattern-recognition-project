library(here)
library(ggplot2)
library(caTools)
library(class)

# ----------------------------------------------------------

trainingKnn <- function(training_set, test_set, features, k, class_column) {
	# Entrenamiento del modelo KNN
  	knn_model <- knn(train = training_set[, features], test = test_set[, features], 
					 cl = training_set[, class_column], k = k)
  
  	# Matriz de confusion
  	confusion_matrix <- table(Prediction = knn_model, Reference = test_set[, class_column])
	print(confusion_matrix)
  
  	# Convertir la matriz de confusion a un dataframe para ggplot
  	confusion_matrix_df <- as.data.frame.table(confusion_matrix)

	return(confusion_matrix_df)
}

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

# Normalizar los datos, sin considerar la columna 'NObeyesdad'
dataset[, -class_column] <- scale(dataset[, -class_column])
#View(dataset)

# Eliminar valores extremos, sin considerar la columna 'NObeyesdad' , mas de 3 desviaciones estandar
dataset <- dataset[apply(dataset[, -class_column], 1, function(x) all(abs(x) < 3)),]


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
# 3. K-Nearest Neighbors (KNN)

# Definimos las k vecinos mas cercanos como el numero de clases 
k <- length(unique(training_set[, class_column]))
print(paste("Numero de clases: ", k))

# Indice de las caracteristicas seleccionadas
selected_features <- names(dataset[,-class_column])

# Entrenamiento del modelo KNN para todas las caracteristicas
confusion_matrix_df <- trainingKnn(training_set, test_set, selected_features, k, 17)

# Visualizar la matriz de confusion
print("Matriz de confusion:")
ggplot(data = confusion_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
	geom_tile() +
	geom_text(aes(label = Freq), vjust = 1) +
	scale_fill_gradient(low = "white", high = "blue") +
	labs(x = "Clase real", y = "Clase predicha", fill = "Frecuencia", 
	     title="KNN considerendo todas las características") +
	
x11()

# Entrenamiento del modelo KNN para el 50% de las caracteristicas obtenidas de forma aletoria

# Seleccionar 50% de las caracteristicas de forma aleatoria
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

# Entrenamiento del modelo KNN
confusion_matrix_df_50 <- trainingKnn(training_set, test_set, selected_features_50, k, 17)

# Visualizar la matriz de confusion
print("Matriz de confusion:")
ggplot(data = confusion_matrix_df_50, aes(x = Reference, y = Prediction, fill = Freq)) +
	geom_tile() +
	geom_text(aes(label = Freq), vjust = 1) +
	scale_fill_gradient(low = "white", high = "blue") +
	labs(x = "Clase real", y = "Clase predicha", fill = "Frecuencia", 
	     title="KNN considerendo el 50% de las características") +

x11()

# Entrenamiento del modelo KNN para el 75% de las caracteristicas obtenidas de forma aletoria

# Seleccionar 75% de las caracteristicas de forma aleatoria
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

# Entrenamiento del modelo KNN
confusion_matrix_df_75 <- trainingKnn(training_set, test_set, selected_features_75, k,17)

# Visualizar la matriz de confusion
print("Matriz de confusion:")
ggplot(data = confusion_matrix_df_75, aes(x = Reference, y = Prediction, fill = Freq)) +
	geom_tile() +
	geom_text(aes(label = Freq), vjust = 1) +
	scale_fill_gradient(low = "white", high = "blue") +
	labs(x = "Clase real", y = "Clase predicha", fill = "Frecuencia", 
	     title="KNN considerendo el 75% de las características") +

x11()

# Entrenamiento del modelo KNN para las caracteristicas seleccionadas del archivo selected_features_dtree.txt

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

# Entrenamiento del modelo KNN
confusion_matrix_df_dtree <- trainingKnn(training_set, test_set, selected_features_tree, k, 17)

# Visualizar la matriz de confusion
print("Matriz de confusion:")
ggplot(data = confusion_matrix_df_dtree, aes(x = Reference, y = Prediction, fill = Freq)) +
	geom_tile() +
	geom_text(aes(label = Freq), vjust = 1) +
	scale_fill_gradient(low = "white", high = "blue") +
	labs(x = "Clase real", y = "Clase predicha", fill = "Frecuencia", 
	     title="KNN considerendo las características seleccionadas por el árbol de decisión") +

x11()
