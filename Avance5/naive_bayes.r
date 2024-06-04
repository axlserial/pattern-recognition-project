library(here)
library(ggplot2)
library(caTools)
library(class)
library(naivebayes)

# Ruta al dataset, Estimation of Obesity Levels Based On Eating Habits and Physical Condition
dataset_path <- here("Datasets", "Avance1_B.csv")

# Cargar dataset
dataset <- read.table(dataset_path, header = TRUE, sep = ",")
summary(dataset)

# Vector con los indices de las columnas con datos continuos
continuous_columns <- c(2, 3, 4, 7, 8, 11, 13, 14)

# Vector con los indices de las columnas con datos categoricos
categorical_columns <- c(1, 5, 6, 9, 10, 12, 15, 16, 17)

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

# ----------------------------------------------------------
# 2. Division de datos en entrenamiento y prueba, 75% y 25% respectivamente

set.seed(215)
split <- sample.split(dataset[, class_column], SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
View(training_set)
test_set <- subset(dataset, split == FALSE)
View(test_set)

# Imprimir el tamaño de los conjuntos de entrenamiento y prueba
print(paste("Tamaño del conjunto de entrenamiento: ", nrow(training_set)))
print(paste("Tamaño del conjunto de prueba: ", nrow(test_set)))

# ----------------------------------------------------------

# 3. Naive Bayes numeric