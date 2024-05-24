library(here)
library(DescTools)
library(ggplot2)

# # Establecer directorio de trabajo 
# # (solo si el directorio actual no es la raíz del proyecto)
# set_here(path='..')

# Ruta al dataset
dataset_path <- here("Datasets", "Avance1_B.csv")

# Cargar dataset
dataset <- read.table(dataset_path, header = TRUE, sep = ",")
summary(dataset)

# Vector con los indices de las columnas con datos continuos
continuous_columns <- c(2, 3, 4, 7, 8, 11, 13, 14)

# Vector con los indices de las columnas con datos categoricos
# categorical_columns <- c(1, 9, 10)
categorical_columns <- c(1, 5, 6, 9, 10, 12, 15, 16) # Sin la columna 'date' (columna 1)

# Vector con los indices de todas las caracteristicas
features <- c(continuous_columns, categorical_columns)

# Variable con el indice de la columna clase
class_column <- 17

# Convertir columnas a numericas
dataset[, continuous_columns] <- lapply(dataset[, continuous_columns], as.numeric)

# Mapear las columnas categoricas a numericas (si son 2 categorias, se mapean a 0 y 1)
for (i in categorical_columns) {
	dataset[, i] <- sapply(dataset[, i], function(x) match(x, unique(dataset[, i])) - 1)
}

total_columns <- c(continuous_columns, categorical_columns)

# Elimninación de la columna 'NObeyesdad' (columna 17)
dataset <- dataset[, -class_column]

# Resumen de los datos
summary(dataset)

# Normalización de los datos
dataset_norm <- dataset
dataset_norm <- lapply(dataset[, total_columns], 
	function(x) (x - mean(x)) / sd(x))

# Imprimir media y desviación estándar después de la normalización
mean_values_norm <- sapply(dataset_norm, mean)
sd_values_norm <- sapply(dataset_norm, sd)

print(mean_values_norm)
print(sd_values_norm)

# for (feature in total_columns) {
# 	col_name <- colnames(dataset_norm)[feature]
# 	cat("Feature: ", col_name, "\tMean: ", mean_values_norm[col_name], "\tSD: ", sd_values_norm[col_name], "\n")
# }

# # ---------------------------------------------------------------------
# # 1. Descripción de los datos

# # --/ Para datos continuos:

# # --/--/ MAX
# max_values <- sapply(dataset[, continuous_columns], max)

# # --/--/ MIN
# min_values <- sapply(dataset[, continuous_columns], min)

# # --/--/ Promedio
# mean_values <- sapply(dataset[, continuous_columns], mean)

# # --/--/ Desviación estándar
# sd_values <- sapply(dataset[, continuous_columns], sd)


# # --/ Para datos categóricos:

# # Tabla de Frequencias de los valores de las columnas categóricas
# # y la columna de clase




# # --/--/ Moda
# mode_values <- sapply(dataset[, categorical_columns], Mode)

# # --/--/ Frecuencia de valores (y la columna de clase)
# categorical_class_columns <- c(categorical_columns, class_column)

# for (feature in categorical_class_columns) {
# 	col_name <- colnames(dataset)[feature]

# 	dataframe <- as.data.frame(table(dataset[, feature]))
# 	names(dataframe) <- c(col_name, "Freq")
# 	print(dataframe)

# 	p <- ggplot(data = dataframe, aes(x=dataframe[, 1], y=dataframe[, 2])) + 
# 		geom_bar(stat = "identity", fill="lightblue") + 
# 		labs(title = paste("Frecuencias por valores de:", col_name), x=col_name, y="Frecuencia")
# 	#x11()
# 	print(p)

# 	# --/--/ Guardar PNG con la gráfica
# 	#ggsave(paste("Frecuencias_", col_name, ".png"), plot = p, path = here("plots"))

# 	# --/--/ Imprimir descripción
# 	cat("Feature: ", col_name, "\tMode: ", mode_values[col_name], "\n")
# }
# # ---------------------------------------------------------------------

# # ---------------------------------------------------------------------
# # 2. Imputación de datos faltantes

# #  No se realizará imputación porqué no hay datos faltantes en el dataset
# # ---------------------------------------------------------------------

# # ---------------------------------------------------------------------
# # 3. Normalización de datos continuos y eliminación de valores extremos

# # --/ Copia del dataset
# dataset_norm <- dataset

# # --/ Normalización
# dataset_norm[, continuous_columns] <- lapply(dataset[, continuous_columns], 
# 	function(x) (x - mean(x)) / sd(x))

# # --/ Mostrar media y desviación estándar después de la normalización
# mean_values_norm <- sapply(dataset_norm[, continuous_columns], mean)
# sd_values_norm <- sapply(dataset_norm[, continuous_columns], sd)

# for (feature in continuous_columns) {
# 	col_name <- colnames(dataset_norm)[feature]
# 	cat("Feature: ", col_name, "\tMean: ", mean_values_norm[col_name], "\tSD: ", sd_values_norm[col_name], "\n")
# }

# # --/ Eliminación de valores extremos por medio de la media y la varianza

# # --/--/ Se eliminan filas que tengan al menos 
# #        una caracteristica con valor extremo
# dataset_norm <- dataset_norm[apply(dataset_norm[, continuous_columns], 1, 
# 	function(x) all(abs(x) < 3)), ]

# # --/--/ Imprimir cantidad de filas eliminadas por valores extremos
# cat("Filas eliminadas por valores extremos: ", nrow(dataset) - nrow(dataset_norm), "\n")


# # --/ Resumen de los datos tratados
# summary(dataset_norm)


# # --/ Histogramas de las caracteristicas continuas después de la normalización
# for (feature in continuous_columns) {
# 	col_name <- colnames(dataset_norm)[feature]
# 	p <- ggplot(dataset_norm, aes(x=dataset_norm[, feature])) + 
# 		geom_histogram(binwidth = 1, fill="lightblue", color="black") + 
# 		labs(title = paste("Histograma (normalizado) de:", 
# 			col_name), x=col_name, y="Frecuencia")
# 	x11()
# 	print(p)

# 	# --/--/ Guardar PNG con la gráfica
# 	ggsave(paste("Histograma_", col_name, ".png"), plot = p, path = here("plots"))
# }