library(here)
library(DescTools)
library(ggplot2)
library(cluster)
library(e1071)


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
	col_name <- colnames(dataset)[feature]

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
dataset_norm[, continuous_columns] <- lapply(dataset[, continuous_columns], 
	function(x) (x - mean(x)) / sd(x))