library(here)
library(ggplot2)
library(GGally)

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
    
    seledtedFeatures <- c()

    # Primera característica seleccionada

    # indice de la característica con mayor factor de Fisher
    s1 <- which.max(fisherFactors)
    s1nn <- names(fisherFactors)[s1]

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

		# Obtenemos la mejor caracteristica
		sk_index <- which.max(sk_values)
		sk <- names(sk_values)[sk_index]

		# Agregamos la característica a la lista de características seleccionadas
		seledtedFeatures <- c(seledtedFeatures, sk)
	}

    return(seledtedFeatures)
}
# ---------------------------------------------------------------------

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
# ---------------------------------------------------------------------
# 1. Preprocesamiento de datos

# Convertir columnas a numericas
dataset[, continuous_columns] <- lapply(dataset[, continuous_columns], as.numeric)

# Mapear las columnas categoricas a numericas (si son 2 categorias, se mapean a 0 y 1)
for (i in categorical_columns) {
	dataset[, i] <- sapply(dataset[, i], function(x) match(x, unique(dataset[, i])) - 1)
}

# Normalizar los datos, sin considerar la columna 'NObeyesdad'
dataset[, -class_column] <- scale(dataset[, -class_column])

# Numero de registros
n <- nrow(dataset)

# Eliminar valores extremos, sin considerar la columna 'NObeyesdad'
dataset <- dataset[apply(dataset[, -class_column], 1, function(x) all(abs(x) < 3)), ]

eliminados <- n - nrow(dataset)

cat("De los", n, "registros, se eliminaron", eliminados, "y quedaron", nrow(dataset), "registros\n")

# ---------------------------------------------------------------------

# 2. Selección individual (características numéricas), con factor de Fisher

# Media global de cada característica
meansGlobalClass <- colMeans(dataset[, -class_column])

# Calcular factor de Fisher para cada característica, indicando el nombre de la característica
fisherFactors <- sapply(1:ncol(dataset), function(x) fisherFactor(dataset, class_column, meansGlobalClass, x))

# Imprimimos nombre de las características y su factor de Fisher
cat("Características y su factor de Fisher:\n")
for (i in 1:length(fisherFactors)) {
    cat(names[i], ": ", fisherFactors[i], "\n")
}

# Características con factor de Fisher NaN (no se pudo calcular)
nanFeatures <- names(dataset[is.nan(fisherFactors)])
print(nanFeatures)

# Agregamos los nombres de las características a fisherFactors
names(fisherFactors) <- names

# Ordenamos de mayor a menor
fisherFactors <- sort(fisherFactors, decreasing = TRUE)

# Graficamos los factores de Fisher agregando el valor de cada característica 
g1 <- ggplot(data = data.frame(names = names(fisherFactors), fisherFactors = fisherFactors), aes(x = reorder(names, fisherFactors), y = fisherFactors)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Factor de Fisher de las características", x = "Características", y = "Factor de Fisher") +
    geom_text(aes(label = round(fisherFactors, 2)), vjust = -0.5, size = 3)

# Guardamos el gráfico anterior
ggsave("plots/fisher_factors.png", g1)

# ---------------------------------------------------------------------

# 3. Selección de subconjuntos de características por el metodo Escalar hacia delante

# Dataset con las características que no presentan problemas ("SMOKE","SCC") y la columna  de la clase
dataset_escalar <- dataset[,-c(which(sapply(dataset[,-class_column], sd) == 0), class_column)]

# Correlación de Pearson entre las características
correlationMatrix <- round(cor(dataset_escalar, method = "pearson"), 2)
View(correlationMatrix)

# Graficamos la matriz de correlación de datos
g2 <- ggcorr(correlationMatrix, label = TRUE, label_size = 3, color = "grey50", nbreaks = 6) +
    labs(title = "Correlación de los datos", center = "center")

# Guardamos el gráfico anterior
ggsave("plots/correlation_data.png", g2)


p1 <- forwardSelection(dataset_escalar, 5, correlationMatrix, fisherFactors, 0.1, 0.1)
print(p1)