library(here)
library(ggplot2)
library(GGally)
library(rpart)
library(rpart.plot)

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

# ----------
# Selección de caracteristicas para el 50% y 75% de los datos
c50 <- floor(length(dataset_escalar) * 0.50)
c75 <- floor(length(dataset_escalar) * 0.75)

p50 <- forwardSelection(dataset_escalar, c50, correlationMatrix, fisherFactors, 0.5, 0.5)
print(p50$selectedFeatures)
t50<- p50$selectedFeatures


p75 <- forwardSelection(dataset_escalar, c75, correlationMatrix, fisherFactors, 0.5, 0.5)
print(p75$selectedFeatures)
t75 <- p75$selectedFeatures

# ---------------------------------------------------------------------

# Arbol de decisión con las características seleccionadas
NObeyesdad <- dataset$NObeyesdad
View(dataset)
df_tree50 <- data.frame(NObeyesdad, dataset[, t50])
df_tree75 <- data.frame(NObeyesdad, dataset[, t75])


model.classification50 <- rpart(NObeyesdad ~ ., data = df_tree50, method = "class", control = rpart.control(minsplit = 1, cp = 0.01))  

# Impresión del árbol
rpart.plot(model.classification50, extra = 1, box.palette= "GnYlRd" ,type = 5, fallen.leaves = TRUE, 
            under = TRUE, faclen = 4, cex = 1, split.cex = 1, split.prefix = " ",cex.main=1.5,
            main="Árbol de decisión con las características seleccionadas (50%)",branch.lty = 3, under.cex=1)


# Imprimimos los nombres de los nodos de model.classification50
nombres_caracteristicas <- model.classification50$frame$var
nombres_caracteristicas <- nombres_caracteristicas[nombres_caracteristicas != "<leaf>"]

# Imprimimos los valores unicos de nombres_caracteristicas
print(unique(nombres_caracteristicas))

# Arbol con las características seleccionadas al 75% parms = list(split = "information")
model.classification75 <- rpart(NObeyesdad ~ ., data = df_tree75, method = "class", control = rpart.control(minsplit = 1, cp = 0.01))

# Impresión del árbol
rpart.plot(model.classification75, extra = 1, box.palette= "GnYlRd" ,type = 5, fallen.leaves = TRUE, 
            under = TRUE, faclen = 4, cex = 1, split.cex = 1, split.prefix = " ",cex.main=1.5,
            main="Árbol de decisión con las características seleccionadas (75%)",branch.lty = 3, under.cex=1)

# Imprimimos los nombres de los nodos de model.classification75
nombres_caracteristicas <- model.classification75$frame$var
nombres_caracteristicas <- nombres_caracteristicas[nombres_caracteristicas != "<leaf>"]

# Imprimimos los valores unicos de nombres_caracteristicas
print(unique(nombres_caracteristicas))


# Arbol con todas las características
View(dataset)
model.classification <- rpart(NObeyesdad ~ ., data = dataset, method = "class", control = rpart.control(minsplit = 1, cp = 0.01))

# Impresión del árbol
rpart.plot(model.classification, extra = 1, box.palette= "GnYlRd" ,type = 5, fallen.leaves = TRUE, 
            under = TRUE, faclen = 4, cex = 1, split.cex = 1, split.prefix = " ", cex.main=1.5,
            main="Árbol de decisión con todas las características", branch.lty = 3, under.cex=1)

nombres_caracteristicas <- model.classification$frame$var
nombres_caracteristicas <- nombres_caracteristicas[nombres_caracteristicas != "<leaf>"]

# Imprimimos los valores unicos de nombres_caracteristicas
print(unique(nombres_caracteristicas))

all_features <- unique(nombres_caracteristicas)

# Guardamos las características seleccionadas en un archivo txt
write.table(all_features, "Avance5/selected_features_dtree.txt", row.names = FALSE, col.names = FALSE)