library(here)

# # Establecer directorio de trabajo 
# # (solo si el directorio actual no es la raíz del proyecto)
# set_here(path='..')

# Ruta al dataset
dataset_path <- here("Datasets", "Avance1_A.csv")

# Cargar dataset
dataset <- read.table(dataset_path, header = TRUE, sep = ",")
summary(dataset)