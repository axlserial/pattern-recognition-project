library(here)

# Establecer directorio de trabajo (raíz del proyecto)
set_here(path='..')

# Ruta al dataset
dataset_path <- here("Datasets", "Avance1_A.csv")

# Cargar dataset
dataset <- read.table(dataset_path, header = TRUE, sep = ",")
summary(dataset)