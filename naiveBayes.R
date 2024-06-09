library(naivebayes)
library(caTools)
library(datasets)
library(dplyr)

summary(iris)

# Normalización
irisNorm <- as.data.frame(scale(iris[,1:4]))

# Agregamos columna de clases
irisNorm$Species <- iris$Species

summary(irisNorm)

# Shuffle the data-order
set.seed(42)
irisNorm <- irisNorm[sample(nrow(irisNorm)),]
View(irisNorm)

# Create k = 5 equally size folds
k <- 5
folds <- cut(seq(1, nrow(irisNorm)), breaks = k, labels = FALSE)
cmList <- list()

# Perform K fold cross validation
for (i in 1:k){
  
  # Segment data by fold using which() function
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- irisNorm[testIndexes, ]
  trainData <- irisNorm[-testIndexes, ]
  
  ppriori <- table(trainData$Species)
  ppriori <- ppriori / sum(ppriori)
  #print(ppriori)
  
  classiffier_gnb <- gaussian_naive_bayes(
    x = as.matrix(trainData[,names(irisNorm[,1:4])]),
    y = trainData$Species,
    levels = levels(trainData$Species), 
    prior = ppriori)
  
  newClass <- predict(classiffier_gnb, 
                      newdata = as.matrix(testData[,names(
                        irisNorm[,1:4])]),
                      type = "class")
  
  #print("newClass: ", newClass)
  print("\n")
  
  clase_true <- testData$Species
  clase_pred <- newClass
  cm <- table(clase_true, clase_pred)
  
  #print(cm)
  cmList[[i]] <- cm
}

class <- levels(iris$Species)
print(class)

i <- 1
for (cm in cmList){
  cat("Fold", i, "\n")
  
  # Accuracy
  accuracy <- sum(diag(cm))/sum(cm)
  cat("Excatitud: ", accuracy, "\n\n")
  
  for (c in class){
    # Obtenemos los elementos de la diagonal principal (True +)
    TP <- cm[c,c]
    FP <- sum(cm[c,]) - TP
    FN <- sum(cm[,c]) - TP
    
    cat("presición-", c, ":",TP/(TP+FN), "\n")
    
  }
  
  i <- i+1
}