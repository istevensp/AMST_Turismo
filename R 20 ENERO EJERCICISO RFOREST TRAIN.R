getwd()
setwd("C:/Users/spmx/Documents/ESPOL/Master CentroEstadística/RScripts")
getwd()

# Activate the R packages
#install.packages("dplyr")
#install.packages("faux")
#install.packages("DataExplorer")
#install.packages("randomForest")

library("dplyr")
* library("faux")
* library("DataExplorer")
library("caret") #Aquí está la matriz de confusión
library("randomForest")
?faux

file <- "https://raw.githubusercontent.com/okanbulut/tds/main/feature_selection_rfe/heart.csv"
df <- read.csv(file, header = TRUE)
fix(df)
summary(df)
head(df)

#Convertir variables categóricas en factores
df <- df %>%
  # Save categorical features as factors
  mutate_at(c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal", "target"), as.factor) %>%
  # Center and scale numeric features
  mutate_if(is.numeric, scale)

#Resumen de df
summary(df)

#Para ver df sin utilizar fix(), pero no editable
View(df)

#Randomforest con cross validation

# Número de repeticiones
num_repts <- 5
# Número de folds en la validación cruzada
num_folds <- 10

# Crear un objeto de control para la validación cruzada

# repeatedcv***
#control <- trainControl(method = "cv", number = num_folds)

control <- trainControl(method = "repeatedcv", number = num_folds, repeats=num_repts)


# Entrenar el modelo con validación cruzada

model <- train(target ~ ., data = df, method = "rf", trControl = control)
summary(model)
resultados <- model$results
print(resultados)
predictions <- predict(model)
print(predictions)
confusionMatrix(predictions, df$target)

importancia_variables <- varImp(model)


# Mostrar la importancia de las variables

print(importancia_variables)


# Supongamos que 'resultados' es el objeto que contiene tus resultados
best_accuracy <- resultados[resultados$Accuracy == max(resultados$Accuracy), ]
best_kappa <- resultados[resultados$Kappa == max(resultados$Kappa), ]

# Imprimir la fila con el mejor rendimiento en términos de Accuracy
print(best_accuracy_row)
print(best_kappa)


?train

