# ajoter bib
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(MASS)
library(Hmisc)
library(caret)
library(randomForest)
library(rpart.plot)
library(e1071)
library(xgboost)
library(ada)


#make this example reproducible
set.seed(42)


# repertoire de travai
setwd("C:/Users/hp/OneDrive - Institut National de Statistique et d'Economie Appliquee/Desktop/app en Gr dim")

#charger les donnees
data <- data.table()
data <- read.csv(file = "new_Base_CDM_balanced_V2.CSV", sep = ";")
data <- data[-c(1),]
#View(data)

#################################################################################################
# convertir Y en nemuric
Y <- as.factor(data$Y)
Y <- unclass(Y)

# convertir X7 en nemuric
X7 <- as.factor(data$X7)
X7 <- unclass(X7)

#################################################################################################
data$X1 <- log(as.numeric(data$X1))
data$X2 <- log(as.numeric(data$X2))
data$X3 <- as.numeric(data$X3)
data$X4 <- as.numeric(data$X4)
data$X6 <- log(as.numeric(data$X6))
data$Y  <- Y-1
data$X7  <- X7-1

glimpse(data)

#################################################################################################
#################################################################################################
#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=data)

#perform one-hot encoding on data frame
final_df <- data.frame(predict(dummy, newdata=data))

#view final data frame
final_df <- final_df[,-c(6)]

#################################################################################################
#################################################################################################

#################################################################################################
#building the model:
library(rpart)

hp<- rpart.control(minsplit=500,# effectif minimal pour séparer un noeud
                   minbucket=500,# effectif minimal dans chaque noeud terminal 
                   maxdepth=5,# hateur maximale de l'arbre
                   cp=0)#parametre de penalisation pour la complexité

# Build the decision tree model
model <- rpart(Y~.,
               final_df,control=hp)

# Print the model
#print(model)

# Plot the decision tree
rpart.plot(model)


final_df$Node <- as.factor(model$where)
#################################################################################################
#################################################################################################
# split data selon les nodes : 
split_data <- split(final_df, f = final_df$Node)
#view(split_data[[1]])
data_node_1  <- split_data[[1]]

#################################################################################################
LONG = 16

Q <- rep(0, LONG)
P <- rep(0, LONG)
R <- rep(0, LONG)
S <- rep(0, LONG)


for (i in 1:LONG) {
  #split into train and test:
  #make this example reproducible
  set.seed(42)

  
  # Split the dataset into training and testing sets
  train_idx <- sample(1:nrow(split_data[[i]]), 0.75*nrow(split_data[[i]]))
  train_data <- split_data[[i]][train_idx, 2:24]
  train_label <- split_data[[i]][train_idx, 1]
  test_data <- split_data[[i]][-train_idx, 2:24]
  test_label <- split_data[[i]][-train_idx, 1]
  
  # Train the AdaBoost model
  model <- ada(train_data, train_label, loss = "exponential", iter = 200)
  
  # Make predictions on the test set
  predictions <- predict(model, test_data)
  
  # predictions <- round(predictions)
  
  confusion_matrix <- confusionMatrix(as.factor(predictions), as.factor(test_label))
  
  Q[i] = confusion_matrix[["overall"]][["Accuracy"]]*100
  P[i] = confusion_matrix[["byClass"]][["F1"]]*100
  R[i] = confusion_matrix[["byClass"]][["Recall"]]*100
  S[i] = confusion_matrix[["byClass"]][["Precision"]]*100
  
  cat(" The accuracy of adaboost classifier is = ", Q[i]," %", "\n")
  cat(" The F1-score of adaboost classifier is = ", P[i]," %", "\n")
  cat(" The recall of adaboost classifier is = ", R[i]," %", "\n")
  cat(" The Precision of adaboost classifier is = ", S[i]," %", "\n")
}

cat("the accuracy is :", mean(Q), "\n") # the accuracy is : 86.33496 %
cat("the F1 score is :", mean(P), "\n") # the F1 score is : 84.99065 %
cat("the recall is ", mean(R), "\n") # the recall is  87.09625 %
cat("the Precision is ", mean(S), "\n") # the Precision is  86.25086 %



















































