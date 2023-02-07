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


# repertoire de travai
setwd("C:/Users/hp/OneDrive - Institut National de Statistique et d'Economie Appliquee/Desktop/app en Gr dim")

#charger les donnees
data <- data.table()
data <- read.csv(file = "data_categ.csv")
data <- data[,-c(1)]
#View(data)

#################################################################################################
# # convertir Y en nemuric
Y <- as.factor(data$Y)
Y <- unclass(Y)

#################################################################################################
data$X1 <- as.character(data$X1)
data$X2 <- as.character(data$X2)
data$X3 <- as.character(data$X3)
data$X4 <- as.character(data$X4)
data$X6 <- as.character(data$X6)

# convertir Y en numeric
Y <- as.factor(data$Y)
Y <- unclass(Y)
data$Y  <- Y-1


glimpse(data)

#################################################################################################

glimpse(data)
data<-data[,c("Y","X1","X2","X3","X4","X6","X5","X7")]

#################################################################################################
#################################################################################################
#################################################################################################
#building the model:
library(rpart)

hp<- rpart.control(minsplit=1000,# effectif minimal pour séparer un noeud
                   minbucket=1000,# effectif minimal dans chaque noeud terminal 
                   maxdepth=3,# hateur maximale de l'arbre
                   cp=0)#parametre de penalisation pour la complexité

# Build the decision tree model
model <- rpart(Y~.,
               data,control=hp)

# Print the model
#print(model)

# Plot the decision tree
rpart.plot(model)


data$Node <- as.factor(model$where)
#################################################################################################
#################################################################################################
# split data selon les nodes : 
split_data <- split(data, f = data$Node)
#################################################################################################
##############################       naiveBayes   ###############################################
#################################################################################################
#################################################################################################
LONG = 7

Q <- rep(0, LONG)
P <- rep(0, LONG)
R <- rep(0, LONG)
S <- rep(0, LONG)


for (i in 1:LONG) {
  #split into train and test:
  #make this example reproducible
  set.seed(42)
  
  
  #use 80% of dataset as training set and 20% as test set
  sample <- sample(c(TRUE, FALSE), nrow(split_data[[i]]), replace=TRUE, prob=c(0.75,0.25))
  train  <- split_data[[i]][sample, ]
  test   <- split_data[[i]][!sample, ]
  
  
  train <- train[,-c(9, 8)]
  test <- test[,-c(9, 8)]
  
  ### NaiveBayes ###
  # Train the classifier
  classifier <- naiveBayes(Y ~ ., data = train)
  # Make predictions on the test set
  predictions <- predict(classifier, test[,-c(1)])
 
  
  confusion_matrix <- confusionMatrix(as.factor(predictions), as.factor(test$Y))
  
  Q[i] = confusion_matrix[["overall"]][["Accuracy"]]*100
  P[i] = confusion_matrix[["byClass"]][["F1"]]*100
  R[i] = confusion_matrix[["byClass"]][["Recall"]]*100
  S[i] = confusion_matrix[["byClass"]][["Precision"]]*100
  
  cat(" The accuracy of naiveBayes classifier is = ", Q[i]," %", "\n")
  cat(" The F1-score of naiveBayes classifier is = ", P[i]," %", "\n")
  cat(" The recall of naiveBayes classifier is = ", R[i]," %", "\n")
  cat(" The Precision of naiveBayes classifier is = ", S[i]," %", "\n")
}

cat("the accuracy is :", mean(Q), "\n") # the accuracy is : 82.42839 %
cat("the F1 score is :", mean(P), "\n") # the F1 score is : 78.05997 %
cat("the recall is ", mean(R), "\n") # the recall is  78.2898 %
cat("the Precision is ", mean(S), "\n") # the Precision is  82.28681 %


#################################################################################################
#################################################################################################
##################################     Random forrest     #######################################
#################################################################################################

# Loading package
library(caTools)
library(ROCR) 

Q <- rep(0, LONG)
P <- rep(0, LONG)
R <- rep(0, LONG)
S <- rep(0, LONG)


for (i in 1:LONG) {
  #split into train and test:
  #make this example reproducible
  set.seed(42)
  
  
  #use 75% of dataset as training set and 25% as test set
  sample <- sample(c(TRUE, FALSE), nrow(split_data[[i]]), replace=TRUE, prob=c(0.75,0.25))
  train  <- split_data[[i]][sample, ]
  test   <- split_data[[i]][!sample, ]
  
  
  train <- train[,-c(9, 8)]
  test <- test[,-c(9, 8)]
  
  best.m = 5
  
  rf <- randomForest(Y~.,data= train, mtry=best.m, importance=TRUE, ntree=500)
  # varImpPlot(rf) # visualizing the importance of variables of the model.
  
  predictions <- predict(rf, newdata = test[,-c(1)], type= "class")
  predictions <- round(predictions)
  
  
  confusion_matrix <- confusionMatrix(as.factor(predictions), as.factor(test$Y))
  
  Q[i] = confusion_matrix[["overall"]][["Accuracy"]]*100
  P[i] = confusion_matrix[["byClass"]][["F1"]]*100
  R[i] = confusion_matrix[["byClass"]][["Recall"]]*100
  S[i] = confusion_matrix[["byClass"]][["Precision"]]*100
  
  cat(" The accuracy of Random forrest classifier is = ", Q[i]," %", "\n")
  cat(" The F1-score of Random forrest classifier is = ", P[i]," %", "\n")
  cat(" The recall of Random forrest classifier is = ", R[i]," %", "\n")
  cat(" The Precision of Random forrest classifier is = ", S[i]," %", "\n")
}

cat("the accuracy is :", mean(Q), "\n") # the accuracy is : 81.69142 %
cat("the F1 score is :", mean(P), "\n") # the F1 score is : 81.07977 %
cat("the recall is ", mean(R), "\n") # the recall is  81.50738 %
cat("the Precision is ", mean(S), "\n") # the Precision is  81.48032 %








