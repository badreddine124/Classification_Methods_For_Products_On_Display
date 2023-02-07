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
# split data selon les nodes : 
split_data <- split(final_df, f = final_df$Node)
#view(split_data[[1]])


long = 16
#################################################################################################
Q <- rep(0, long)
P <- rep(0, long)
R <- rep(0, long)
S <- rep(0, long)


for (i in 1:long) {
  #split into train and test
  #use 80% of dataset as training set and 20% as test set
  sample <- sample(c(TRUE, FALSE), nrow(split_data[[i]]), replace=TRUE, prob=c(0.75,0.25))
  train  <- split_data[[i]][sample, ]
  test   <- split_data[[i]][!sample, ]
  
  
  train <- train[,-c(26)]
  test <- test[,-c(26)]
  
  train <- train[,-c(25)]
  test <- test[,-c(25)]
  
  # train <- train[,-c(22, 19)]
  # test <- test[,-c(22, 19)]
  
  # mtry <- tuneRF(train[-1],train$Y, ntreeTry=100, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)
  # best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  best.m = 15
  
  rf <- randomForest(Y~.,data= train, mtry=best.m, importance=TRUE, ntree=500)
  # varImpPlot(rf) # visualizing the importance of variables of the model.
  
  predictions <- predict(rf, newdata = test, type= "class")
  predictions <- round(predictions)
  
  
  confusion_matrix <- confusionMatrix(as.factor(predictions), as.factor(test$Y))
  
  Q[i] = confusion_matrix[["overall"]][["Accuracy"]]*100
  P[i] = confusion_matrix[["byClass"]][["F1"]]*100
  R[i] = confusion_matrix[["byClass"]][["Recall"]]*100
  S[i] = confusion_matrix[["byClass"]][["Precision"]]*100
  
  cat(" The accuracy of random forest classifier is = ", Q[i]," %", "\n")
  cat(" The F1-score of random forest classifier is = ", P[i]," %", "\n")
  cat(" The recall of random forest classifier is = ", R[i]," %", "\n")
  cat(" The Precision of random forest classifier is = ", S[i]," %", "\n")
}

cat("the accuracy is :", mean(Q), "\n") # 88.38662 %
cat("the F1 score is :", mean(P), "\n") # 88.89216 %
cat("the recall is ", mean(R), "\n") # 91.34441 %
cat("the Precision is ", mean(S), "\n") # 86.82864 %

#################################################################################################
#################################################################################################














