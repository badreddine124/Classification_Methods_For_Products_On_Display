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
# Loading package
library(caTools)
library(ROCR)


# repertoire de travai
setwd("C:/Users/hp/OneDrive - Institut National de Statistique et d'Economie Appliquee/Desktop/app en Gr dim")

#charger les donnees
data <- data.table()
data <- read.csv(file = "new_Base_CDM_balanced_V2.CSV", sep = ";")
data <- data[-c(1),]

#################################################################################################
# convertir Y en numeric
Y <- as.factor(data$Y)
Y <- unclass(Y)

# convertir X7 en nemuric
X7 <- as.factor(data$X7)
X7 <- unclass(X7)

#################################################################################################
data$X1 <- as.numeric(data$X1)
data$X2 <- as.numeric(data$X2)
data$X3 <- as.numeric(data$X3)
data$X4 <- as.numeric(data$X4)
data$X6 <- as.numeric(data$X6)
data$Y  <- Y-1
data$X7  <- X7-1

glimpse(data)
data<-data[,c("Y","X1","X2","X3","X4","X6","X5","X7")]

###########################################
for (i in 2:6) {
  
  hp<- rpart.control(minsplit=1000,# effectif minimal pour s?parer un noeud
                     minbucket=1000,# effectif minimal dans chaque noeud terminal 
                     maxdepth=5,# hateur maximale de l'arbre
                     cp=0)#parametre de penalisation pour la complexit?
  arbre_rpart=rpart(Y~., data[,c(1,i)],control=hp)
  library(rpart.plot)
  rpart.plot(arbre_rpart)
  #by Row number
  # view(arbre_rpart$where)
  data[,c(i)] <- as.character(arbre_rpart$where)
}

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

cat("the accuracy is :", mean(Q), "\n") # the accuracy is : 82.55675 %
cat("the F1 score is :", mean(P), "\n") # the F1 score is : 79.12049 %
cat("the recall is ", mean(R), "\n") # the recall is  79.25106 %
cat("the Precision is ", mean(S), "\n") # the Precision is  81.8591 %








#################################################################################################
#################################################################################################
#################################################################################################
##################################     Random forrest     #######################################
#################################################################################################
#################################################################################################
#################################################################################################

#charger les donnees
data <- data.table()
data <- read.csv(file = "new_Base_CDM_balanced_V2.CSV", sep = ";")
data <- data[-c(1),]

#################################################################################################
# convertir Y en numeric
Y <- as.factor(data$Y)
Y <- unclass(Y)

# convertir X7 en nemuric
X7 <- as.factor(data$X7)
X7 <- unclass(X7)

#################################################################################################
data$X1 <- as.numeric(data$X1)
data$X2 <- as.numeric(data$X2)
data$X3 <- as.numeric(data$X3)
data$X4 <- as.numeric(data$X4)
data$X6 <- as.numeric(data$X6)
data$Y  <- Y-1
data$X7  <- X7-1

glimpse(data)
data<-data[,c("Y","X1","X2","X3","X4","X6","X5","X7")]

###########################################
for (i in 2:6) {
  
  hp<- rpart.control(minsplit=100,# effectif minimal pour s?parer un noeud
                     minbucket=100,# effectif minimal dans chaque noeud terminal 
                     maxdepth=8,# hateur maximale de l'arbre
                     cp=0)#parametre de penalisation pour la complexit?
  arbre_rpart=rpart(Y~., data[,c(1,i)],control=hp)
  library(rpart.plot)
  rpart.plot(arbre_rpart)
  #by Row number
  # view(arbre_rpart$where)
  data[,c(i)] <- as.character(arbre_rpart$where)
}

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
                   maxdepth=2,# hateur maximale de l'arbre
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

LONG = 4


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

cat("the accuracy is :", mean(Q), "\n") # the accuracy is : 83.90617  %
cat("the F1 score is :", mean(P), "\n") # the F1 score is : 83.23388 %
cat("the recall is ", mean(R), "\n") # the recall is  84.77523 %
cat("the Precision is ", mean(S), "\n") # the Precision is  82.23531 %








