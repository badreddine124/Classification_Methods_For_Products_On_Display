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
# Load the vcd package
library(vcd)


# repertoire de travai
setwd("C:/Users/hp/OneDrive - Institut National de Statistique et d'Economie Appliquee/Desktop/app en Gr dim")

#charger les donnees
data <- data.table()
data <- read.csv(file = "data_categ.csv")
data <- data[,-c(1)]
#View(data)

#################################################################################################
#################################################################################################
data$X1 <- as.character(data$X1)
data$X2 <- as.character(data$X2)
data$X3 <- as.character(data$X3)
data$X4 <- as.character(data$X4)
data$X6 <- as.character(data$X6)


glimpse(data)
# If the p-value is less than 0.05, you can conclude that there is a significant association between the two variables.

#################################################################################################
############################ MLDPC descritisation : testing the quality #########################
#################################################################################################

for(i in 1:7){
  # Create the contingency table
  table <- table(data[,c(i)], data$Y)
  
  # Perform the chi-square test

  # print(chisq.test(table))
  print(assocstats(table)$cramer)
}


#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################

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
  
  hp<- rpart.control(minsplit=90,# effectif minimal pour s?parer un noeud
                     minbucket=90,# effectif minimal dans chaque noeud terminal 
                     maxdepth=5,# hateur maximale de l'arbre
                     cp=0)#parametre de penalisation pour la complexit?
  arbre_rpart=rpart(Y~., data[,c(1,i)],control=hp)
  library(rpart.plot)
  rpart.plot(arbre_rpart)
  #by Row number
  # view(arbre_rpart$where)
  data[,c(i)] <- as.character(arbre_rpart$where)
}

# If the p-value is less than 0.05, you can conclude that there is a significant association between the two variables.

#################################################################################################
################## Decision tree descritisation : testing the quality ###########################
#################################################################################################

for(i in 2:8){
  # Create the contingency table
  table <- table(data[,c(i)], data$Y)
  
  # Perform the chi-square test
  # print(chisq.test(table))
  print(assocstats(table)$cramer)
}


# Cramer's V is a measure of association that ranges from 0 to 1.
# with a value of 1 indicating a perfect association and a value of 0 indicating no association.
# It is calculated as the square root of the chi-square statistic divided by the total number of observations.


# MDLPC :

# [1] 0.380861 X1
# [1] 0.4229202 X2
# [1] 0.16425 X3
# [1] 0.3893416 X4
# [1] 0.4261988 X5
# [1] 0.1743091 X6
# [1] 0.5049593 X7


# decision tree:

# [1] 0.3826051 X1
# [1] 0.4291601 X2
# [1] 0.1508703 X3
# [1] 0.3689436 X4
# [1] 0.429341 X5
# [1] 0.1743091 X6
# [1] 0.5049593 X7













