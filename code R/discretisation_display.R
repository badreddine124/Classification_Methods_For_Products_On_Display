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
library(discretization)


# repertoire de travai
setwd("C:/Users/hp/OneDrive - Institut National de Statistique et d'Economie Appliquee/Desktop/app en Gr dim")

#charger les donnees
data <- data.table()
data <- read.csv(file = "new_Base_CDM_balanced_V2.CSV", sep = ";")
data <- data[-c(1),]
#################################################################################################
# # convertir Y en nemuric
# Y <- as.factor(data$Y)
# Y <- unclass(Y)
# 
# # convertir X7 en nemuric
# X7 <- as.factor(data$X7)
# X7 <- unclass(X7)
#################################################################################################
data$X1 <- as.numeric(data$X1)
data$X2 <- as.numeric(data$X2)
data$X3 <- as.numeric(data$X3)
data$X4 <- as.numeric(data$X4)
data$X6 <- as.numeric(data$X6)
# data$Y  <- Y-1
# data$X7  <- X7-1
#################################################################################################
df = data.table()

df$X1 <- as.numeric(data$X1)
df$X2 <- as.numeric(data$X2)
df$X3 <- as.numeric(data$X3)
df$X4 <- as.numeric(data$X4)
df$X6 <- as.numeric(data$X6)
df$Y <- as.factor(data$Y)

# Select the continuous variables to discretize
df <- df[,c("X1", "X2", "X3", "X4", "X6", "Y")]

glimpse(df)

# Discretize the continuous variables using the mdlp method
# data_disc <- mdlp(df)$Disc.data

data_disc$X5 <- data$X5
data_disc$X7 <- data$X7

data_disc <- data_disc[,c("X1", "X2", "X3", "X4","X5", "X6","X7", "Y")]

# Save the data frame to a file in CSV format
# write.csv(data_disc, "data_categ.csv", row.names=TRUE)

#################################################################################################




#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################






