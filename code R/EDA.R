library(data.table)
library(tidyverse)
library(Hmisc)
library(MASS)
library(ggplot2)
library(ggpubr)
library(discretization)
# volume de sales nombre de l vendu (en unité de produit)
# valeur in volume nombre d'unités de produits vendu 
# chiffre d'affaire magazin (indep du produit)
# value : valeur vendu 
# enseigne : magazin 
# vente converti(prix total vendu du produit) = value(prix du produit) *cor_sales in volume(quantité)
# feature promo ou non
#volume is a count of sales and value is a total sum of the sales value.
#volume is quantity and value is its worth
#Sales by value means sales in monetary terms, 
#Sales by volume means just the number of units sold, say, 20 air conditioners.
# cor_sales_in_val( ce qu'on a gagné au total)/cor_sales_in_vol(ce qu'on a vendu en unité) < value(prix de vente)
# la division des deux ( ce qu'on a gagné par unité) <(prix de vente d" l'unité)
#  ce qu'on a gagné par unité=prix de vente d" l'unité-prix d'achat à partir du fourniseur


path2data <- file.path("C:/Users/dell/Desktop/moussanniiif_2")

#la data contient 5 variables continu et 2 vars qualitatif


setwd(path2data)
data<- data.table()
data<-read.csv2("new_Base_CDM_balanced_V2.csv", sep=";" , header=TRUE)
data<- data[-c(1),]


data$X1 <- as.numeric(data$X1)
data$X2 <- as.numeric(data$X2)
data$X3 <- as.numeric(data$X3)
data$X4 <- as.numeric(data$X4)
data$X6 <- as.numeric(data$X6)
data$Y <- as.factor(data$Y)
data$X7 <- as.factor(data$X7)
data$X5 <- as.factor(data$X5)


# Shuffle the rows of the data frame using sample
data <- data[sample(nrow(data)), ]

glimpse(data)

data$X8 <- data$X1*data$X4
data$X9 <- data$X2/data$X1
data$X10  <- data$X4 - data$X9
# 
# # convertir Y en numeric
# Y <- as.factor(data$Y)
# Y <- unclass(Y)
# data$Y  <- Y
#######################################################################################"
theme_set(theme_pubr())
ggplot(data, aes(X7)) +geom_bar(fill = c("#DB745E","#46639A")) +theme_pubclean()
## pourcentage of displ no displ in feat no feat 
ggplot(data, aes(Y)) +geom_bar(fill=c("#DB745E","#46639A")) +theme_pubclean()+scale_fill_brewer(palette = "Set1")

#####################################

ggplot(data, aes(X8, X6, color = Y)) + 
  geom_point(alpha = 0.5) +
  theme_bw()+labs(title = " VenteConv = cor_sales_in_vol * value")


ggplot(data, aes(X4, X2/X1, color = Y)) + 
  geom_point(alpha = 0.5) +
  theme_bw()+labs(title = " le profit par rapport au prix de vente")


ggplot(data, aes(X4, X10, color = Y)) + 
  geom_point(alpha = 0.5) +
  theme_bw()+labs(title = " le prix d'achat par rapport au prix de vente")


###################################"

library("ggplot")
library(dplyr)
library(tidyr)
df3 <- data %>% 
  group_by(X7, Y) %>% 
  tally() %>% 
  complete(Y, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)

ggplot(df3, aes(X7, percentage, fill = Y)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw()

########################"


library(ggplot2)
ddd<-data
ddd$X5 <- factor(ddd$X5, levels = rev(names(sort(table(ddd$X5), decreasing = FALSE))))

ggplot(data = ddd, aes(x = X5, fill = Y)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(type = "seq", palette = "Set2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = " la distribution des produits par type de Magazin")

################################""
ggplot(data, aes(X5, X3, fill = Y)) + 
  geom_boxplot() +
  scale_fill_brewer(type="seq",palette = "Dark2")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = " la distribution des chiffre d'affaires par type de Magazin")


############################""
ggplot(data, aes(X4, X6, color = Y)) +
  geom_point(alpha = 0.5)+
  ggtitle("les produits display ont un nombre de vente tres elevé")

# Create the scatter plot using ggplot and geom_point
ggplot(data, aes(X3, X1, color = Y)) +
  geom_point(alpha = 0.4) + 
  scale_color_brewer(type = "qual", palette = "Set1") + 
  labs(title = "My Scatter Plot", x = "value", y = "prix total vendu")

# Create the scatter plot using ggplot and geom_point
ggplot(data, aes(X3, X1, color = Y)) +
  geom_point(alpha = 0.4) + 
  scale_color_brewer(type = "qual", palette = "Set1")+
  labs(title = "nombre d'unités vendus par CA", x = "CA Magazin", y = "Cor_sales in volume")




####################################""


# convertir Y en numeric
Y <- as.factor(data$Y)
Y <- unclass(Y)
data$Y  <- Y
data$Y  <- as.character(Y-1)

#split data frame based on particular column value
df1 <- data[data$Y == 1, ]
df1 <- df1[df1$X6 > 10000, ]

df2 <- data[data$Y != 1, ]
df2 <- df2[df2$X6 > 10000, ]

plot(df1$X6, df1$X8, main="df2$X6 > 10000",xlab="ventes Conv", ylab="cor_sales_in_vol * value", col="blue", cex=1, pch = 2)
points(df2$X6, df2$X8, col="red", cex=1, pch = 10)

#######################################################"

# Create a  plot for X2
plot(Y, X2, main = "plot : X2 en fonction de Y") + 
  abline(h=1800, col = "red")

###################################
#################################################################################################

galtonskew.proc <- function(x){
  #
  #  Compute Galton's skewness measure for x
  #  NOTE: this procedure assumes no x values are missing
  #
  quarts <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))
  num <- quarts[1] + quarts[3] - 2*quarts[2]
  denom <- quarts[3] - quarts[1]
  gskew <- num/denom
  gskew
}

galtonskew.proc(data$X1)
galtonskew.proc(data$X2)
galtonskew.proc(data$X3)
galtonskew.proc(data$X4)
galtonskew.proc(data$X6)



# histograms:

hist(data$X1, breaks=200, main="histogram X1",xlim=c(0,300))
hist(log(data$X1), breaks=200,main="histogram log(X1)") 


hist(data$X2, breaks=200, main="histogram X2",xlim=c(0,1500))
hist(log(data$X2), breaks=200,main="histogram log(X2)") 


hist(data$X3, breaks=200, main="histogram X3")


hist(data$X4, breaks=200, main="histogram X4")

hist(data$X6, breaks=200, main="histogram X6",xlim=c(0,10000))
hist(log(data$X6), breaks=200,main="histogram log(X6)") 


#################################################################################################
# chisq test : 
table(data$Y, data$X7)
chisq.test(data$Y, data$X7)
# We have a high chi-squared value and a p-value of less than 0.05 significance level.
# So we reject the null hypothesis and conclude that X7 and Y have a significant relationship.

chisq.test(data$X5, data$Y)
# We have a high chi-squared value and a p-value of less than 0.05 significance level.
# So we reject the null hypothesis and conclude that X5 and Y have a significant relationship.

chisq.test(data$X5, data$X7)
# We have a high chi-squared value and a p-value of less than 0.05 significance level.
# So we reject the null hypothesis and conclude that X7 and X5 have a significant relationship.
#################################################################################################

