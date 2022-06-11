install.packages("e1071")
install.packages("factoextra")
install.packages("cluster")
install.packages("tidyr")
install.packages("ggplot2")
library(tidyr)
library(ggplot2)

library(RCurl)


# pobieranie danych
wine_gh <- getURL("https://raw.githubusercontent.com/JerzyMarczewski/MAD-Projekt-2/main/winequality-red.csv")
wine_data <- read.csv(text = wine_gh, header = T, sep = ';', dec = '.')

# usuwamy puste wiersze 
wine_data <- na.omit(wine_data)

# srednia | mediana  | min | max | odchylenie standardowe | skośność
sapply(wine_data, mean)
sapply(wine_data, median)
sapply(wine_data, min)
sapply(wine_data, max)
sapply(wine_data, sd)
sapply(wine_data, skewness)

# standaryzujemy dane
stand_wine_data <- scale(wine_data)
stand_wine_data




###########################


PimaIndiansDiabetes %>%
  ggplot(aes(x = glucose)) +
  geom_histogram()


wine_data %>%
  pivot_longer(cols = setdiff(colnames(wine_data), 'White wine')) %>%
  ggplot(aes(x = value, group = wine_data, fill = wine_data)) +
  geom_histogram(position = 'identity', alpha = 0.5) +
  facet_wrap(~name, scales = 'free')






##########################



data("PimaIndiansDiabetes")
Y <- PimaIndiansDiabetes$diabetes
PimaIndiansDiabetes$diabetes <- NULL


## Skalowanie
install.packages("caret")
library(caret)

pp <- preProcess(wine_data, method = "range")

my_set <- predict(pp, wine_data)

boxplot(my_set)


## Obs. odstajace

my_scaled_set <- my_set

for (i in colnames(my_scaled_set)) {
  print(i)
  a <- my_scaled_set[,i]
  b <- boxplot(my_scaled_set, plot = F)
  
  #lowerwhisker <- b$stats[1]
  #upperwhisker <- b$stats[5]
  
  #a[a>upperwhisker] <- upperwhisker
  #a[a<upperwhisker] <- lowerwhisker
  
  my_scaled_set[,i] <- a
}


boxplot(my_scaled_set)


## korelacja

cor(my_scaled_set)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(my_scaled_set)

findCorrelation(cor(my_scaled_set), cutoff = 0.5, names = T) -> to_exclude # funckja zwraca numer kolumny, ktora spelnia korelacje bez names = T

my_scaled_set[,to_exclude] <- NULL

chart.Correlation(my_scaled_set)




################################
#LDA
install.packages("klaR")
library(klaR)
install.packages("psych")
library(psych)
install.packages("MASS")
library(MASS)
install.packages("ggord")
library(ggord)
install.packages("devtools")
library(devtools)

library(ggplot2)

stand_wine_data <- scale(wine_data[1:11])


apply(stand_wine_data, 2, sd)

set.seed(123)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(iris), replace=TRUE, prob=c(0.7,0.3))
train <- wine_data[sample, ]
test <- wine_data[!sample, ] 

model <- lda(quality~., data = train)
model

predicted <- predict(model, test)
predicted

lda_plot <- cbind(train, predict(model)$x)
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = quality))

