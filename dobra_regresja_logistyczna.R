library(mlbench)
library(dplyr)
library(caret)
library(pROC)
library(e1071)

library(RCurl)
wine_gh <- getURL("https://raw.githubusercontent.com/JerzyMarczewski/MAD-Projekt-2/main/winequality-red.csv")
wine_data <- read.csv(text = wine_gh, header = T, sep = ';', dec = '.')

pp <- preProcess(wine_data, method = "range")

Y <- wine_data$quality
wine_data$quality <- NULL

Y <- Y/10

my_scaled_set <- scale(wine_data)
my_scaled_set <- cbind.data.frame(my_scaled_set, Y)

for (i in 1:100) {
  train <- 
    my_scaled_set %>%
    sample_frac(0.7)

  test <- setdiff(my_scaled_set, train)
  
  my_logit <- glm(Y~., family = binomial, data = train)
  my_test_score <- predict(my_logit, test[,1:11], type = "response")
  auc_compl <- auc(test$Y, my_test_score)[1]
  
  my_row <-
    data.frame(
      experiment_num = i,
      auc_compl = auc_compl
    )
  
  if (i == 1) {
    my_results <- my_row
    
  } else {
    my_results <- rbind.data.frame(my_results,my_row)
    
  }
  print(i)
}

boxplot(my_results[,-1])

