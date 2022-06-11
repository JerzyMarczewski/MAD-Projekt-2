library(RCurl)
wine_gh <- getURL("https://raw.githubusercontent.com/JerzyMarczewski/MAD-Projekt-2/main/winequality-red.csv")
wine_data <- read.csv(text = wine_gh, header = T, sep = ';', dec = '.')

library(ggplot2)
library(dplyr)
library(tidyverse)
library(modelr)
install.packages("broom")
library(broom)
install.packages('ISLR')
library(ISLR)
install.packages('tibble')
library(tibble)

mydata <- as_tibble(ISLR::Default)
mydata

sum(is.na(mydata)) # checking for NA values

sample <- sample(c(TRUE, FALSE), nrow(mydata), replace = T, prob = c(0.6, 0.4))
train <- mydata[sample,]
test <- mydata[!sample,]

logmodel <- glm(default ~ balance, family = "binomial", data = train)

mydata %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")


summary(logmodel)

logmodel_test <- glm(default ~ balance, family = "binomial", data = test)
summary(logmodel_test)


predict(logmodel, data.frame(balance = c(5)), type = "response")





