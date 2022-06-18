library(RCurl)
wine_gh <- getURL("https://raw.githubusercontent.com/JerzyMarczewski/MAD-Projekt-2/main/winequality-red.csv")
mydata <- read.csv(text = wine_gh, header = T, sep = ';', dec = '.')

library(ggplot2)
library(dplyr)
library(tidyverse)
library(modelr)
library(broom)
library(ISLR)
library(tibble)


sum(is.na(mydata)) # checking for NA values


#sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, prob = c(0.6,0.4))
dat.d <- sample(mydata, nrow(mydata), replace = T)

sample <- sample(nrow(mydata$quality), nrow(mydata$quality), replace = F, prob = c(0.6, 0.4))
train <- mydata[dat.d,]
test <- mydata[-dat.d,]



logmodel <- glm(default ~ my_data$quality, family = "binomial", data = train)

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


prob_pred <- predict(logmodel, data.frame(balance = c(3,4,5,6,7,8)), type = "response")
?predict
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

cm <- table(prob_pred, y_pred > 0.5)

library(pROC)

roc_score <- roc(prob_pred, y_pred)



library(MLmetrics)
F1_Score(prob_pred, y_pred)

confint(logmodel)




