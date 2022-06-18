library(RCurl)
wine_gh <- getURL("https://raw.githubusercontent.com/JerzyMarczewski/MAD-Projekt-2/main/winequality-red.csv")
wine_data <- read.csv(text = wine_gh, header = T, sep = ';', dec = '.')


wine.subset <- wine_data[colnames(wine_data)]

str(wine.subset)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

wine.subset.norm <- as.data.frame(lapply(wine.subset[,1:11], normalize))

head(wine.subset.norm)

set.seed(123)

dat.d <- sample(1:nrow(wine.subset.norm), size=nrow(wine.subset.norm)*0.7, replace = F)

train.wine <- wine.subset[dat.d,]
test.wine <- wine.subset[-dat.d,]

train.wine_labels <- wine.subset[dat.d, 12]
test.wine_labels <- wine.subset[-dat.d, 12]

library(class)

NROW(train.wine_labels)


knn.33 <- knn(train = train.wine, test = test.wine, cl = train.wine_labels, k = 33)
knn.34 <- knn(train = train.wine, test = test.wine, cl = train.wine_labels, k = 34)

Acc.33 <- 100 * sum(test.wine_labels == knn.33)/NROW(test.wine_labels)
Acc.34 <- 100 * sum(test.wine_labels == knn.34)/NROW(test.wine_labels)

table(knn.33, test.wine_labels)
table(knn.34, test.wine_labels)


library(caret)
confusionMatrix(table(knn.33, test.wine_labels))


#i = 1
k.optm = 1
for (i in 1:34) {
  knn.mod <- knn(train = train.wine, test = test.wine, cl = train.wine_labels, k = i)
  k.optm[i] <- 100 * sum(test.wine_labels == knn.mod)/NROW(test.wine_labels)
  k=i
  cat(k,'=',k.optm[i],'
      ')
}

plot(k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy level")

knn.1 <- knn(train = train.wine, test = test.wine, cl = train.wine_labels, k = 1)

confusionMatrix(table(knn.1, test.wine_labels), mode = 'everything', positive = '1')

