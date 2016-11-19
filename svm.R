library(e1071)
data("iris")
trainIndex <- sample(1:nrow(iris), 0.8 * nrow(iris))
iristrain <- iris[trainIndex, ]
iristest <- iris[-trainIndex, ]

model <- svm(Species~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iristrain, method="C-classification", kernel="sigmoid", probability=T, gamma=0.0002, cost=100000)
prediction <- predict(model, iristest)
dtt<-table(iristest$Species, prediction)
sum(diag(dtt))/sum(dtt)