library(e1071)
train <- read.csv("C:\\Users\\admin\\Desktop\\Studies\\Machine Learning\\Project\\training.csv")
train<-train[complete.cases(train),]
trainIndex <- sample(1:nrow(train), 0.8 * nrow(train))
iristrain <- train[trainIndex, ]
iristest <- train[-trainIndex, ]

model <- svm(iristrain$status_group ~ ., data=iristrain, method="C-classification", kernel="sigmoid", probability=T, gamma=0.0002, cost=100000)
prediction <- predict(model, iristest)
dtt<-table(iristest$status_group, prediction)
sum(diag(dtt))/sum(dtt)
