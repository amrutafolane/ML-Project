
require("e1071")
train <- read.csv("C:/Users/nileshpharate/Desktop/train.csv",header = TRUE,
                  sep = ",",quote = "\"");

train<-train[complete.cases(train),];
trainIndex <- sample(1:nrow(train), 0.1 * nrow(train));
iristrain <- train[trainIndex, ];
iristest <- train[-trainIndex, ];
iristest1 <- sample(1:nrow(iristest), 0.1 * nrow(iristest));
iristtestdata <- iristest[iristest1, ];

obj <-tune.svm(status_group ~ ., data = iristrain, sampling = "fix",
               gamma = 2^c(-8,-4,0,4), cost = 2^c(-8,-4,-2,0));

obj;

model <- svm(iristrain$status_group ~ ., data=iristrain,
             method="C-classification", kernel="sigmoid",
             probability=T,gamma=0.0002, cost=1);


prediction <- predict(model, iristtestdata)
dtt<-table(iristtestdata$status_group, prediction)
sum(diag(dtt))/sum(dtt)
