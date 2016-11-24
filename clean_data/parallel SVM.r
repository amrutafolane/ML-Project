
library(parallelSVM)
library(doParallel)

train <- read.csv("C:/Users/nileshpharate/Documents/GitHub/ML-Project/data/clean_data/training.csv",na.strings=c("", "NA"))
test  <- read.csv("C:/Users/nileshpharate/Documents/GitHub/ML-Project/data/normalized/testing.csv",na.strings=c("", "NA"))

x<-train[complete.cases(train),];
str(x)
train[!complete.cases(train),]

train <- na.omit(train)

train <- train[!apply(is.na(train) | train == " ", 1, all),]

cores<-detectCores()
#Create cluster with desired number of cores, leave one open for the machine         
#core processes
cl <- makeCluster(cores[1]-1)
#Register cluster
registerDoParallel(cl)

x1<- subset(train, select = -status_group)
y<- train$status_group

apply(x1, MARGIN = 2, FUN = function(x) sum(is.na(x)))

parallel_svm <- parallelSVM(x1, y , na.action = na.omit)

