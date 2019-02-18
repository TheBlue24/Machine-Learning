#load the data
data <-read.csv2("bank-additional-full.csv",sep=";")
# To see the structure
str(data)
head(data)
#To omit NA
data <- na.omit(data)
#decision Tree
library(caTools) # for spliting data into test and train
library(rpart) # fro decision tree
set.seed(101)
split <- sample.split(data$y,SplitRatio=0.75)
train <- subset(data,split==T)
test <- subset(data,split==F)
#model
rpart.model <- rpart(y~.,train)
summary(rpart.model)
plot(rpart.model)
#predict
prediction <-predict(rpart.model,test)
View(prediction)
prediction <-as.data.frame(prediction)
result <-ifelse(prediction$no > prediction$yes,"no","yes")
View(result)
#confusion matrix
c_m <- table(test$y,result)
print(c_m)
#Accuracy
print((sum(diag(c_m))/sum(c_m))*100)
