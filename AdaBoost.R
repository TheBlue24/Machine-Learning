library(fastAdaboost)
#load the data
data <-read.csv2("bank-additional-full.csv",sep=";")
# To see the structure
str(data)
head(data)
must_convert<-sapply(data,is.factor)       
M2<-sapply(data[,must_convert],unclass)    
data<-cbind(data[,!must_convert],M2)
#To omit NA
data <- na.omit(data)
#decision Tree
library(caTools) # for spliting data into test and train

set.seed(101)
split <- sample.split(data$y,SplitRatio=0.75)
train <- subset(data,split==T)
test <- subset(data,split==F)


#model
ada <- adaboost(y~.,train,100)
  
#predict
prediction <-predict(ada,test)
#View(prediction)
print(prediction$error)
c_m <-table(prediction$class,test$y)

print(c_m)
#Accuracy
print((sum(diag(c_m))/sum(c_m))*100)

