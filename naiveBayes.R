library(e1071)
#load the data
data <-read.csv2("bank-additional-full.csv",sep=";")
# To see the structure
str(data)
head(data)
must_convert<-sapply(data,is.factor)       
M2<-sapply(data[,!must_convert],is.factor)
data$age <-as.factor(data$age)
data$duration <-as.factor(data$duration)
data$campaign<-as.factor(data$campaign)
data$pdays <as.factor(data$pdays)
data$previous <-as.factor(data$previous)
#To omit NA
data <- na.omit(data)
#decision Tree
library(caTools) # for spliting data into test and train

set.seed(101)
split <- sample.split(data$y,SplitRatio=0.75)
train <- subset(data,split==T)
test <- subset(data,split==F)


#model
model <- naiveBayes(y~.,train)
summary(model)  
model
#predict
prediction <-predict(model,test)
#View(prediction)
print(prediction)
c_m <-table(prediction,test$y)

print(c_m)
#Accuracy
print((sum(diag(c_m))/sum(c_m))*100)

