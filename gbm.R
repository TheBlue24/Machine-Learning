library(gbm)
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
train$y <-ifelse(train$y==1,1,0)
test$y <-ifelse(test$y==1,1,0)

#model
gbm.model <-gbm(y~.,distribution = "bernoulli",,data=train,n.trees = 100,
                interaction.depth = 4,shrinkage = 0.30,cv.folds = 30)


summary(gbm.model)
print(gbm.model)
#NUmber of trees
n_tree_cv <-gbm.perf(gbm.model,method="cv")
print(n_tree_cv)
n_tree_oob <-gbm.perf(gbm.model,method="OOB")  
print(n_tree_oob)
#predict
prediction <-predict(gbm.model,test,n.trees = n_tree_oob,type="response")
print(auc(test$y,prediction))
#View(prediction)
prediction<-ifelse(prediction>0.60,1,0)
c_m <-table(test$y,prediction)


print(c_m)

#Accuracy
print((sum(diag(c_m))/sum(c_m))*100)

