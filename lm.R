#library
library("lattice") # for viualization
library("dplyr") #for manipulation
library("corpcor") # for partial correlation
library("MASS") # for step AIC value
# read the data
raw_data <- read.csv("data.csv")

#normalize the data 
m_data <- raw_data[1:nrow(raw_data),3:ncol(raw_data)]
data <- as.data.frame(m_data)
attach(data)
View(data)
#box plot for checking whether the data is normalized (EDA)

#Almost most of the input variables has outliers
boxplot(price,col="blue",horizontal = T) 
boxplot(bathrooms,col="blue",horizontal = T) 
boxplot(sqft_living,col="blue",horizontal = T) 
boxplot(sqft_lot,col="blue",horizontal = T)
boxplot(floors,col="blue",horizontal = T)
boxplot(waterfront,col="blue",horizontal = T)
boxplot(view,col="blue",horizontal = T)
boxplot(condition,col="blue",horizontal = T)
boxplot(grade,col="blue",horizontal = T)
boxplot(sqrt_above,col="blue",horizontal = T)
boxplot(sqft_basement,col="blue",horizontal = T)
boxplot(yr_built,col="blue",horizontal = T)
boxplot(yr_renovated,col="blue",horizontal = T)
boxplot(zipcode,col="blue",horizontal = T)
boxplot(sqft_living15,col="blue",horizontal = T)
boxplot(sqft_lot15,col="blue",horizontal = T)

#scatter plot
#linearity
plot(bathrooms,price) # somewhat linear
plot(sqft_above,price) #linear
plot(sqft_basement,price) # linear
plot(sqft_living,price) # linear
plot(sqft_living15,price) #linear
plot(sqft_lot,price) # non linear
plot(sqft_lot15,price)# non linear
plot(floors,price) 
plot(waterfront,price)
plot(view,price)
plot(condition,price)
plot(grade,price) # linear
plot(yr_built,price) # non-linear
plot(yr_renovated,price) # non-linear

#correlation
# all variables are '+' ve correlated with price
# greater than 0.85 is strong correlation
# between 0.50 to 0.85 is moderate correlation
cor(bathrooms,price) # 0.5251375 moderate
cor(sqft_above,price) #0.6055673 moderate
cor(sqft_basement,price) #0.323816 
cor(sqft_living,price) # 0.7020351 moderate
cor(sqft_living15,price) # 0.5853789 moderate
cor(sqft_lot,price) #0.08966086 
cor(sqft_lot15,price) #0.08244715 
cor(floors,price) # 0.2567939 
cor(waterfront,price)#0.2663694
cor(view,price)#0.3972935
cor(condition,price)#0.03636179
cor(grade,price) # 0.6674343 moderate

cor(yr_built,price) #0.05401153
cor(yr_renovated,price)#0.1264338 

#generate a model
model <- lm(price~price+bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+sqft_living15+sqft_lot15)
summary(model)
#Coefficients: (1 not defined because of singularities)  so i remove sqrt_basement , sqrt_above,sqrt_lot

# so we need to find the correlation between these three variables and all other variables are significant
pairs(data[1:nrow(data),c(7,13,14)])
# for partial correlation between variables
cor2pcor(cor(data[1:nrow(data),3:ncol(data)]))
#or we can find correlation by variance inflation factor
# we use step AIC to find the prominant features
stepAIC(model)
# in this problem no use
# we remove the sqft_lot and sqft_above
model_1 <- lm(price~price+bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+sqft_basement+yr_built+yr_renovated+sqft_living15+sqft_lot15)
summary(model_1)
# we remove the sqft_lot and sqft_basement
model_2 <- lm(price~price+bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+sqft_living15+sqft_lot15)
summary(model_2)
# we remove the sqft_basement and sqft_above
model_3 <- lm(price~price+bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+sqft_lot+yr_built+yr_renovated+sqft_living15+sqft_lot15)
summary(model_3)
# remove all the three
model_4 <-lm(price~price+bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+yr_built+yr_renovated+sqft_living15+sqft_lot15)
summary(model_4)
# In model 4 all variables are significant and p < 0.05 so model is accepted
# The model 4 is better so we can used it for regression
#predict
predict_values <- predict(model_4,data)
#RMSE VALUE
res <- predict_values-price
print(sqrt(mean(res ^ 2)))
