#read the data
data <-read.csv("diabetes.csv")
# Function to remove outliers
remove_outlier <- function(data,index){
  #convert it as a matrix
  matrix_data <- as.matrix(data)
   #plot the data points to see outliers
    boxplot(data[,index],horizontal = T)
    # Take the first and third quantile
    quant <- quantile(data[,index], probs=c(.25, .75), na.rm =T)
    #to see difference berweeen the first and third quantile
    H_spread <- 1.5 * IQR(data[,index], na.rm = T)
    modified_data <- data
    # less than this is outlier
    modified_data[data[,index] < (quant[1] - H_spread),] <- NA
    # greater than this outlier
    modified_data[data[,index] > (quant[2] + H_spread),] <- NA
    data <-na.omit(modified_data)
    data <- as.matrix(data)
    #plot the data points
    boxplot(data[,index],horizontal = T)
   return (as.data.frame(data))
  }
data <- as.matrix(data)
#send that data matrix and column number to remove outliers
data <- remove_outlier(data,2)
