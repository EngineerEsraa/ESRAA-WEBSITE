library(readr)
library(caret)
library(class)# for knn func.
library(descr) # for crosstable func. 
library(caret) # for confusion matrix
library(ggplot2)
diabetes_data <- read.csv("C:/Users/tayseer/Desktop/diabetes.csv")
# Steps of perparing the data 
# 1) Now we wanna to know the missing in our  data
missing_data <- diabetes_data[,setdiff(names(diabetes_data), c('Outcome', 'Pregnancies'))]
missing_data[missing_data <= 0] <- NA
complete.cases(diabetes_data)
diabetes_data <- diabetes_data[complete.cases(diabetes_data), ] ##remove rows with null values
str(diabetes_data)

# 2) normalized the data 
# we need to normalize data if we have one attribute has  a wide range of values . we can know this through summary func.
#look at min. and max of values of all attributes 
summary(diabetes_data)
normalize<-function(x){
  num<-x-min(x)
  denom<- max(x)-min(x)
  return(num/denom)
  
}
#we use lapply to apply afunc. over diabetes_data 
YourNormalizedDataSet <- as.data.frame(lapply(diabetes_data, normalize))
YourNormalizedDataSet
summary(YourNormalizedDataSet)


# 3) Training and testing the data 
#In order to assess our model’s performance later, we will need to divide the data set into two parts: a training set and a test set.
#the most common splitting choice is to take 2/3 of your original data set as the training set, while the 1/3 that remains will compose the test set.
#when we spilt the data we should gaurantee that the model classify all unknown instances in outcome  
# we do this by seed that generate a random numbers 
YourNormalizedDataSet$Outcome <- as.factor(YourNormalizedDataSet$Outcome)

levels(YourNormalizedDataSet$Outcome) <- c('healthy', 'diabetes')
set.seed(777)
folds<-createFolds(YourNormalizedDataSet$Outcome,k=10)
model=lapply(folds, function(x){
  training_set=YourNormalizedDataSet[-x,]
  test_set=YourNormalizedDataSet[x,]
  ##################### knn model*******************************
  KNN=train(Outcome ~ .,data= training_set ,method="knn")
  pred=predict(KNN,newdata=test_set[-9])
  cm=table(test_set[,9],pred)
  accuarcy=(cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  sensitivity=cm[2,2]/(cm[2,2]+cm[2,1])
  specificity=cm[1,1]/(cm[1,2]+cm[1,1])
  ##################### linear regresstion model *********************************
  linear=train(Outcome ~ .,data= training_set,method="lda")
  pred1=predict(linear,newdata=test_set[-9])
  cm1=table(test_set[,9],pred1)
  accuarcy1=(cm1[1,1]+cm1[2,2])/(cm1[1,1]+cm1[2,2]+cm1[1,2]+cm1[2,1])
  sensitivity1=cm1[2,2]/(cm1[2,2]+cm1[2,1])
  specificity1=cm1[1,1]/(cm1[1,2]+cm1[1,1])
  ######################################
  conf.mat=as.data.frame(c(accuarcy,sensitivity,specificity,accuarcy1,sensitivity1,specificity1))
  return(conf.mat)
})

rowMeans(as.data.frame(model))