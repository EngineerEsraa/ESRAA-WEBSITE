library(readr)
library(caret)
library(class)# for knn func.
library(descr) # for crosstable func. 
library(caret) # for confusion matrix 

diabetes_data <- read.csv("diabetes.csv")

# Steps of perparing the data 
# 1) Now we wanna to know the missing in our  data
is.na(diabetes_data)

#we can find the sum  of missings in our data 
sum(is.na(diabetes_data))

#na.omit func. to remove the missings 
na.omit(diabetes_data)
diabetes_data

# 2) normalized the data 
# we need to normalize data if we have one attribute has  a wide range of values . we can know this through summary func.
#look at min. and max of values of all attributes 
summary(diabetes_data)

#to normalize the data:
normalize<-function(x){
  num<-x-min(x)
  denom<- max(x)-min(x)
  return(num/denom)
  
}
#we use lapply to apply afunc. over diabetes_data 
YourNormalizedDataSet <- as.data.frame(lapply(diabetes_data, normalize))
YourNormalizedDataSet
summary(YourNormalizedDataSet)
# 3) exploration our data 
diabetes_data$Outcome<-factor(diabetes_data$Outcome)
ggplot(diabetes_data,aes(Outcome,fill=Outcome))+
  geom_bar()+
  ggtitle("distribution of outcome variable")

ggplot(diabetes_data,aes( Pregnancies,fill = Outcome)) + 
  geom_bar(position = "Dodge") + 
  scale_x_continuous(limits = c(0,16)) +
  theme(legend.position = "bottom") +
  labs(title = "Pregnancies Vs Outcome")
ggplot(diabetes_data, aes(Glucose, color = Outcome, fill = Outcome)) +
  geom_density(alpha = 0.8) +
  theme(legend.position = "bottom") +
  labs(x = "Glucose", y = "Density", title = "Density plot of glucose")
ggplot(diabetes_data, aes(x = Outcome, y = Glucose,fill = Outcome)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Variation of glucose in women Vs Diabetes")
ggplot(diabetes_data, aes(BloodPressure, color = Outcome, fill = Outcome)) +
  geom_density(alpha = 0.8) +
  theme(legend.position = "bottom") +
  labs(x = "Blood pressure", y = "Density", title = "Density plot of Blood pressure")
ggplot(diabetes_data, aes(SkinThickness, color = Outcome, fill = Outcome)) +
  geom_density(alpha = 0.8) +
  theme(legend.position = "bottom") +
  labs(x = "Skin thickness", y = "Density", title = "Density plot of skin thickness")
ggplot(diabetes_data, aes(SkinThickness, color = Outcome, fill = Outcome)) +
  geom_histogram() +
  theme(legend.position = "bottom") +
  labs(x = "Skin thickness", y = "Density", title = "Density plot of skin thickness")

ggplot(diabetes_data, aes(SkinThickness, color = Outcome, fill = Outcome)) +
  geom_histogram() +
  theme(legend.position = "bottom") +
  labs(x = "Skin thickness", y = "count", title = "histogram for skin thickness")

ggplot(diabetes_data, aes(Outcome, y = Glucose,fill = Outcome)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Variation of glucose in women Vs Diabetes")
ggplot(diabetes_data, aes(Insulin, fill = Outcome)) +
  geom_histogram(binwidth=10) +
  theme(legend.position = "bottom") +
  ggtitle("Variation of Insulin content Vs Diabetes")

ggplot(diabetes_data, aes(BMI, fill = Outcome)) +
  geom_histogram() +
  theme(legend.position = "bottom") +
  ggtitle("Variation of BMI of women Vs Diabetes")

ggplot(diabetes_data, aes(Age, fill = Outcome)) +
  geom_histogram(binwidth = 5) +
  theme(legend.position = "bottom") +
  ggtitle("Variation of Age of women Vs Diabetes")


# 4 ) Training and testing  
#In order to assess our model performance later, we will need to divide the data set into two parts: a training set and a test set.
#the most common splitting choice is to take 2/3 of your original data set as the training set, while the 1/3 that remains will compose the test set.
#when we spilt the data we should gaurantee that the model classify all unknown instances in outcome  
# we do this by seed that generate a random numbers 
set.seed(12345)
sampling <- sample(2, nrow(YourNormalizedDataSet), replace=TRUE, prob=c(0.8, 0.2))
sampling
diabetes_training <- YourNormalizedDataSet[sampling==1,1:8]
diabetes_training
diabetes_testing <-YourNormalizedDataSet[sampling==2,1:8]
diabetes_testing

diabetes.trainlabels<-YourNormalizedDataSet[sampling==1,9]
diabetes.testlabels<-YourNormalizedDataSet[sampling==2,9]
diabetes.trainlabels
diabetes.testlabels

#KNN Model
#new instances are classified by looking at the majority vote or weighted vote. 
#In case of classification, the data point with the highest score wins the battle and the unknown instance receives the label of that winning data point.
#If there is an equal amount of winners, the classification happens randomly.
# we set k parameter by increasing the k value by from 1 until we obtain acceptable  accuracy and senstivity 


diabetes_perdiction <-knn(train = diabetes_training ,test = diabetes_testing,cl=diabetes.trainlabels , k=2)

diabetes_perdiction 

# Evaluation of Your Model
# we evalute our model through its accuracy and sensitivity 

diabetes.test<-data.frame(diabetes.testlabels)
merge<-data.frame(diabetes_perdiction ,diabetes.test)
names(merge)<-c("dia_pre","observations")

# to understand the relationship between expected values and predected values , we use the cross table
CrossTable(x = diabetes.testlabels, y = diabetes_perdiction, prop.chisq=FALSE)

# to calculate accuracy and sensitivity of our model we use confusion matrix 
expected<-factor(diabetes.testlabels)
expected
perdiction<-factor(diabetes_perdiction)
confusionMatrix(data=perdiction,reference = expected, positive = "1")