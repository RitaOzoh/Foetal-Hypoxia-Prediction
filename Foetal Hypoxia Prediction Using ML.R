#Install and load the packages to read an Excel data set

install.packages("readxl")
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("corrplot")
install.packages("class")
install.packages("caret")
install.packages("party")
install.packages("rpart.plot")
install.packages("randomForest")

library(readxl)
library(tidyverse)
library(ggthemes)
library(corrplot)
library(class)
library(caret)
library(party)
library(rpart)
library(rpart.plot)
library(randomForest)

#Load the data set

cardiotocography <- read_excel("C:/Users/ritao/OneDrive - Teesside University/Data Science Foundations/ICA/SUBMISSIONS/REPORT DOCUMENTS/CTG.xls", sheet = "Raw Data")


#Find out if you have a missing value
na_values <- is.na(cardiotocography)

#Sum of the missing values
sum(na_values)

#percentage of missing values
Total_variables_Cardiotocography  <- 85080
per_missing <- (sum(na_values)/Total_variables_Cardiotocography)*100
per_missing

#0.12% is missing, so, Use the listwise deletion method to delete rows that contain missing values
cardiotocography <- na.omit(cardiotocography)
cardiotocography

#Assign cardiotocography to a new variable and select the data/columns that you need 
ctg <- cardiotocography %>% select(LB, AC, UC, ASTV, MSTV, ALTV, MLTV, DL, DS, DP, Width, Min, Max, Nmax, Nzeros, Mode, Mean, Median, Variance, Tendency, NSP)

#Show the current structure of the data set
str(ctg)
head(ctg)

#The target column needs to be in a categorical class
ctg$NSP <- as.factor(ctg$NSP)

#Number of observations for each of the three levels of my NSP
table(ctg$NSP)

#Show the current structure of the data set
str(ctg)

#PART 2

#EXPLORATORY DATA ANALYSIS

ctg2 <- gather(ctg, "attribute", "n", 1:20)
str(ctg2)

ggplot(ctg2)+
  geom_boxplot(aes(NSP, log(n), color = NSP))+
  facet_wrap(~attribute, scales = "free")+
  labs(title = "Box-plot of all predictors(log scaled) per status of Foetus",
       subtitle = "Status can either be Normal -N- or Suspect -S- or Pathologic -P-")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text()) +
  ylab("Predictor's log value") +
  xlab('')


#Correlogram of all numeric variables.
corrplot(cor(ctg[,1:20]), type = "lower", tl.srt = 90)

correlations <- cor(ctg[,1:20])
corrplot(correlations, method="number")

#PART 3

#BUILDING MODEL USING KNN

#Data Normalization is done, so that the output remains unbiased.
normalize <- function(x){
  return (x-min(x)) / (max(x)-min(x))
}

ctg_norm <- as.data.frame(lapply(ctg[,1:20], normalize))
summary(ctg_norm)
head(ctg_norm)

#Create your testing and training data set (Data splicing)
#set the starting number used to generate a sequence of random numbers
set.seed(123)

#We allocate a sample of 70% of our data set to a new variable
ctg_sample <- sample(1:nrow(ctg_norm), size = nrow(ctg_norm)*0.7, replace = FALSE)

#We choose to train the allocated 70% of our data set
ctg_training <- ctg[ctg_sample,]

#We choose the test the remaining values of our data set
ctg_testing <- ctg[-ctg_sample,]

#create a separate training data frame for NSP feature, which is our target
train_ctg <- ctg[ctg_sample,21]
str(ctg_testing)

#create a separate testing data frame for NSP feature, which is our target
test_ctg <- ctg[-ctg_sample,21]

#We now want to build a machine learning model
#Step-1: We need to install the package required for training in KNN

#Step-2: Find the number of observations
NROW(train_ctg)

#We have 1488 variables
#Step-3: Determine the value of K by calculating the square root of 1488, which is 38.57
#We will have  values of K because the value is a decimal/fraction.

#Model for the first value of K
knn_38 <- knn(train = ctg_training, test = ctg_testing, cl = train_ctg$NSP, k = 38)


#Model for the Second value of K
knn_39 <- knn(train = ctg_training, test = ctg_testing, cl = train_ctg$NSP, k = 39)

#Calculate the accuracy of the created models
#Step-1: Calculate the proportion of correct classification for k = 38,39

#Accuracy when k =38
accuracy_38 <- 100 * sum(test_ctg$NSP == knn_38) / NROW(test_ctg$NSP)
accuracy_38

#Accuracy when k =39
accuracy_39 <- 100 * sum(test_ctg$NSP == knn_39) / NROW(test_ctg$NSP)
accuracy_39

#STEP 4

#PREDICTION

#Step-2: Check prediction against actual value in tabular form
#Checking prediction against actual value in tabular form for k = 39
table(knn_38, test_ctg$NSP)
knn_38

#Checking prediction against actual value in tabular form for k = 39
table(knn_39, test_ctg$NSP)
knn_39

#The accuracy for the model for when k=38 is 86.68, while that of k =39 is 86.99

#STEP 5

#CONFUSION MATRIX

#calculate the accuracy of the KNN model of k = 38
confusionMatrix(table(knn_38, test_ctg$NSP))

#calculate the accuracy of the KNN model of k = 39
confusionMatrix(table(knn_39, test_ctg$NSP))

#The accuracy of k = 39 is higher, SO, I will use the model of k = 39

#STEP 6

#TUNE MY MODEL

#I will create a loop that calculates the accuracy of the KNN model for K values ranging
#from 1 to 39 to find the maximum value of k.

i = 1
k_optm = 1
for(i in 1:39){
  knn_mod <- knn(train = ctg_training, test = ctg_testing, cl = train_ctg$NSP, k = i) 
  k_optm[i] <- 100 * sum(test_ctg$NSP == knn_mod) / NROW(test_ctg$NSP) 
  k = i
  cat(k, "=", k_optm[i], "\n")
}

#Plot the values of K against accuracy

plot(k_optm, type = "b", xlab = "k-values", ylab = "Accuracy level")

#The above graph shows that for 'K' value of 4 we get the maximum accuracy.

#To prove that the highest accuracy is when k = 4
x = knn(train = ctg_training, test = ctg_testing, cl = train_ctg$NSP, k = 4)
x
accuracy_1 <- 100 * sum(test_ctg$NSP == x) / NROW(test_ctg$NSP)

accuracy_1

#I will now build a KNN model of when k = 4

knn_4 <- knn(train = ctg_training, test = ctg_testing, cl = train_ctg$NSP, k = 4)

#Calculate the accuracy of the created models
#Step-1: Calculate the proportion of correct classification for k = 38,39

#Accuracy when k =4
accuracy_4 <- 100 * sum(test_ctg$NSP == knn_4) / NROW(test_ctg$NSP)
accuracy_4

#STEP 7

#CHECK PREDICTION

#Step-2: Check prediction against actual value in tabular form
#Checking prediction against actual value in tabular form for k = 39
table(knn_4, test_ctg$NSP)
knn_4

#The accuracy for the model for when k=4 is 90.90909

#Let's also use the confusion matrix to calculate the accuracy of the KNN models


#STEP 8

#CONFUSION MATRIX

#calculate the accuracy of the KNN model of k = 4
confusionMatrix(table(knn_4, test_ctg$NSP))

#STEP 9

#FEATURE SELECTION

set.seed(7)
cutoff <- 0.70
correlations <- cor(ctg[,1:20])
highlyCorrelated <- findCorrelation(correlations, cutoff=cutoff)
for (value in highlyCorrelated) {
  print(names(ctg)[value])
}
# create a new dataset without highly correlated features
ctgFeatures <- ctg[,-highlyCorrelated]
dim(ctgFeatures)

#Data Normalization is done, so that the output remains unbiased.
ctgF_norm <- as.data.frame(lapply(ctgFeatures[,1:15], normalize))
summary(ctgF_norm)
head(ctgF_norm)

#Create your testing and training data set (Data splicing)
#set the starting number used to generate a sequence of random numbers
set.seed(123)

#We allocate a sample of 70% of our data set to a new variable
ctgF_sample <- sample(1:nrow(ctgF_norm), size = nrow(ctgF_norm)*0.7, replace = FALSE)

#We choose to train the allocated 70% of our data set
ctgF_training <- ctgFeatures[ctgF_sample,]

#We choose the test the remaining values of our data set
ctgF_testing <- ctgFeatures[-ctgF_sample,]

#create a separate training data frame for NSP feature, which is our target
train_ctgF <- ctgFeatures[ctgF_sample,16]
str(ctgF_testing)

#create a separate testing data frame for NSP feature, which is our target
test_ctgF <- ctgFeatures[-ctgF_sample,16]

#We now want to build a machine learning model
#Step-1: We need to load the package required for training in KNN
library(class)

#Step-2: Find the number of observations
NROW(train_ctgF)

#We have 1488 variables
#Step-3: Determine the value of K by calculating the square root of 1488, which is 38.57
#We will have  values of K because the value is a decimal/fraction.

#Model for the first value of K
knn_38 <- knn(train = ctgF_training, test = ctgF_testing, cl = train_ctgF$NSP, k = 38)


#Model for the Second value of K
knn_39 <- knn(train = ctgF_training, test = ctgF_testing, cl = train_ctgF$NSP, k = 39)

#Calculate the accuracy of the created models
#Step-1: Calculate the proportion of correct classification for k = 38,39

#Accuracy when k =38
accuracy_38 <- 100 * sum(test_ctgF$NSP == knn_38) / NROW(test_ctgF$NSP)
accuracy_38

#Accuracy when k =39
accuracy_39 <- 100 * sum(test_ctgF$NSP == knn_39) / NROW(test_ctgF$NSP)
accuracy_39

#STEP 10

#PREDICTION

#Step-2: Check prediction against actual value in tabular form
#Checking prediction against actual value in tabular form for k = 39
table(knn_38, test_ctgF$NSP)
knn_38

#Checking prediction against actual value in tabular form for k = 39
table(knn_39, test_ctgF$NSP)
knn_39

#The accuracy for the model for when k=38 is 86.68, while that of k =39 is 86.99

#STEP 11

#CONFUSION MATRIX

#Let's also use the confusion matrix to calculate the accuracy of the KNN models

#calculate the accuracy of the KNN model of k = 38
confusionMatrix(table(knn_38, test_ctgF$NSP))

#calculate the accuracy of the KNN model of k = 39
confusionMatrix(table(knn_39, test_ctgF$NSP))

#The accuracy of k = 39 is higher, SO, I will use the model of k = 39

#I will create a loop that calculates the accuracy of the KNN model for K values ranging from 1 to 39

#STEP 12

#TUNE MY FEATURED MODEL

i = 1
k_optm = 1
for(i in 1:39){
  knn_mod <- knn(train = ctgF_training, test = ctgF_testing, cl = train_ctgF$NSP, k = i) 
  k_optm[i] <- 100 * sum(test_ctgF$NSP == knn_mod) / NROW(test_ctgF$NSP) 
  k = i
  cat(k, "=", k_optm[i], "\n")
}

#Plot the values of K against accuracy

plot(k_optm, type = "b", xlab = "k-values", ylab = "Accuracy level")

#To prove that the highest accuracy is when k = 1
x = knn(train = ctgF_training, test = ctgF_testing, cl = train_ctgF$NSP, k = 1)
x
accuracy_1 <- 100 * sum(test_ctgF$NSP == x) / NROW(test_ctgF$NSP)

accuracy_1

#The above graph shows that for 'K' value of 4 we get the maximum accuracy.
#I will now build a KNN model of when k = 4

knn_1 <- knn(train = ctg_training, test = ctg_testing, cl = train_ctg$NSP, k = 1)

#Calculate the accuracy of the created models
#Step-1: Calculate the proportion of correct classification for k = 38,39

#Accuracy when k =1
accuracy_1 <- 100 * sum(test_ctgF$NSP == knn_1) / NROW(test_ctgF$NSP)
accuracy_1

#STEP 13

#PREDICTION

#Step-2: Check prediction against actual value in tabular form
#Checking prediction against actual value in tabular form for k = 39
table(knn_1, test_ctgF$NSP)
knn_1

#The accuracy for the model for when k=1 is 90.28213

#STEP 14

#CONFUSION MATRIX

#Let's also use the confusion matrix to calculate the accuracy of the KNN models
#Load the required libraries
library(caret)

#calculate the accuracy of the KNN model of k = 1
confusionMatrix(table(knn_1, test_ctgF$NSP))

#IN CONCLUSION, WE CAN SEE THAT THE ACCURACY OF BOTH THE INITIAL AND TUNED DATA SET ARE THE SAME


#BUILDING MODEL USING DECISION TREE

#Create your testing and training data set (Data splicing)
#set the starting number used to generate a sequence of random numbers
set.seed(123)

#We allocate a sample of 70% of our data set to a new variable
ctg_sample <- sample(1:nrow(ctg), size = nrow(ctg)*0.7, replace = FALSE)

#We choose to train the allocated 70% of our data set
ctg_training <- ctg[ctg_sample,]

#We choose the test the remaining values of our data set
ctg_testing <- ctg[-ctg_sample,]

#create a separate training data frame for NSP feature, which is our target
train_ctg <- ctg[ctg_sample,21]
str(ctg_testing)

#create a separate testing data frame for NSP feature, which is our target
test_ctg <- ctg[-ctg_sample,21]

#Install and load decision tree packages

#Using ctree(Classification Variable) to view some variables (e.g first 3 variables), in your train data
ctg_tree <- ctree(NSP~., data = ctg_training, controls = ctree_control(mincriterion = 0.9, minsplit = 1000))
ctg_tree
#We can see that there are vector 23 nodes in this tree
#Plotting the tree
plot(ctg_tree, main = "Status classification of Foetus as at time of cardiography")

#PART 4

#PREDICTION

#We can see from the plot that the most important predictors are: DP, ALTV, MODE and ASTV
#Predicting your model
pred <- predict(ctg_tree, ctg_testing)
pred1 <- predict(ctg_tree, ctg_testing, controls = ctree_control(mincriterion = 0.99, minsplit = 500)) #For a more visible data

#PART 5

#CONFUSION MATRIX

#Install and Load confusion matrix packages
library(caret)

#Using the confusion Matrix to find the accuracy of my model
confusionMatrix(pred, ctg_testing$NSP)

#We can see that it has an overall accuracy of 92.3%


####DECISION TREE WITH RPART####


#Decision Tree with rpart package
ctg_tree2 <- train(NSP~., data = ctg_training, method = "rpart")
ctg_tree2

#To plot decision trees in rpart, we have to install and load the package

#Plot your decision tree
rpart.plot(ctg_tree2$finalModel, fallen.leaves = FALSE, main = "Status classification of foetus as at time of cardiography")

#We can see from the plot that the most important predictors are: MSTV and MEAN


##PREDICTION##

#Predict the data set
pred2 <- predict(ctg_tree2, ctg_testing)

##CONFUSION MATRIX##

#Using the confusion Matrix to find the accuracy of my model
confusionMatrix(pred2, ctg_testing$NSP)

#We can see that it has an overall accuracy of 87.2%


#BUILDING MODEL USING RANDOM FOREST

#Create your testing and training data set (Data splicing)
#set the starting number used to generate a sequence of random numbers
set.seed(123)

#We allocate a sample of 70% of our data set to a new variable
ctg_sample <- sample(1:nrow(ctg), size = nrow(ctg)*0.7, replace = FALSE)

#We choose to train the allocated 70% of our data set
ctg_training <- ctg[ctg_sample,]

#We choose the test the remaining values of our data set
ctg_testing <- ctg[-ctg_sample,]

#create a separate training data frame for NSP feature, which is our target
train_ctg <- ctg[ctg_sample,21]
str(ctg_testing)

#create a separate testing data frame for NSP feature, which is our target
test_ctg <- ctg[-ctg_sample,21]


#Using random Forest to train our data
rf_testing <- randomForest(NSP~., data = ctg_training)
rf_testing
plot(rf_testing)
str(rf_testing)
#We can see that as the number of trees increases, the OBB(Out of Bag Error) initially drops down, and then it becomes somewhat constant.
#The error cannot be improved after about 300 trees.

#PART 4

#TUNE MY MODEL

#I will tune mtry, so as to have an idea of what mtry value to use
set.seed(123)
mt <- tuneRF(ctg_training[,c(1:20)], ctg_training$NSP,
             stepFactor = 0.5,
             plot = TRUE,
             ntreeTry = 400,
             trace = TRUE,
             improve = 0.05)

#I will now go back to my rf_testing and rename it to rf, then give values for mtry and ntree,

set.seed(123)
rf <- randomForest(NSP~., data = ctg_training,
                   ntree = 400,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)

rf #Get information about the model

#We can see that our OOB now decreases from 5.78% to 5.31%

#Attributes of the random Forest
attributes(rf)

#From the attributes, we saw confusion, which is confusion Matrix, so
rf$confusion

#PART 5

#PREDICT MY MODEL

#Prediction with the test data
predict_1 <- predict(rf, ctg_testing)

#Look at what is predicted
head(predict_1)

#Check if the prediction is correct
head(ctg_testing$NSP)

#So, we can see that the predictions/classifications are 100% accurate for the first six items


#PART 6

#Confusion matrix 
confusionMatrix(predict_1, ctg_testing$NSP)

#The accuracy of the model is 94.2% (which is very good)

#Error Rate of our random forest model
plot(rf)

#Size of tree in terms of number of nodes
hist(treesize(rf),
     main = "Number of Nodes for the trees",
     col = "green")

#PART 7

#VARIABLE IMPORTANCE

#Variable importance (Find out which variable plays an important role in the model)
varImpPlot(rf, main = "Predictors level of Importance to this model")

#We can see from the plot that the most important predictors are: ASTV, ALTV and MSTV

#To see the actual values for the data points that were used for plotting the two charts
importance(rf,)

#To find out which predictor variable are actually used in the random forest
varUsed(rf)
#So, depending on the importance, we can see how often they have appeared in the random forest


#PART 8

#EXTRACT A SINGLE TREE TO SEE WHAT A TREE LOOKS LIKE

#Extract a single Tree
getTree(rf, 1, labelVar = TRUE)

