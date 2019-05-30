#Clean Start - Remove all variables

rm(list=ls())

################################ Handwritten Digit Classification Problem using MNIST Dataset ###########################################################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear SVM Model at C=1(Default)
#  4.2 Non-Linear Model - Kernels using rfdot
#  4.3 Non-Linear Model - Kernels using polydot
#  4.4 Inference
# 5.Tuning
# 5.1 Tunning linear SVM model 
# 5.2 Tunning Non-linear model using RBF
##########################################################################################################################################################################

# 1. Business Understanding: 

# The MNIST database contains 60,000 training images and 10,000 testing images.
# The set of images in the MNIST database is a combination of two of NIST's databases: Special Database 1 and Special Database 3.
# Special Database 1 and Special Database 3 consist of digits written by high school students and employees of the United States Census Bureau, respectively.
# 1. The first column in the dataset is the 'label' column containing handwritten digits 
# 2. Each digit is represented by 784 attributes(columns).Each attribute imply a pixel in the 28x28 image by representing them in the form
# of a numeric number.The least number 0 indicate the color 'white' while the highest number '255' indicate color 'black'.There are variations
# in the numerals for pixels indicating shades from white to black.

# The goal of this assignment is to develop a model that can correctly identify the digit (between 0-9) written in an image. 

##########################################################################################################################################################################

# 2. Data Understanding: 
# Before Reference links need to be read in order to gain insights into the MNIST dataset
# http://yann.lecun.com/exdb/mnist/
# https://en.wikipedia.org/wiki/MNIST_database
# Number of Instances: 60000
# Number of Attributes: 785 (1 class variabe and the rest attributes)
# Given the size of the dataset ,building a model with full training data would make computation expensive by time and energy,
# Hence only 15% of the training data imported will be used

#3. Data Preparation: 

##Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(caTools)
library(ggplot2)
library(tidyverse)
library(doParallel)


############ Enable parallel processing for faster computation ###################################

#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
#Choose the appropriate clusters that need to be used according to your PC configuration,I have chosen 4 as 
#my machine uses 4 clusters,Also plz ensure to disbale parallel processing when done using stopCluster(cl) which is in the last step

cl <-makePSOCKcluster(4)
registerDoParallel(cl)

##################################################################################################

#Loading Data

mnist_train <- read.csv("mnist_train.csv",header = F, stringsAsFactors = F)
mnist_test <- read.csv("mnist_test.csv",header=F,stringsAsFactors = F)

dim(mnist_train) #60000   785
dim(mnist_test) #10000   785

#Subset 15% of data from mnist_train for modelling - This data to be used for linear classification
set.seed(100)
ind_15 <- sample(1:nrow(mnist_train),.15*nrow(mnist_train))
mnist_train_15 <- mnist_train[ind_15,] #mnist_train_15 to be used for modelling

#Subset 10% of data from mnist_train for modelling - This data to be used for Non-linear classification
set.seed(100)
mnist_train_10_ind <- sample(1:nrow(mnist_train),0.1*nrow(mnist_train))
mnist_train_10 <- mnist_train[mnist_train_10_ind,]

#Understanding Dimensions
dim(mnist_test) # 10000   785
dim(mnist_train_15) # 9000  785
dim(mnist_train_10) # 6000  785

#Remove raw train data imported
rm(mnist_train)

#Adding a column name for the output variable for train data and test data
colnames(mnist_train_15)[1]<-'digit'
colnames(mnist_test)[1]<-'digit'
colnames(mnist_train_10)[1] <- 'digit'

# Changing output variable "digit" to factor type 
mnist_train_15$digit <- as.factor(mnist_train_15$digit)
mnist_test$digit <- as.factor(mnist_test$digit)
mnist_train_10$digit <- as.factor(mnist_train_10$digit )

#####checking the distribution of the label column ('digit') in training data

#1 for mnist_train_15 dataset
table(mnist_train_15$digit)
# 0   1   2   3   4   5   6   7   8   9 
#880 964 902 910 943 856 850 922 879 894 
#plot
ggplot(mnist_train_15,aes(x=digit))+geom_bar( )

#2 for mnist_train_10 dataset
table(mnist_train_10$digit)
#  0   1   2   3   4   5   6   7   8   9 
# 570 655 601 592 610 597 584 610 587 594 
#plot
ggplot(mnist_train_10,aes(x=digit))+geom_bar( )

#3 for mnist_test dataset
table(mnist_test$digit)
# 0    1    2    3    4    5    6    7    8    9 
#980 1135 1032 1010  982  892  958 1028  974 1009 
#plot
ggplot(mnist_test,aes(x=digit))+geom_bar( )

# Check column names matching in test and train data
a<-colnames(mnist_test)
length(a)
b<-colnames(mnist_train_15)
length(b)
setdiff(a,b) #Column names match in train and test data
setdiff(b,a) #Column names match in train and test data

#Checking column types
a<-data.frame(sapply(mnist_train_15, function(x) typeof(x)))
colnames(a)[1]<-'colnames' 
a$colnames<-as.factor(a$colnames)
summary(a) #all columns are of integer type

#Distribution of the label columnn 'digit' in train and test 

#Structure of the dataset
str(mnist_train_15)

#printing only first row to get a glimpse of the data
head(mnist_train_15,1)

#Exploring the data
summary(mnist_train_15)

# Checking missing value
sum(is.na(mnist_train_15)) # No missing values
sum(is.na(mnist_train_10)) # No missing values
sapply(mnist_train_15, function(x) sum(is.na(x))) # No missing values


#################################################################################################################

# 4. Model Building Using ksvm() function

#################################################################################################################
# 4.1 Linear model - SVM  at Cost(C) = 1(Default)
model_linear <- ksvm(digit ~ .,data=mnist_train_15,scale=FALSE,kernel='vanilladot')
eval_linear <- predict(model_linear,mnist_test)
(conf_linear <- confusionMatrix(eval_linear,mnist_test$digit))
#Accuracy : 0.9182 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9806   0.9850   0.9109   0.9020   0.9593   0.8688   0.9374   0.9066   0.8439   0.8751
#Specificity            0.9922   0.9944   0.9901   0.9834   0.9892   0.9886   0.9962   0.9936   0.9903   0.9911

# 4.2 Non-Linear model - SVM using polynomial
model_poly <- ksvm(digit ~ .,data=mnist_train_15,scale=FALSE,kernel='polydot')
eval_poly <- predict(model_poly,mnist_test)
(conf_linear <- confusionMatrix(eval_poly,mnist_test$digit))
#Accuracy : 0.9182
#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9806   0.9850   0.9109   0.9020   0.9593   0.8688   0.9374   0.9066   0.8439   0.8751
#Specificity            0.9922   0.9944   0.9901   0.9834   0.9892   0.9886   0.9962   0.9936   0.9903   0.9911

# 4.3 Non-Linear model - SVM using RBF 
model_radial <- ksvm(digit ~ .,data=mnist_train_15,scale=FALSE,kernel='rbfdot')
eval_radial <- predict(model_radial,mnist_test)
(conf_linear <- confusionMatrix(eval_radial,mnist_test$digit))
#Accuracy :  0.9554 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9898   0.9903   0.9506   0.9584   0.9633   0.9417   0.9676   0.9300   0.9363   0.9227
#Specificity            0.9955   0.9972   0.9945   0.9944   0.9927   0.9951   0.9965   0.9960   0.9947   0.9941

# 4.4 Inference
#----------------
#From the above models run on default hyper parameters we can see that non linear classifier using RBF achives highest accuracy i.e Accuracy : 0.9554 
#compared to others

#Free-up large objects from memory for further execution 

rm(model_linear)
rm(model_radial)
rm(model_poly)

################################################################################################################
# 5 Hyperparameter tuning  - on Linear and RBF models
################################################################################################################

# Since we have enough data in training there does not seem to be a need for cross validation,however i will experiment cross validation
# to find the optimal hyper parameters.


# 5.1 Tunning linear SVM model 
#-----------------------------------------------------------------------------------------------------------------------
 set.seed(100)
# making a grid of C values,C values are chosen very low becasue C value with 1,2,3,4 all give same accuracy
(grid <- expand.grid(C=c(1e-08, 1e-07, 5e-07, 1e-06, 2e-06 ,1e-05)))

#setup train control for corss validation with 5 folds

trainControl <- trainControl(method="cv",n=5,verboseIter=TRUE)
metric <- "Accuracy"

#Train the model
fit.svm_linear <- train(digit~., data=mnist_train_15, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
#Take a look at the model
print(fit.svm_linear)
plot(fit.svm_linear) #The final value used for the model was C = 5e-07

#Evaluate
eval_linear_tuned <- predict(fit.svm_linear,mnist_test)
(conf_linear_tuned <- confusionMatrix(eval_linear_tuned,mnist_test$digit))

#Accuracy : 0.9317 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9837   0.9824   0.9186   0.9238   0.9593   0.8890   0.9457   0.9202   0.8850   0.8999
#Specificity            0.9943   0.9958   0.9916   0.9892   0.9892   0.9898   0.9951   0.9934   0.9930   0.9925

# 'fit.svm_linear' is a good linear model with the optimal hyper parameter with an accuracy of 93%,This can be very well used for classifying digits


# 5.2 Tunning Non-linear model using RBF
#-----------------------------------------------------------------------------------------------------------------------

(trainControl <- trainControl(method="cv",n=5))
# Making a grid of Sigma and C values
set.seed(100)
(grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2)))
metric <- 'Accuracy'

fit.svm_rbf <- train(digit~.,data = mnist_train_15,trControl=trainControl,metric=metric,tuneGrid=grid,method='svmRadial')

#Take a look at the model
print(fit.svm_rbf)
plot(fit.svm_rbf) 

#Evaluate
Eval_rbf_tuned <- predict(fit.svm_rbf,mnist_test)
(conf_rbf_tuned <- confusionMatrix(Eval_rbf_tuned,mnist_test$digit)) #Model accuracy is not great hence i prefer to use the
#                                                                    model created using default sigma and c parameters

#Conclusion
#-----------
#The MNIST dataset can be classified using linear and non-linear SVM classifiers.However better accuracy is seen in Non-linear classifiers,
#Amongst non-linear classifiers 'Radial basis Function' rurns put to be good predictors compared to other classiiers within SVM.
#Hence 'Radial basis Function'is chosen for modelling.
#SVM model created usig rbf kernel i.e 'model_radial' using the ksvm() function with default gamma(1/ncol(mnist_train_15)) and c values is a good model with
#an accuracy of 95.5%.This is a better choice for classifying digits  ,Ref : # 4.3 Non-Linear model - SVM using RBF              
########################################################################################################################################## 

#Close Parallel processing

stopCluster(cl)


######################################## END of Assignment ###############################################################################    

#Below section can be used to Monitor memory usage during the execution,
library(magrittr)
Mb <- ls() %>% sapply(. %>% get %>% object.size %>% '/'(10^6)) 
(df_memory<-cbind(Mb, "Mb") %>% as.data.frame)
df_memory$Mb<-as.numeric(df_memory$Mb)
sum(df_memory$Mb) #returns results in Mb


######################################## END of Work #####################################################################################   






