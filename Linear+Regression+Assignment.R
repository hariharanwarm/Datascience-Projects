#Clear all variables
rm(list=ls())

#Libraries required for analysis
library(MASS)
library(car)
library(tidyverse)
library(stringr)
library(corrplot)
library(gridExtra)

#Data required for analysis
car_price_data <- read.csv('CarPrice_Assignment.csv')

#understand the data
dim(car_price_data) #rows = 205 ,columns = 26
summary(car_price_data)

#Check NAs,duplicates
sum(is.na(car_price_data)) #nil,there are no NAs present
#Check any duplicate rows
sum(duplicated(car_price_data$car_ID))#nil,there is no duplicate information

#Derive a variable called  Carname with only 'Car name' i.e separate car model name
table(car_price_data$CarName)
car_price_data <- separate(car_price_data,CarName,into = c('Carname','Carmodel'),sep=' ')
table(car_price_data$Carname)#Car names are not consistent such as vw,vokswagen instead of volkswagen,toyouta instead of toyota,
#maxda instead of mazda,porcshce instead of porsche,Nissan(capitalisation consistency issue),using gsub lets bring all car names consistent with the correct names
car_price_data$Carname <- gsub('vw','volkswagen',car_price_data$Carname)
car_price_data$Carname <- gsub('toyouta','toyota',car_price_data$Carname)
car_price_data$Carname <- gsub('vokswagen','volkswagen',car_price_data$Carname)
car_price_data$Carname <- gsub('maxda','mazda',car_price_data$Carname)
car_price_data$Carname <- gsub('porcshce','porsche',car_price_data$Carname)
car_price_data$Carname <- gsub('Nissan','nissan',car_price_data$Carname)
table(car_price_data$Carname )#all names are now consistent
View(car_price_data) 
colnames(car_price_data)#Variable 'Carmodel' can be removed for modelling as not required as per the assigmenet instrucions
str(car_price_data) # a mxture of numeric,character and factor variables observed
#check outliers in numeric variables
car_num_data <- select_if(car_price_data,is.numeric)
colnames(car_num_data)
boxplot(car_num_data$symboling) # this is a categorical variable
boxplot(car_num_data$wheelbase) #outlier at the upper limit
quantile(car_num_data$wheelbase,probs = seq(0,1,0.01))
boxplot(car_num_data$carlength) #outlier at the lower limit
quantile(car_num_data$carlength,probs = seq(0,1,0.01))
boxplot(car_num_data$carwidth) #outlier at the upper limit
quantile(car_num_data$carwidth,probs = seq(0,1,0.01))
boxplot(car_num_data$carheight) #nil
boxplot(car_num_data$curbweight)#nil
boxplot(car_num_data$enginesize) #outlier at the upper limit
quantile(car_num_data$enginesize,probs = seq(0,1,0.01))
boxplot(car_num_data$boreratio)#nil
boxplot(car_num_data$stroke) #outlier at the upper/lower limit
quantile(car_num_data$stroke,probs = seq(0,1,0.01))
boxplot(car_num_data$compressionratio) #high outliers at the upper/lower limit
quantile(car_num_data$compressionratio,probs = seq(0,1,0.01))
boxplot(car_num_data$horsepower) #outlier at the upperlimit
quantile(car_num_data$horsepower,probs = seq(0,1,0.01))
boxplot(car_num_data$peakrpm) #outlier at the upper limit 
quantile(car_num_data$peakrpm,probs = seq(0,1,0.01))
boxplot(car_num_data$citympg) #outlier at the upper limit
quantile(car_num_data$citympg,probs = seq(0,1,0.01))
boxplot(car_num_data$highwaympg) #outlier at the upper limit
quantile(car_num_data$highwaympg,probs = seq(0,1,0.01))
boxplot(car_num_data$price) #outlier at the upper limit
quantile(car_num_data$price,probs = seq(0,1,0.01))
#the outliers seen in all variables seem to be correct in thier own right ,hence it will not be treated for modelling
#cor plot - to understand how each numerical variables are cirrelated with each other,this will help us in refining the model
corr_num <- cor(car_num_data)
corrplot(corr_num,method = 'color',type='lower',order='hclust',addCoef.col="grey",number.cex = 0.50)
#1.Postively correlated items(using a cut off of 0.75) are a)Wheelbase with carwidth,carlength and curbweight
#b)carwidth with carlength , curbweight with horsepower and engine size
#d)horse power with engine size e)citympg and highwaympg
#2.Negatively correlated items are (using a cut off -0.75) are a)citympg with curb weight
#b)highwaympg with curbweight and horse power
#3.Highly correlated variables with price(using a cut off of 0.75)
#Price with carwidth,curbweight,horsepower and engine size
############################Segmented Univariate Analysis####################################
#Function to create boxplots for segmented univariate analysis
compare_price_boxplot <- function(data,var_x){
  data%>%ggplot(aes(x=var_x,y=price))+geom_boxplot()
}
#Perform analysis of all categorical variables against price
#aspiration vs price - Aspiration appears to be a significant variable
compare_price_boxplot(car_price_data,car_price_data$aspiration)#median price of turbo is higher than std
#cylindernumber vs price - cylindernumber appears to be a significant variable
compare_price_boxplot(car_price_data,car_price_data$cylindernumber)#median price increases with the number of cyclinders except for 2 cyl
#doornumber vs price - doornumber does not appear to be a significant variable
compare_price_boxplot(car_price_data,car_price_data$doornumber) # median price is same ,no correlation
#carbody vs price - carbody appears to be a significant variable
compare_price_boxplot(car_price_data,car_price_data$carbody)# median price varies with carbody(hard top priced higher)
#drivewheel vs price - drivewheel appears to be a significant variable
compare_price_boxplot(car_price_data,car_price_data$drivewheel) #rwd priced higher,then4wd and then fwd
#enginelocation vs price - enginelocation appears to be a significant variable
compare_price_boxplot(car_price_data,car_price_data$enginelocation) #engine at rear side is priced higher,more cars have engine in the front
#enginetype vs price - enginetype appears to be a significant variable(dohcv may not be significant)
compare_price_boxplot(car_price_data,car_price_data$enginetype) #price varies with engine type,ohcv is priced higher and more cars in this type
#citympg vs price - citympg appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$citympg))#Citympg is negatively correlated
#highwaympg vs price - highwaympg appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$highwaympg))#highwaympg is negatively correlated
#horsepower vs price - horsepower appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$horsepower))#increase in horse power has some correlation with price
#peakrpm vs price - peakrpm may be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$peakrpm)) #does not show much correlation
#boreratio vs price - boreratio may be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$boreratio))#shows some correlation
#stroke vs price - stroke may be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$stroke))#doesn't show much correlation
#compressionratio vs price  - compressionratio may be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$compressionratio))#not show much correlation,but more cars with compressionratio 8
#fuelsystem vs price  - fuelsystem may be a significant variable(mfi,spfi does not seem to be significant)
compare_price_boxplot(car_price_data,factor(car_price_data$fuelsystem))#some correlation,mpfi are highly charged 
#enginesize vs price - enginesize appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$enginesize))#increase in size increases the price
#wheelbase vs price - wheelbase appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$wheelbase)) #some correlation
#carlength vs price - carlength appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$carlength))#some correlation
#carwidth vs price - carwidth appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$carwidth))#Increase in width increases price
#carheight vs price - carheight appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$carheight))#some correlation
#curbweight vs price - curbweight appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$curbweight))#increase in weight increases the price
#symboling vs price - symboling may be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$symboling))#there is some correlation
#Carname vs price - Carname appears to be a significant variable
compare_price_boxplot(car_price_data,factor(car_price_data$Carname))# there is a correlation of brand and price
#fueltype vs price - fueltype does not appear to be a significant value
compare_price_boxplot(car_price_data,factor(car_price_data$fueltype))#median price of diesel is higher
#Observations in segmented univariate analysis
#Variables that may not be signiifcant for prediction
#doorno,aspiration,carbody,4wd$fwd,dohcv engine type,mfi#spfi,fueltype

#################################End of Segmented univariate analysis#############################################

#################################Add Dummy Variables to the Categorical variables for modelling#################################

#Add 1s and 0s to the variables with factor level 2
#fueltype
levels(car_price_data$fueltype)<-c(1,0)
car_price_data$fueltype<-as.numeric(levels(car_price_data$fueltype))[car_price_data$fueltype]
#aspiration
levels(car_price_data$aspiration)<-c(1,0)
car_price_data$aspiration<-as.numeric(levels(car_price_data$aspiration))[car_price_data$aspiration]
#doornumber
levels(car_price_data$doornumber)<-c(1,0)
car_price_data$doornumber<-as.numeric(levels(car_price_data$doornumber))[car_price_data$doornumber]
#enginelocation
levels(car_price_data$enginelocation)<-c(1,0)
car_price_data$enginelocation<-as.numeric(levels(car_price_data$enginelocation))[car_price_data$enginelocation]
#Add dummy variables to variables with factor levels more than 2
#Carname
car_price_data$Carname<-as.factor(car_price_data$Carname)
carname_dummy <- data.frame(model.matrix(~Carname,data=car_price_data))
carname_dummy <- carname_dummy[,-1]
#symboling
car_price_data$symboling<-as.factor(car_price_data$symboling)
symboling_dummy <- data.frame(model.matrix(~symboling,data=car_price_data))
symboling_dummy <- symboling_dummy[,-1]
#carbody
carbody_dummy <- data.frame(model.matrix(~carbody,data=car_price_data))
carbody_dummy <- carbody_dummy[,-1]
#drivewheel
drivewheel_dummy <- data.frame(model.matrix(~drivewheel,data=car_price_data))
drivewheel_dummy <- drivewheel_dummy[,-1]
#enginetype
enginetype_dummy <- data.frame(model.matrix(~enginetype,data=car_price_data))
enginetype_dummy <- enginetype_dummy[,-1]
#cylindernumber
cylindernumber_dummy <- data.frame(model.matrix(~cylindernumber,data=car_price_data))
cylindernumber_dummy <- cylindernumber_dummy[,-1]
#fuelsystem
fuelsystem_dummy <- data.frame(model.matrix(~fuelsystem,data=car_price_data))
fuelsystem_dummy <- fuelsystem_dummy[,-1]
#Append the dummy variables to the dataframe car_price_data
car_price_data <- cbind(car_price_data,carname_dummy,symboling_dummy,carbody_dummy,drivewheel_dummy,enginetype_dummy,
                        cylindernumber_dummy,fuelsystem_dummy)
car_price_data <- car_price_data[,-which(names(car_price_data)%in%c('Carmodel','Carname','symboling','carbody','drivewheel','enginetype','cylindernumber','fuelsystem'))]

ncol(car_price_data) #70 variables for model
str(car_price_data)
nrow(car_price_data)
#create a correlation plot for all variables in the car_price_data
cor_all<-cor(car_price_data)
View(cor_all)
corrplot(cor_all,method = 'circle',type = 'lower',order = 'hclust')
#########################################End of Dummy Variable Setup####################################
#Split the dataset for training and test
set.seed(100)
train_ind <- sample(1:nrow(car_price_data),0.7*nrow(car_price_data))
length(train_ind)
#training dataset
car_price_data_train <- car_price_data[train_ind,]
dim(car_price_data_train)
View(car_price_data_train)
#test dataset
car_price_data_test <- car_price_data[-train_ind,]
dim(car_price_data_test)
##################Building the model####################
#m1 with all variables
m1<-lm(price~.-car_ID,data = car_price_data_train)
summary(m1)
step1 <- stepAIC(m1,direction = 'both')
step1
#model2 - using the formula suggested by stepAIC
m2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
           carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen + symboling.1 + symboling0 + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
           fuelsystem2bbl + fuelsystemmpfi, data = car_price_data_train)
summary(m2) # Adjusted R-squared:  0.9739
vif(m2) # some higher to lower vif variables : curbweight,enginesize,carbodysedan,carbodyhatchback,carlength,carwidth
#carlength seems to be less significant compared above variables with high VIF
#model3 - remove carlength
m3 <- lm(formula = price ~ aspiration + enginelocation  + 
           carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen + symboling.1 + symboling0 + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
           fuelsystem2bbl + fuelsystemmpfi, data = car_price_data_train)
summary(m3)#Adjusted R-squared:  0.9737 
vif(m3) # some higher to lower vif variables : curbweight,enginesize,carbodysedan have high vif

#model4 - remove fuelsystemmpfi as from univariate analysis it is understood the dsitribution of data is less and model3
#shows fuelsystemmpfi is not significant
m4 <- lm(formula = price ~ aspiration + enginelocation  + 
           carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen + symboling.1 + symboling0 + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
           fuelsystem2bbl , data = car_price_data_train)
summary(m4) #Adjusted R-squared:  0.9736 
vif(m4)#  some higher to lower vif variables : curbweight,enginesize,carbodysedan
#citympg is highly insignificant

#model5 - remove citympg
m5 <- lm(formula = price ~ aspiration + enginelocation  + 
           carwidth + curbweight + enginesize + stroke + peakrpm + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen + symboling.1 + symboling0 + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
           fuelsystem2bbl , data = car_price_data_train)
summary(m5) #Adjusted R-squared:  0.9737 
vif(m5)#  some higher to lower vif variables : curbweight,enginesize,carbodysedan,carbodyhatchback
#curbweight and enginesize are correlated with each other so we can choose to remove curbweight

#model6 remove curbweight
m6 <- lm(formula = price ~ aspiration + enginelocation  + 
           carwidth + enginesize + stroke + peakrpm + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen + symboling.1 + symboling0 + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
           fuelsystem2bbl , data = car_price_data_train)
summary(m6) #Adjusted R-squared:  0.9717 
vif(m6) #some higher to lower vif variables : carbodysedan,enginesize,carbodyhatchback
#symboling0 highly insignificant

#model7 - remove symboling0

m7 <- lm(formula = price ~ aspiration + enginelocation  + 
           carwidth + enginesize + stroke + peakrpm + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen + symboling.1 + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
           fuelsystem2bbl , data = car_price_data_train)
summary(m7) #Adjusted R-squared:  0.9719 
vif(m7) ##some higher to lower vif variables : carbodysedan,carbodyhatchback

#Model8 - remove symboling.1 
m8 <- lm(formula = price ~ aspiration + enginelocation  + 
           carwidth + enginesize + stroke + peakrpm + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen  + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
           fuelsystem2bbl , data = car_price_data_train)
summary(m8) #Adjusted R-squared:  0.972 
vif(m8) # #some higher to lower vif variables : carbodyhatchback  ,carbodysedan ,enginesize
#cylindernumberfive,fuelsystem2bbl highly insignificant

#model 9 - remove cylindernumberfive
m9 <- lm(formula = price ~ aspiration + enginelocation  + 
           carwidth + enginesize + stroke + peakrpm + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen  + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor  + 
           fuelsystem2bbl , data = car_price_data_train)
summary(m9) #Adjusted R-squared:  0.972 
vif(m9) #  #some higher to lower vif variables : carbodyhatchback  ,carbodysedan ,enginesize
#Carnamesaab and fuelsystem2bbl are insignificant

#model10 - remove fuelsystem2bbl as it is insignificant in all models
m10 <- lm(formula = price ~ aspiration + enginelocation  + 
           carwidth + enginesize + stroke + peakrpm + 
           Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
           Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
           Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
           Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
           Carnamevolkswagen  + symboling3 + 
           carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
           drivewheelrwd + enginetypeohc + enginetyperotor,data = car_price_data_train)
           
summary(m10) #Adjusted R-squared:  0.9718 
vif(m10)  # #some higher to lower vif variables :  carbodyhatchback  ,carbodysedan ,enginesize
#symboling3 is insignificant in all models

#model11 - remove symboling3
m11 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth + enginesize + stroke + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemercury + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
            drivewheelrwd + enginetypeohc + enginetyperotor,data = car_price_data_train)

summary(m11) #Adjusted R-squared:  0.9715 
vif(m11)  # #some higher to lower vif variables :  carbodyhatchback  ,carbodysedan ,enginesize
#Carnamemercury is insignificant in all models

#model 12 - remove Carnamemercury
m12 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth + enginesize + stroke + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesaab + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
            drivewheelrwd + enginetypeohc + enginetyperotor,data = car_price_data_train)

summary(m12) #Adjusted R-squared:  0.9714 
vif(m12)#some higher to lower vif variables : carbodyhatchback,enginesize,carbodysedan

#model13 - remove Carnamesaab

m13 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth + enginesize + stroke + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
            drivewheelrwd + enginetypeohc + enginetyperotor,data = car_price_data_train)

summary(m13) #Adjusted R-squared:  0.9709 
vif(m13) ##some higher to lower vif variables : enginesize,carbodyhatchback,carbodysedan 

#model14 - remove enginesize as all values are significant

m14 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth + stroke + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
            drivewheelrwd + enginetypeohc + enginetyperotor,data = car_price_data_train)

summary(m14) #Adjusted R-squared:  0.945  - signifficant drop
vif(m14) ##some higher to lower vif variables : carbodyhatchback, carbodysedan 

#model15 - remove carbodysedan

m15 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth + stroke + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + carbodywagon + 
            drivewheelrwd + enginetypeohc + enginetyperotor,data = car_price_data_train)

summary(m15) #Adjusted R-squared:  0.9416
vif(m15)#all less than 5
#enginetyperotor highly insignificant

#model16
m16 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth + stroke + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + carbodywagon + 
            drivewheelrwd + enginetypeohc ,data = car_price_data_train)

summary(m16) #Adjusted R-squared:  0.942
vif(m16)#all less than 5
#drivewheelrwd highly insignificant

#model17 - remove drivewheelrwd
m17 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth + stroke + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + carbodywagon + 
           enginetypeohc ,data = car_price_data_train)

summary(m17) #Adjusted R-squared:  0.9425
vif(m17)#all less than 5
#carbodywagon is highly insignificant

#modle18  - remove carbodywagon
m18 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth + stroke + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + 
            enginetypeohc ,data = car_price_data_train)

summary(m18) #Adjusted R-squared:  0.9429
vif(m18)#all less than 5
#stroke is highly insignificant

#model19 - remove stroke
m18 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth  + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhardtop + carbodyhatchback + 
            enginetypeohc ,data = car_price_data_train)

summary(m18) #Adjusted R-squared:  0.9432
vif(m18)#all less than 5
#carbodyhardtop is highly insignificant

#model19 - remove carbodyhardtop
m19 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth  + peakrpm + 
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhatchback + 
            enginetypeohc ,data = car_price_data_train)

summary(m19) #Adjusted R-squared:  0.9435 
vif(m19)#all less than 5
#peakrpm is highly insignificant

#model20 - remove peakrpm
m20 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth + Carnameporsche + 
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhatchback + 
            enginetypeohc ,data = car_price_data_train)

summary(m20) #Adjusted R-squared:  0.9432 
vif(m20)#all less than 5
#Carnameporsche is highly insignificant

#model21 - remove Carnameporsche
m21 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            carbodyhatchback + 
            enginetypeohc ,data = car_price_data_train)

summary(m21) #Adjusted R-squared:  0.9428 
vif(m21)#all less than 3
#carbodyhatchback is highly insignificant

#model22 - remove carbodyhatchback
m22 <- lm(formula = price ~ aspiration + enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            enginetypeohc ,data = car_price_data_train)

summary(m22) #Adjusted R-squared:  0.9426
vif(m22)#all less than 5
#aspiration is insignificant

#model23 - remove aspiration
m23 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + Carnamedodge + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            enginetypeohc ,data = car_price_data_train)

summary(m23) #Adjusted R-squared:  0.9418 
vif(m23)#all less than 2.1
#this is a good model with all values significant and vif lesser than 2

########Here onwards all th model will be refined to achive all variables fully signififcant with 3 stars
#model 24(refine further based on significance) - remove Carnamedodge
m24 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot + Carnameplymouth +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            enginetypeohc ,data = car_price_data_train)

summary(m24) #Adjusted R-squared:  0.9402 
vif(m24)#all less than 2.2

#model 25 - remove Carnameplymouth
m25 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + Carnamehonda + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            enginetypeohc ,data = car_price_data_train)

summary(m25) #Adjusted R-squared:  0.9381 
vif(m25)#all less than 2.1

#model26 - remove Carnamehonda
m26 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamenissan + Carnamepeugeot +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            enginetypeohc ,data = car_price_data_train)

summary(m26) #Adjusted R-squared:  0.9367 
vif(m26)#all less than 2

#model27 - remove Carnamenissan
m27 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamepeugeot +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            Carnamevolkswagen  + 
            enginetypeohc ,data = car_price_data_train)

summary(m27) #Adjusted R-squared:  0.9352 
vif(m27)#all less than 2

#model28 - remove Carnamevolkswagen
m28 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + 
            Carnamejaguar + Carnamemazda + Carnamemitsubishi + 
            Carnamepeugeot +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            enginetypeohc ,data = car_price_data_train)

summary(m28) #Adjusted R-squared:  0.9335
vif(m28)#all less than 2

#model29 - remove Carnamemitsubishi
m29 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + 
            Carnamejaguar + Carnamemazda + 
            Carnamepeugeot +
            Carnamerenault + Carnamesubaru + Carnametoyota + 
            enginetypeohc ,data = car_price_data_train)

summary(m29) #Adjusted R-squared:  0.9317
vif(m29)#all less than 2

#model30 - remove Carnametoyota
m30 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + 
            Carnamejaguar + Carnamemazda + 
            Carnamepeugeot +
            Carnamerenault + Carnamesubaru +
            enginetypeohc ,data = car_price_data_train)

summary(m30) #Adjusted R-squared:  0.9298
vif(m30)#all less than 2

#model31 - remove Carnamerenault
m31 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + 
            Carnamejaguar + Carnamemazda + 
            Carnamepeugeot +
            Carnamesubaru +
            enginetypeohc ,data = car_price_data_train)

summary(m31) #Adjusted R-squared:  0.9281
vif(m31)#all less than 2

#model32 
#model31 - remove Carnamemazda
m32 <- lm(formula = price ~ enginelocation  + 
            carwidth  +
            Carnamebmw + Carnamebuick + 
            Carnamejaguar  + 
            Carnamepeugeot +
            Carnamesubaru +
            enginetypeohc ,data = car_price_data_train)

summary(m32) #Adjusted R-squared:  0.9226 
vif(m32) #all less than 5
#Since all variables are highly significant and vif for all variables is less than 2 refining the model stops here at model 32

#############################Predict the prices###############################
#1.using model m23 - all values are significant and vif not more than 2.3,Adjusted R-squared: 0.9418
#Predict training data
car_price_m23_train <- car_price_data_train
colnames(car_price_m23_train)
car_price_m23_train$predicted_price <- predict(m23,car_price_m23_train[,-19])
car_price_m23_train$error_pred <- car_price_m23_train$predicted_price-car_price_m23_train$price
View(car_price_m23_train[,c('price','predicted_price','error_pred')])
ggplot(car_price_m23_train,aes(x=car_ID,y=price))+geom_line()+geom_line(aes(x=car_ID,y=predicted_price,col='red'))
#predict test data
car_price_m23_test <- car_price_data_test
colnames(car_price_m23_test)
car_price_m23_test$predicted_price <- predict(m23,car_price_m23_test[,-19])
car_price_m23_test$error_pred <- car_price_m23_test$predicted_price-car_price_m23_test$price
View(car_price_m23_test[,c('price','predicted_price','error_pred')])
#few cars are not predicted well
#r-square of test data for predicted vales
(rsqr_m23 <-(cor(car_price_m23_test$price,car_price_m23_test$predicted_price))^2) #80.44%
#plot to compare prove vs predicted price
(m23_plot_test<-ggplot(car_price_m23_test,aes(x=car_ID,y=price))+geom_line(colour='blue')+geom_line(aes(x=car_ID,y=predicted_price,colour='red'))+
    ggtitle('Model 23 - price prediction on test data')+labs(subtitle='Adjusted R-squared of the model:0.9418\nR square of price vs predicted 80.44%'))
#error plot
(err_m23<-ggplot(car_price_m23_test,aes(car_ID,error_pred))+geom_point()+
  scale_x_continuous(breaks = seq(0,250,25), limits = c(0,225))+ 
  scale_y_continuous(name = "Error", breaks = seq(-15000,15000,5000), limits = c(-15000,15000))+ 
  geom_hline(yintercept = 0)+labs(title='Model 23'))
#-------------------------------------------------------------------------
#2.using m31 - has all 3stars except one two star variable with Adjusted R-squared:  0.9281
car_price_m31_train <- car_price_data_train
colnames(car_price_m31_train)
car_price_m31_train$predicted_price <- predict(m31,car_price_m31_train[,-19])
car_price_m31_train$error_pred <- car_price_m31_train$predicted_price-car_price_m31_train$price
View(car_price_m31_train[,c('price','predicted_price','error_pred')])
ggplot(car_price_m31_train,aes(x=car_ID,y=price))+geom_line(colour='blue')+geom_line(aes(x=car_ID,y=predicted_price,colour='red'))
#predict test data
car_price_m31_test <- car_price_data_test
colnames(car_price_m31_test)
car_price_m31_test$predicted_price <- predict(m31,car_price_m31_test[,-19])
car_price_m31_test$error_pred <- car_price_m31_test$predicted_price-car_price_m31_test$price
View(car_price_m31_test[,c('price','predicted_price','error_pred')])
#few cars are not predicted well
#r-square of test data for predicted vales
(rsqr_m31 <-(cor(car_price_m31_test$price,car_price_m31_test$predicted_price))^2) #80.78%
#plot to compare prove vs predicted price
(m31_plot_test<-ggplot(car_price_m31_test,aes(x=car_ID,y=price))+geom_line(colour='blue')+geom_line(aes(x=car_ID,y=predicted_price,colour='red'))+
    ggtitle('Model 31 - price prediction on test data')+labs(subtitle='Adjusted R-squared of the model:0.9281\nR square of price vs predicted 80.78%'))
#error plot
(err_m31<-ggplot(car_price_m31_test,aes(car_ID,error_pred))+geom_point()+
  scale_x_continuous(breaks = seq(0,250,25), limits = c(0,225))+ 
  scale_y_continuous(name = "Error", breaks = seq(-15000,15000,5000), limits = c(-15000,15000))+ 
  geom_hline(yintercept = 0)+labs(title='Model 31'))
#-------------------------------------------------------------------------
#3.using m32 - has all 3stars variables with Adjusted R-squared:  0.9226 
car_price_m32_train <- car_price_data_train
colnames(car_price_m32_train)
car_price_m32_train$predicted_price <- predict(m32,car_price_m32_train[,-19])
car_price_m32_train$error_pred <- car_price_m32_train$predicted_price-car_price_m32_train$price
View(car_price_m32_train[,c('price','predicted_price','error_pred')])
ggplot(car_price_m32_train,aes(x=car_ID,y=price))+geom_line()+geom_line(aes(x=car_ID,y=predicted_price,col='red'))

#predict test data
car_price_m32_test <- car_price_data_test
colnames(car_price_m32_test)
car_price_m32_test$predicted_price <- predict(m32,car_price_m32_test[,-19])
car_price_m32_test$error_pred <- car_price_m32_test$predicted_price-car_price_m32_test$price
View(car_price_m32_test[,c('price','predicted_price','error_pred')])
#few cars are not predicted well
#r-square of test data for predicted vales
(rsqr_m32 <-(cor(car_price_m32_test$price,car_price_m32_test$predicted_price))^2) #82.4%
#plot to compare prove vs predicted price
(m32_plot_test<-ggplot(car_price_m32_test,aes(x=car_ID,y=price))+geom_line(colour='blue')+geom_line(aes(x=car_ID,y=predicted_price,colour='red'))+
    ggtitle('Model 32 - price prediction on test data')+labs(subtitle='Adjusted R-squared of the model:0.9226\nR square of price vs predicted 82.4%'))
#error plot
(err_m32<-ggplot(car_price_m32_test,aes(car_ID,error_pred))+geom_point()+
  scale_x_continuous(breaks = seq(0,250,25), limits = c(0,225))+ 
  scale_y_continuous(name = "Error", breaks = seq(-15000,15000,5000), limits = c(-15000,15000))+ 
  geom_hline(yintercept = 0)+labs(title='Model 32'))
#-------------------------------------------------------------------------
#Plots
#compare models 23,31,32
grid.arrange(m23_plot_test,m31_plot_test,m32_plot_test)
#compare errors
grid.arrange(err_m23,err_m31,err_m32)

##############Conclusion##################
#Model 32 is chosen to be the best model because all variables used are highly significant,vif for all values are less than 2,R-square for predicted 
#value in the test data is 82.4
#following are the vraiables that are the best predictors
#1.enginelocation
#2.carwidth
#3.Carnamebmw
#4.Carnamebuick
#5.Carnamejaguar
#6.Carnamepeugeot
#7.Carnamesubaru
#8.enginetypeohc

