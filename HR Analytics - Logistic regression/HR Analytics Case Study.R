#*****************************Libraries**********************************************************#
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(caTools)
library(e1071)
library(cowplot)    # for plot_grid
library(GGally)     # ggpair for correlation between numeric variables
library(car)      # for stepAIC function
library(bda)        # for binning function
library(caret)      # for confusionMatrix function
library(ROCR)       # for prediction
library(pROC)       # roc function
library(MASS)       # for stepAIC function 

#************************************************************************************************#
#                    Load Input Files
#************************************************************************************************#
employee_survey_data<- read.csv("employee_survey_data.csv",stringsAsFactors = F)
general_data <- read.csv("general_data.csv",stringsAsFactors = F)
manager_survey_data <-read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

#************************************************************************************************#
# Checking each data set created above
#************************************************************************************************#
str(employee_survey_data)   #columns are of integer type
head(employee_survey_data)
summary(employee_survey_data)

str(general_data)   #columns are of character and integer type
head(general_data)
summary(general_data)

str(manager_survey_data)  #columns are of integer type
head(manager_survey_data)
summary(manager_survey_data)

str(in_time)   #columns are of character and logical type,also the first column name is missing which appears to be the customer id
head(in_time)
summary(in_time)


str(out_time)   #columns are of character and logical type,also the first column name is missing whic appears to be the customer id
head(out_time)
summary(out_time)


#Employee id min =1 and max = 4410 in each dataset


#************************************************************************************************#
# Checking duplicate employee ids  in each dataset
#************************************************************************************************#
nrow(employee_survey_data)
length(unique(tolower(employee_survey_data$EmployeeID))) # 4410 = nrow 
nrow(manager_survey_data)
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410 = nrow 
nrow(general_data)
length(unique(tolower(general_data$EmployeeID))) # 4410 = nrow 
nrow(in_time)
length(unique(tolower(in_time$X))) # 4410 = nrow 
nrow(out_time)
length(unique(tolower(out_time$X))) # 4410 = nrow 

#  EmployeeID is the primary key of each data set.


#Dim of data
dim(employee_survey_data) #row=4410,col=4
dim(general_data)#row=4410,col=24
dim(manager_survey_data)#row=4410,col=3
dim(in_time)#row=4410,col=262
dim(out_time)#row=4410,col=262


#************************************************************************************************#
# Renaming first column of in_time and out_time dataset as EmployeeID
#************************************************************************************************#

colnames(in_time) [1]  <- "EmployeeID"
colnames(out_time) [1] <- "EmployeeID"
summary(in_time)
summary(out_time)

#intime/outtime column name analysis
(col_names_in <- names(in_time))
(col_names_in <- col_names_in[-1])
(col_names_out <- names(out_time))
(col_names_out <- col_names_out[-1])
#Run a setdiff to see if there are same column names in in and out time dataset
setdiff(col_names_out,col_names_in)
setdiff(col_names_in,col_names_out)


#************************************************************************************************#
# Checking if Employee IDs are identical
#************************************************************************************************#
setdiff(in_time$EmployeeID, out_time$EmployeeID)  #0     
setdiff(employee_survey_data$EmployeeID, in_time$EmployeeID) #0     
setdiff(general_data$EmployeeID, in_time$EmployeeID)   #0 
setdiff(manager_survey_data$EmployeeID, general_data$EmployeeID) #0


#Checking NA values for employee intime data

sum(is.na(in_time))   #109080 NA values in employee intime datafrmae

intime_na<-as.data.frame(sapply(in_time, function(x) sum(is.na(x))))

sum(intime_na$`sapply(in_time, function(x) sum(is.na(x)))`==4410) # 12 columns has only NA values

#Removing columns which has NA values for all rows

in_time<-Filter(function(x) !(all(x=="")), in_time)

#Checking NA values for employees outtime data

sum(is.na(out_time))   #109080 NA values in employee outime datafrmae

outtime_na<-as.data.frame(sapply(out_time, function(x) sum(is.na(x))))

sum(outtime_na$`sapply(out_time, function(x) sum(is.na(x)))`>300) # 12 columns has only NA values

#Removing columns which has NA values for all rows

out_time<-Filter(function(x) !(all(x=="")), out_time)

#converting intime and outtime as date format ad calculating hours and leaves for employees
in_time[,-1]<-data.frame(lapply(in_time[,-1],function(x)ymd_hms(x)))
out_time[,-1]<-data.frame(lapply(out_time[,-1],function(x)ymd_hms(x)))
emp_hours<-out_time[,2:250]-in_time[,2:250]
emp_hours<-data.frame(lapply(emp_hours, as.numeric))
emp_hours<-emp_hours %>% mutate(average=rowMeans(.,na.rm = TRUE))
emp_hours<-mutate(emp_hours,leaves=apply(emp_hours, MARGIN = 1, FUN = function(x) length(x[is.na(x)]) ))

#************************************************************************************************#
# Merge all datasets to form master attrition data
#************************************************************************************************#
Master_attrition_data<-merge(employee_survey_data,manager_survey_data,by="EmployeeID",all=F)
Master_attrition_data<-merge(Master_attrition_data,general_data,by="EmployeeID",all=F)
Master_attrition_data<-cbind(Master_attrition_data,hours=emp_hours[,250],leaves=emp_hours[,251])


#************************************************************************************************#
# Removing columns which has same value in all rows
#************************************************************************************************#

Master_attrition_data_V2 <- Master_attrition_data[,-c(14,21,23)]

#Univariate Analysis

Attrition__summary <- Master_attrition_data_V2 %>% group_by(Attrition) %>% summarise(count1 = n())

View(Attrition__summary)
Attrition__summary$count1 <- 100 * Attrition__summary$count1/nrow(Master_attrition_data_V2)
Attrition__summary$count2 <- str_c(round(Attrition__summary$count1,2),"%")

ggplot(Attrition__summary,aes(x=Attrition,y=count1,fill=Attrition))+
  geom_bar(stat="identity") +
  geom_text(aes(label=count2),vjust = 2)

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
#Segmented univariate analysis
#Attrition vs Age
Attrition_Age<-ggplot(Master_attrition_data_V2,aes(Age,fill=Attrition))+geom_histogram(binwidth = 5,position = "fill")+bar_theme1

#Attrition vs Buisness Travel
Attrition_Buisness_Travel<-ggplot(Master_attrition_data_V2,aes(BusinessTravel,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Department
Attrition_Department<-ggplot(Master_attrition_data_V2,aes(Department,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs DistanceFromHome
Attrition_DistanceFromHome<-ggplot(Master_attrition_data_V2,aes(DistanceFromHome,fill=Attrition))+geom_histogram(binwidth = 2,position = "fill")+bar_theme1

#Attrition vs Education
Attrition_Education<-ggplot(Master_attrition_data_V2,aes(Education,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Education field
Attrition_Educationfield<-ggplot(Master_attrition_data_V2,aes(EducationField,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Gender
Attrition_Gender<-ggplot(Master_attrition_data_V2,aes(Gender,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Job Level
Attrition_Job_Level<-ggplot(Master_attrition_data_V2,aes(JobLevel,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Job Role
Attrition_Job_Role<-ggplot(Master_attrition_data_V2,aes(JobRole,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Marital status
Attrition_Marital_status<-ggplot(Master_attrition_data_V2,aes(MaritalStatus,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Income
Attrition_Income<-ggplot(Master_attrition_data_V2,aes(MonthlyIncome,fill=Attrition))+geom_histogram(binwidth = 10000,position = "fill")+bar_theme1

#Attrition vs No of companies worked
Attrition_Cwoked<-ggplot(Master_attrition_data_V2,aes(NumCompaniesWorked,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Salary Hike percentage
Attrition_Hike<-ggplot(Master_attrition_data_V2,aes(PercentSalaryHike,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Stockoptionlevel
Attrition_stock<-ggplot(Master_attrition_data_V2,aes(StockOptionLevel,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs TotalWorkingYears
Attrition_tyears<-ggplot(Master_attrition_data_V2,aes(TotalWorkingYears,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Training
Attrition_Training<-ggplot(Master_attrition_data_V2,aes(TrainingTimesLastYear,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Years at company
Attrition_yearscom<-ggplot(Master_attrition_data_V2,aes(YearsAtCompany,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Promotion
Attrition_promotion<-ggplot(Master_attrition_data_V2,aes(YearsSinceLastPromotion,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Years with current Manager
Attrition_yearsman<-ggplot(Master_attrition_data_V2,aes(YearsWithCurrManager,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs hours
Attrition_hours<-ggplot(Master_attrition_data_V2,aes(hours,fill=Attrition))+geom_histogram(binwidth = 2,position = "fill")+bar_theme1

#Attrition vs leaves
Attrition_leaves<-ggplot(Master_attrition_data_V2,aes(leaves,fill=Attrition))+geom_histogram(binwidth = 5,position = "fill")+bar_theme1

#Attrition vs Years with Environment Satisfaction
Attrition_env<-ggplot(Master_attrition_data_V2,aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs Job Satisfaction
Attrition_jobsat<-ggplot(Master_attrition_data_V2,aes(JobSatisfaction,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs work life balance
Attrition_wlb<-ggplot(Master_attrition_data_V2,aes(WorkLifeBalance,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs job involvement
Attrition_ji<-ggplot(Master_attrition_data_V2,aes(JobInvolvement,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

#Attrition vs performance rating
Attrition_pr<-ggplot(Master_attrition_data_V2,aes(PerformanceRating,fill=Attrition))+geom_bar(position = "fill")+bar_theme1

plot_grid(Attrition_Buisness_Travel,Attrition_Department,Attrition_Age,Attrition_DistanceFromHome)
#Employees who travel rarely have the highest count of attrition. However when looking at the proportion 24.9% employees who travel frequently and 15% of employees who travel rarely are likely to attrition.
#Research & Development has the highest count of attrition employees while Human Resource has the highest proportion of Attrition members in a department 
#Younger Employees are more likely to attrition than older ones
#No clear distinction between distance from home and Attrition

plot_grid(Attrition_Education,Attrition_Educationfield,Attrition_Gender,Attrition_Job_Level)
#Employees with a Life Sciences educational field account for the highest number of Attrition. However, employees with Human resources background accounts for the highest proportion of Attrition [Almost 40% of all employees with HR background Attrition 
#Seems to be no clear distinction of Attrition Rate between Male and Female 
#No clear distinction between Attrition Rate between Job levels
                                                                                                                                                                                      

plot_grid(Attrition_Job_Role,Attrition_Marital_status,Attrition_Income,Attrition_Cwoked)

#The attrition rate for research director is marginally higher but there seems to be no clear distinction of Attrition Rate for Job roles
#Single Employees are significantly more likely to attrition
#No clear distinction between monthly income and attrition
#Employees who have worked in less than 2 companies or more than 5 companies are at a higher risk of attrition

plot_grid(Attrition_Hike,Attrition_stock,Attrition_tyears,Attrition_Training)
#No clear relation between salary hike and attrition status. Single Employees are significantly more likely to attrition
#Employees with Total Working Years <8 years are at a higher risk of attrition 
#No distinct difference in training frequency with respect to attrition
#No clear relation between attrition and Stock level

plot_grid(Attrition_yearscom,Attrition_promotion,Attrition_yearsman,Attrition_hours)
#Employees with YearsAtCompany<5 are at a higher risk of attrition 
#No clear distinction between last promotion and attrition
#Employees with less than 1 Year worked under current Manager are at a higher rate of attrition 
#Employees who worked more hours are at a higher risk of attrition

plot_grid(Attrition_leaves,Attrition_env,Attrition_jobsat,Attrition_wlb,Attrition_pr,Attrition_ji)
#No clear distinction between leaves and attrition
#Employees with Low Work Environment Satisfaction levels have a very high risk of attrition 
#Employees with Low Job Satisfaction levels have a very high risk of attrition 
#High for less work balance lift employees
#Seems no difference for performance rating and job involvement categories

#Outlier treatment

quantile(Master_attrition_data_V2$MonthlyIncome,seq(0,1,0.02)) 

quantile(Master_attrition_data_V2$DistanceFromHome,seq(0,1,0.02)) 

quantile(Master_attrition_data_V2$Age,seq(0,1,0.02)) 

quantile(Master_attrition_data_V2$NumCompaniesWorked,seq(0,1,0.02),na.rm = TRUE) 

quantile(Master_attrition_data_V2$PercentSalaryHike,seq(0,1,0.02)) 

quantile(Master_attrition_data_V2$TotalWorkingHours,seq(0,1,0.02),na.rm = FALSE)

quantile(Master_attrition_data_V2$TrainingTimesLastYear,seq(0,1,0.02))

quantile(Master_attrition_data_V2$YearsAtCompany,seq(0,1,0.02)) 

quantile(Master_attrition_data_V2$YearsSinceLastPromotion,seq(0,1,0.02)) 

quantile(Master_attrition_data_V2$YearsWithCurrManager,seq(0,1,0.02)) 

quantile(Master_attrition_data_V2$hours,seq(0,1,0.02)) 

quantile(Master_attrition_data_V2$leaves,seq(0,1,0.02))

#There are no outliers


##############################################################################
#Approach 1 - convert most factored variables into dummy variables using bins
##############################################################################
#Treat NAs
NAs_col_wise <- sapply(Master_attrition_data_V2, function(x)sum(is.na(x))/nrow(Master_attrition_data_V2)*100)
NAs_col_wise[which(NAs_col_wise>0)] #Few columns with less than 1% NAs
#It is safe to completely omit NAs from Master_attrition_data_V2
Master_data <- na.omit(Master_attrition_data_V2) #Dataframe free form NAs
sum(is.na(Master_data))
##########Convert variable to factors and dummy variables########
str(Master_data) # there are columns with character and factor type
#A.Select columns with character type and convert them into factors and dummy variables
Master_data_char <- select_if(Master_data,is.character)
dim(Master_data_char) #4300    7
str(Master_data_char)
Master_data_char[] <-lapply(Master_data_char, factor) #convert all columns into factors
str(Master_data_char)
#Columns Attrition and Gender have only two levels so that can be coded with 1's and 0's
Master_data_char$Attrition <- ifelse(Master_data_char$Attrition=='Yes',1,0)
table(Master_data_char$Attrition)
Master_data_char$Gender <- ifelse(Master_data_char$Gender=='Male',1,0)
table(Master_data_char$Gender)
#Convert columns with factor levels more than into dummy variables
#Add dummy variables to the factor columns
Master_data_char_fordummy <- Master_data_char[,-which(colnames(Master_data_char)%in%c('Attrition','Gender'))]
str(Master_data_char_fordummy)
dummy_set_1 <- data.frame(lapply(Master_data_char_fordummy, function(x) data.frame(model.matrix(~x-1,data = Master_data_char_fordummy))[,-1]))
View(dummy_set_1)
str(dummy_set_1)
#Add ,Master_data_char$Attrition,Master_data_char$Gender to dummy_set_1
dummy_set_1 <- cbind(dummy_set_1,Gender = Master_data_char$Gender,Attrition = Master_data_char$Attrition )
str(dummy_set_1)
#Lets analyse Numeric columns of Master_data
# Analyse if any other column can be factored
a<-select_if(Master_data,is.numeric)
table_a<-sapply(a, table)
sapply(table_a, length)
summary(Master_data$hours)
#hours -which is average working hours ,can be binned as works_less_than_8hrs,Meets_8hrs,works_above_8hrs
#leaves - which is leaves taken by employees can be binned as less_than_10,10_to_20,above_20
#PerformanceRating - 3 <- 0,4 <- 1
#JobInvolvement - convert to dummy 
#YearsWithCurrManager - binned into YearsWithCurrManager_lessthan2 - 0 and 2,YearsWithCurrManager_2to5 - 2 and 5,YearsWithCurrManager_above5 - above5
#YearsSinceLastPromotion -binned into  promoted_recently - 1 or less than 1,promoted_last_4 - 2 and 4 yrs,promoted_last_10 - 5 and 10,promoted_last_15 - 11 and 15
#YearsAtCompany -binned into  recent_employee - 0 and 2,employee_3to5 - 3 and 5,employee_6to10 - 6 and 10,employee_above10 - above 10
#TrainingTimesLastYear -convert to dummy variable
#TotalWorkingYears -binned into  TotalWorkingYears_lesthan5,TotalWorkingYears_6and10,TotalWorkingYears_above10
#StockOptionLevel - convert to dummy variable
#PercentSalaryHike -binned into  PercentSalaryHike_11and15 - 11 and 15,PercentSalaryHike_16and20 - 16 and 20,PercentSalaryHike_21and25 - 21 and 25
#NumCompaniesWorked -binned into  NumCompaniesWorked_lessthan2 - 0 and 2,NumCompaniesWorked_2and5,NumCompaniesWorked_above5
#MonthlyIncome - Better to leave it as it is and scaled
#JobLevel - convert to dummy variable
#Education - convert to dummy variable
#DistanceFromHome - binned into DistanceFromHome_lessthan5 - 0 and 5,DistanceFromHome_lessthan10 - 6 and 10,DistanceFromHome_above10 - 11 and 29
#Age -binned into  Age_18to25,Age_26to30,Age_31to40,Age_41to50,Age_51to60
#WorkLifeBalance - convert to dummy variable
#JobSatisfaction - convert to dummy variable
#EnvironmentSatisfaction - convert to dummy variable

#Variabes that can be dummied directly without binning  are stored in to_dummy
to_dummy <- c('JobInvolvement','TrainingTimesLastYear','StockOptionLevel','JobLevel','Education','WorkLifeBalance','JobSatisfaction','EnvironmentSatisfaction')
#1
Avg_working_hrs <- round(Master_data$hours)
summary(Avg_working_hrs)
Avg_working_hrs_bin <- ifelse(Avg_working_hrs==8,'Meets_8hrs',ifelse(Avg_working_hrs<8,'works_less_than_8hrs',ifelse(Avg_working_hrs>8,'works_above_8hrs',NA)))
length(Avg_working_hrs_bin)
sum(is.na(Avg_working_hrs_bin))
table(Avg_working_hrs_bin)
#2
leaves_taken_bin <- ifelse(Master_data$leaves<=10,'leaves_taken_10orless',ifelse(between(Master_data$leaves,11,20),'leaves_taken_11to20',
                                                                                 ifelse(Master_data$leaves>20,'leaves_taken_above20',NA)))
summary(Master_data$leaves)
table(leaves_taken_bin)
#3
PerformanceRating_bin <- ifelse(Master_data$PerformanceRating==3,0,1)
table(PerformanceRating_bin)
#4
YearsWithCurrManager_bin <- ifelse(Master_data$YearsWithCurrManager<=2,'YearsWithCurrManager_lessthan2',ifelse(between(Master_data$YearsWithCurrManager,3,5),'YearsWithCurrManager_2to5',
                                                                                                               ifelse(Master_data$YearsWithCurrManager>5,'YearsWithCurrManager_above5',NA)))
table(YearsWithCurrManager_bin)
#5
YearsSinceLastPromotion_bin <- ifelse(Master_data$YearsSinceLastPromotion<=1,'Recently Promoted',ifelse(between(Master_data$YearsSinceLastPromotion,2,4),'Promoted_inlast_4',
                                                                                                        ifelse(between(Master_data$YearsSinceLastPromotion,5,10),'Promoted_inlast_10',
                                                                                                               ifelse(between(Master_data$YearsSinceLastPromotion,11,15),'Promoted_inlast_15',NA))))

table(YearsSinceLastPromotion_bin)
#6
YearsAtCompany_bin <- ifelse(Master_data$YearsAtCompany<=2,'Recent_Employee',ifelse(between(Master_data$YearsAtCompany,3,5),'YearsAtCompany_3to5',ifelse(between(Master_data$YearsAtCompany,6,10),'YearsAtCompany_6to10',
                                                                                                                                                         ifelse(Master_data$YearsAtCompany>10,'YearsAtCompany_above10',NA))))
table(YearsAtCompany_bin)
#7
TotalWorkingYears_bin <- ifelse(Master_data$TotalWorkingYears<=5,'TotalWorkingYears_lesthan5',ifelse(between(Master_data$TotalWorkingYears,6,10),'TotalWorkingYears_6and10',ifelse(Master_data$TotalWorkingYears>=11,
                                                                                                                                                                                   'TotalWorkingYears_above10','NA')))
table(TotalWorkingYears_bin)
#8
PercentSalaryHike_bin <- ifelse(Master_data$PercentSalaryHike<=15,'PercentSalaryHike_11and15',ifelse(between(Master_data$PercentSalaryHike,16,20),'PercentSalaryHike_16and20','PercentSalaryHike_21and25'))
table(PercentSalaryHike_bin)
#9
NumCompaniesWorked_bin <- ifelse(Master_data$NumCompaniesWorked<=2,'NumCompaniesWorked_lessthan2',ifelse(between(Master_data$NumCompaniesWorked,3,5),'NumCompaniesWorked_3and5','NumCompaniesWorked_above5'))
table(NumCompaniesWorked_bin)
#10
DistanceFromHome_bin <- ifelse(Master_data$DistanceFromHome<=5,'DistanceFromHome_lessthan5',ifelse(between(Master_data$DistanceFromHome,6,10),'DistanceFromHome_6and10',ifelse(Master_data$DistanceFromHome>10,
                                                                                                                                                                               'DistanceFromHome_above10',NA)))
length(DistanceFromHome_bin)
table(DistanceFromHome_bin)
#11
Age_bin<-ifelse(Master_data$Age<=25,'Age25andbelow',ifelse(between(Master_data$Age,26,30),'Age_26to30',ifelse(between(Master_data$Age,31,40),
                                                                                                              'Age_31to40',ifelse(between(Master_data$Age,41,50),'Age_41to50','Age_51to60'))))
table(Age_bin)
length(Age_bin)
#Dataframe of newly binned values is stored in Employee_data_df_binned
Master_data_df_binned<-data.frame(Avg_working_hrs_bin,leaves_taken_bin,YearsWithCurrManager_bin,YearsSinceLastPromotion_bin,
                                  YearsAtCompany_bin,TotalWorkingYears_bin,PercentSalaryHike_bin,NumCompaniesWorked_bin,DistanceFromHome_bin,
                                  Age_bin)
dim(Master_data_df_binned) #4300   10
str(Master_data_df_binned)
#Subset data that are originally factored
Master_data_original_bins <- Master_data[,which(names(Master_data)%in%to_dummy)]
dim(Master_data_original_bins) #4300    8
str(Master_data_original_bins)
#Convert them into factors
Master_data_original_bins <- data.frame(lapply(Master_data_original_bins, factor))
str(Master_data_original_bins)
dim(Master_data_original_bins) #4300    8
#Now lets convert other factor variables into dummy variables
dummy_set_2<-cbind(Master_data_df_binned,Master_data_original_bins)
str(dummy_set_2) 
dim(dummy_set_2) #4300   18
sum(is.na(dummy_set_2)) #nil
#dummy all numeric variables
dummy_set_2 <- data.frame(lapply(dummy_set_2,function(x) data.frame(model.matrix(~x-1,data = dummy_set_2))[,-1]))
dim(dummy_set_2) #4300   53
str(dummy_set_2)
sum(is.na(dummy_set_2)) #nil
#Add PerformanceRating_bin to dummy_set_2
dummy_set_2<-cbind(dummy_set_2,Performancerating = PerformanceRating_bin)
dim(dummy_set_2) #4300   54
sum(is.na(dummy_set_2)) #nil
#Scale monthly income
monthly_income <- scale(Master_data$MonthlyIncome)
summary(monthly_income)
sum(is.na(monthly_income)) #nill
#employee id
employee_id <- Master_data$EmployeeID
length(employee_id)
#Prepare final dataframe for modelling
Master_data_final <- cbind(employee_id,dummy_set_1,dummy_set_2,monthly_income)
dim(Master_data_final) #4300   77
str(Master_data_final)
###############################Train/Test Data split#################################
set.seed(100)
index <- sample.split(Master_data_final,SplitRatio = 0.7)
train<-Master_data_final[index,]
dim(train) #2959   77
str(train)
test <- Master_data_final[!index,]
dim(test) # 1341   77

###############################Logistics regression modelling#########################

model_1 <- glm(Attrition~.,data = train[,-1],family = 'binomial')
summary(model_1) # AIC: 2038.8

model_2 <- stepAIC(model_1,direction = 'both')
summary(model_2) # 1995.2
vif(model_2)
#remove JobInvolvement.x2 due to having relatively higher p-value
model_3 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                 Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                 YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                 YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                 TotalWorkingYears_bin.xTotalWorkingYears_above10 + TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                 NumCompaniesWorked_bin.xNumCompaniesWorked_above5 + NumCompaniesWorked_bin.xNumCompaniesWorked_lessthan2 + 
                 Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                 Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
               family = "binomial", data = train[, -1])
summary(model_3) #AIC: 1995.2
vif(model_3)
#remove JobInvolvement.x3 due to having relatively higher p-value
model_4 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                 Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                 YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                 YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                 TotalWorkingYears_bin.xTotalWorkingYears_above10 + TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                 NumCompaniesWorked_bin.xNumCompaniesWorked_above5 + NumCompaniesWorked_bin.xNumCompaniesWorked_lessthan2 + 
                 Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                 Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +
                 JobLevel.x5 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
               family = "binomial", data = train[, -1])
summary(model_4) #AIC:  1995
vif(model_4)
#remove TrainingTimesLastYear.x4 due to having relatively higher p-value
model_5 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                 Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                 YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                 YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                 TotalWorkingYears_bin.xTotalWorkingYears_above10 + TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                 NumCompaniesWorked_bin.xNumCompaniesWorked_above5 + NumCompaniesWorked_bin.xNumCompaniesWorked_lessthan2 + 
                 Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                 Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +
                 JobLevel.x5 + StockOptionLevel.x1 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
               family = "binomial", data = train[, -1])
summary(model_5) #AIC:  1995.2
vif(model_5)
#remove TrainingTimesLastYear.x1 due to having relatively higher p-value
model_6 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                 Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                 YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                 YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                 TotalWorkingYears_bin.xTotalWorkingYears_above10 + TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                 NumCompaniesWorked_bin.xNumCompaniesWorked_above5 + NumCompaniesWorked_bin.xNumCompaniesWorked_lessthan2 + 
                 Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                 Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +
                 JobLevel.x5 + StockOptionLevel.x1 +
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
               family = "binomial", data = train[, -1])
summary(model_6) #AIC:  1995.6
vif(model_6)
#remove NumCompaniesWorked_bin.xNumCompaniesWorked_lessthan2 due to having relatively higher p-value
model_7 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                 Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                 YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                 YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                 TotalWorkingYears_bin.xTotalWorkingYears_above10 + TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                 NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                 Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                 Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +
                 JobLevel.x5 + StockOptionLevel.x1 +
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
               family = "binomial", data = train[, -1])
summary(model_7) #AIC:  1996.3
vif(model_7)
#remove TotalWorkingYears_bin.xTotalWorkingYears_above10  due to having relatively higher p-value
model_8 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                 Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                 YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                 YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                 TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                 NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                 Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                 Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +
                 JobLevel.x5 + StockOptionLevel.x1 +
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
               family = "binomial", data = train[, -1])
summary(model_8) #AIC:  1996.5
vif(model_8)
#remove JobRole.xSales.Executive   due to having relatively higher p-value
model_9 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                 Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                 YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                 YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                 TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                 NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                 Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                 Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +
                 JobLevel.x5 + StockOptionLevel.x1 +
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
               family = "binomial", data = train[, -1])
summary(model_9) #AIC:  1997
vif(model_9)
#remove Age_bin.xAge_26to30   due to having relatively higher p-value

model_9 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                 Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                 YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                 YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                 TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                 NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                 Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                 Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +
                 JobLevel.x5 + StockOptionLevel.x1 +
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
               family = "binomial", data = train[, -1])
summary(model_9) #AIC:  1997.7
vif(model_9)

# model_9 has all variables with significant values who p-value is less than 0.05

# lets refine the model further untill we get three stars

#remove JobLevel.x5
model_10 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 +
                  StockOptionLevel.x1 +
                  TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_10) #AIC:  2001
vif(model_10)
#remove TrainingTimesLastYear.x5
model_11 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 +
                  StockOptionLevel.x1 +
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_11) #AIC:  2003.3
vif(model_11)
#remove BusinessTravel.xTravel_Rarely
model_12 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 +
                  StockOptionLevel.x1 +
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_12) #AIC:  2003.3
vif(model_12)
#remove StockOptionLevel.x1
model_13 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  Age_bin.xAge_51to60 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 +
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_13) #AIC:  2009.5
vif(model_13)
#remove Age_bin.xAge_51to60   
model_14 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 +
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_14) #AIC:  2013.7
vif(model_14)

#model_14 is another good mode with variables whose significance is 2 stars and above
model_15 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 +
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_15) #AIC:  2013.7
vif(model_15)
#remove JobRole.xManufacturing.Director
model_16 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 +
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_16) #AIC:  2018.9
vif(model_16)
#remove WorkLifeBalance.x4  
model_17 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_17) #AIC:  2024.1
vif(model_17)
#remove JobSatisfaction.x2  
model_18 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_18) #AIC:  2060.6
vif(model_18)
#remove JobSatisfaction.x3 
model_19 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_19) #AIC:  2058.7
vif(model_19)
#remove JobSatisfaction.x2 
model_20 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_20) #AIC:  2057.3
vif(model_20)
#remove JobRole.xResearch.Director  
model_21 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_31to40 + Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_21) #AIC:  2062.4
vif(model_21)
#remove Age_bin.xAge_31to40  
model_22 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  Age_bin.xAge_41to50 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_22) #AIC:  2068.4
vif(model_22)
#remove Age_bin.xAge_41to50  
model_23 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  TrainingTimesLastYear.x6, 
                family = "binomial", data = train[, -1])
summary(model_23) #AIC:  2076.8
vif(model_23)
#remove TrainingTimesLastYear.x6 
model_24 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales +
                  MaritalStatus.xSingle + Avg_working_hrs_bin.xworks_above_8hrs + 
                  Avg_working_hrs_bin.xworks_less_than_8hrs + YearsSinceLastPromotion_bin.xPromoted_inlast_4 + 
                  YearsSinceLastPromotion_bin.xRecently.Promoted + YearsAtCompany_bin.xYearsAtCompany_3to5 + 
                  YearsAtCompany_bin.xYearsAtCompany_6to10 + YearsAtCompany_bin.xYearsAtCompany_above10 + 
                  TotalWorkingYears_bin.xTotalWorkingYears_lesthan5 + 
                  NumCompaniesWorked_bin.xNumCompaniesWorked_above5 +
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3,
                family = "binomial", data = train[, -1])
summary(model_24) #AIC:  2089
vif(model_24)

Model_3star_allbinned_var <- model_24

#We choose Model_3star_allbinned_var as all variables are highly significant with VIF less than 4
#Significant variables are as below : 
#BusinessTravel-Travel_Frequently 
#Department-Research&Development,Sales 
#MaritalStatus-Single 
#Avg_working_hrs- above_8hrs,less_than_8hrs
#YearsSinceLastPromotion - Promoted_inlast_4,Recently.Promoted
#YearsAtCompany- YearsAtCompany_3to5,YearsAtCompany_6to10,YearsAtCompany_above10 
#TotalWorkingYears-TotalWorkingYears_lesthan5
#NumCompaniesWorked-NumCompaniesWorked_above5
#EnvironmentSatisfaction-2,3,4
#WorkLifeBalance-2,3

################Predict data using Model_3star_allbinned_var #######################

#Predict test data
dim(test)
str(test)
test_predict <- predict(Model_3star_allbinned_var,test[,-1],type = 'response')
length(test_predict)
#set probability cutover as 0.5 and measure accuracy
test_predict_0.5prob <- ifelse(test_predict>=0.5,'Yes','No')
test_predict_0.5prob<-as.factor(test_predict_0.5prob)
table(test_predict_0.5prob)
test_actual <- ifelse(test$Attrition==1,'Yes','No')
test_actual<-as.factor(test_actual)
table(test_actual)
#Set a confusion matrix to measure Accuracy,sensitivity and specificity
test_confusion_0.5prob <- confusionMatrix(test_predict_0.5prob,test_actual,positive = 'Yes')
test_confusion_0.5prob
test_confusion_0.5prob$overall[1]
test_confusion_0.5prob$byClass[1]
test_confusion_0.5prob$byClass[2]

#finding optimal probability cutoff

setconfusion_diffprobcutoff <- function(cutoff)
{
  test_predict_cutoff <- factor(ifelse(test_predict>=cutoff,'Yes','No'))
  create_confusion <- confusionMatrix(test_predict_cutoff,test_actual,positive = 'Yes')
  Accuracy <- create_confusion$overall[1]
  Sensitivity <- create_confusion$byClass[1]
  Specificity <- create_confusion$byClass[2]
  output <- t(as.matrix(c(Accuracy,Sensitivity,Specificity)))
  colnames(output) <- c("sensitivity", "specificity", "accuracy")
  return(output)
}
#create cutoff range
summary(test_predict) #0.005 - 0.87
cutoff_range = seq(0.005,0.87,length=100)
cutoff_range
#placeholder to store output of different probabilty cutoff
(OUT <- matrix(0,100,3))

#Create a matrix of outputs of accurancy,specificity,sensitivity

for (i in 1:100) {
  OUT[i,]<-setconfusion_diffprobcutoff(cutoff_range[i])
}

#plot
plot(cutoff_range,OUT[,1],xlab = 'Cutoff',ylab='Values',cex.lab=1.5,cex.axis=1.5,ylim = c(0,1),type = 'l',
     lwd=2,axes = F,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_range,OUT[,2],col="darkgreen",lwd=2)
lines(cutoff_range,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#Best cutoff where Accuracy,specificity,sensivity intersects
(cutoff <- cutoff_range[which(abs(OUT[,1]-OUT[,2])<0.01)])

#Choosing 0.14 as the probability cutoff
test_predict_0.14prob <- ifelse(test_predict>=0.14,'Yes','No')
test_predict_0.14prob<-as.factor(test_predict_0.14prob)
table(test_predict_0.14prob)
test_confusion_0.14prob <- confusionMatrix(test_predict_0.14prob,test_actual,positive = 'Yes')
test_confusion_0.14prob
test_confusion_0.14prob$overall[1] #Accuracy 0.699478 
test_confusion_0.14prob$byClass[1] #Sensitivity 0.7110092
test_confusion_0.14prob$byClass[2] #Specificity 0.6972395
#KS - Statistics
test_predict_0.14prob_ks <- ifelse(test_predict_0.14prob=='Yes',1,0)
str(test_predict_0.14prob_ks)
test_actual_ks<-ifelse(test_actual=='Yes',1,0)
str(test_actual_ks)
prediction_test <- prediction(test_predict_0.14prob_ks,test_actual_ks)
prediction_test
performance_test <- performance(prediction_test,'tpr','fpr')
performance_test
ks_table_test<-attr(performance_test,'y.values')[[1]]-attr(performance_test,'x.values')[[1]]
max(ks_table_test)
#Lift and Gain chart
lift <- function(labels,predicted_prob,groups=10){
  if(is.factor(labels))labels <- as.integer(as.character(labels))
  if(is.factor(predicted_prob))predicted_prob<-as.integer(as.character(predicted_prob))
  helper<-data.frame(cbind(labels,predicted_prob))
  helper[,'bucket']<-ntile(-helper[,'predicted_prob'],groups)
  gaintable <- helper %>% group_by(bucket) %>% summarise_at(vars(labels),funs(total=n(),
                                                                              totalresp=sum(.,na.rm = T)))%>%
    mutate(Cumresp=cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}
Attrition_decile = lift(test_actual_ks, test_predict, groups = 10)
Attrition_decile

plot_grid(ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)


#************************************************************************************************#
#Approach 2: Considering numeric variables as continous variables. Performed scaling for all continous variables.
#************************************************************************************************#

#************************************************************************************************#
#Converting categorical variables to factors
#************************************************************************************************#
Master_attrition_data_V3 <- Master_attrition_data_V2

Master_attrition_data_V3$Education <- factor(Master_attrition_data_V3$Education,
                                             levels = c(1,2,3,4,5),
                                             labels = c("Below_College", "College", "Bachelor","Master","Doctor"))


Master_attrition_data_V3$EnvironmentSatisfaction <- factor(Master_attrition_data_V3$EnvironmentSatisfaction,
                                                           levels = c(1,2,3,4),
                                                           labels = c("Low", "Medium", "High","Very_High"))


Master_attrition_data_V3$JobInvolvement <- factor(Master_attrition_data_V3$JobInvolvement,
                                                  levels = c(1,2,3,4),
                                                  labels = c("Low", "Medium", "High","Very_High"))

Master_attrition_data_V3$JobSatisfaction <- factor(Master_attrition_data_V3$JobSatisfaction,
                                                   levels = c(1,2,3,4),
                                                   labels = c("Low", "Medium", "High","Very_High"))

Master_attrition_data_V3$PerformanceRating <- factor(Master_attrition_data_V3$PerformanceRating,
                                                     levels = c(1,2,3,4),
                                                     labels = c("Low","Good","Excellent","Outstanding"))

Master_attrition_data_V3$WorkLifeBalance <- factor(Master_attrition_data_V3$WorkLifeBalance,
                                                   levels = c(1,2,3,4),
                                                   labels = c("Bad", "Good", "Better","Best"))



View(Master_attrition_data_V3)

str(Master_attrition_data_V3)
summary(Master_attrition_data_V3)

#************************************************************************************************#
# Checking missing values and removing
#************************************************************************************************#

sapply(Master_attrition_data_V3, function(x) sum(is.na(x)))

# Removing NAs in EnvironmentSatisfaction
level <- levels(Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$EnvironmentSatisfaction)),]$EnvironmentSatisfaction)
level[length(level) + 1] <- "Not Available"
Master_attrition_data_V3$EnvironmentSatisfaction <- factor(Master_attrition_data_V3$EnvironmentSatisfaction, levels = level)
Master_attrition_data_V3$EnvironmentSatisfaction[is.na(Master_attrition_data_V3$EnvironmentSatisfaction)] <- "Not Available"

# Removing NAs in JobSatisfaction
summary(Master_attrition_data_V3$JobSatisfaction)

level_1 <- levels(Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$JobSatisfaction)),]$JobSatisfaction)
level_1[length(level_1) + 1] <- "Not Available"
Master_attrition_data_V3$JobSatisfaction <- factor(Master_attrition_data_V3$JobSatisfaction, levels = level_1)
Master_attrition_data_V3$JobSatisfaction[is.na(Master_attrition_data_V3$JobSatisfaction)] <- "Not Available"

# Removing NAs in WorkLifeBalance

level_2 <- levels(Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$WorkLifeBalance)),]$WorkLifeBalance)
level_2[length(level_2) + 1] <- "Not Available"
Master_attrition_data_V3$WorkLifeBalance <- factor(Master_attrition_data_V3$WorkLifeBalance, levels = level_2)
Master_attrition_data_V3$WorkLifeBalance[is.na(Master_attrition_data_V3$WorkLifeBalance)] <- "Not Available"


# Removing NAs in NumCompaniesWorked

if (Master_attrition_data_V3$TotalWorkingYears==Master_attrition_data_V3$YearsAtCompany) {Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$NumCompaniesWorked)),]$NumCompaniesWorked<- 0
} else if (Master_attrition_data_V3$TotalWorkingYears > Master_attrition_data_V3$YearsAtCompany) { Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$NumCompaniesWorked)),]$NumCompaniesWorked<-1
}

# Removing NAs in TotalWorkingYears

if (Master_attrition_data_V3$NumCompaniesWorked == 0) {Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$TotalWorkingYears)),]$TotalWorkingYears <- Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$TotalWorkingYears)),]$YearsAtCompany
} else if (Master_attrition_data_V3$NumCompaniesWorked > 0) {Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$TotalWorkingYears)),]$TotalWorkingYears <- Master_attrition_data_V3[which(is.na(Master_attrition_data_V3$TotalWorkingYears)),]$YearsAtCompany+1
}


#************************************************************************************************#
#Checking missing values
#************************************************************************************************#

sapply(Master_attrition_data_V3, function(x) sum(is.na(x)))

# There are no missing values


#************************************************************************************************#
# Normalising continuous Variables
#************************************************************************************************#

Master_attrition_data_V4<-Master_attrition_data_V3
str(Master_attrition_data_V4)
View(Master_attrition_data_V4)
summary(Master_attrition_data_V4)

# Missing value
sapply(Master_attrition_data_V4, function(x) sum(is.na(x)))

# No missing values in the dataset


Master_attrition_data_V4$Age <- scale(Master_attrition_data_V4$Age)
Master_attrition_data_V4$DistanceFromHome <- scale(Master_attrition_data_V4$DistanceFromHome)
Master_attrition_data_V4$JobLevel <- scale(Master_attrition_data_V4$JobLevel)
Master_attrition_data_V4$MonthlyIncome <- scale(Master_attrition_data_V4$MonthlyIncome)
Master_attrition_data_V4$NumCompaniesWorked <- scale(Master_attrition_data_V4$NumCompaniesWorked)
Master_attrition_data_V4$PercentSalaryHike <- scale(Master_attrition_data_V4$PercentSalaryHike)
Master_attrition_data_V4$TotalWorkingYears <- scale(Master_attrition_data_V4$TotalWorkingYears)
Master_attrition_data_V4$StockOptionLevel<-scale(Master_attrition_data_V4$StockOptionLevel)
Master_attrition_data_V4$TrainingTimesLastYear<-scale(Master_attrition_data_V4$TrainingTimesLastYear)
Master_attrition_data_V4$YearsAtCompany<-scale(Master_attrition_data_V4$YearsAtCompany)
Master_attrition_data_V4$YearsSinceLastPromotion<-scale(Master_attrition_data_V4$YearsSinceLastPromotion)
Master_attrition_data_V4$YearsWithCurrManager<-scale(Master_attrition_data_V4$YearsWithCurrManager)
Master_attrition_data_V4$hours<- scale(Master_attrition_data_V4$hours)
Master_attrition_data_V4$leaves<- scale(Master_attrition_data_V4$leaves)
#Identify numeric variables

continuous_var_set <- Master_attrition_data_V4[,which(sapply(Master_attrition_data_V4, is.numeric))]
str(continuous_var_set) # 15 variables

#************************************************************************************************#
# Converting attrition to factor
#************************************************************************************************#

Master_attrition_data_V4$Attrition<- ifelse(Master_attrition_data_V3$Attrition=="Yes",1,0)

#************************************************************************************************#
# creating a dataframe of categorical variables

#char:2:6,9,10,12:14,16,17
#************************************************************************************************#
Char_Var_set<- Master_attrition_data_V4[,c(2:6,9,10,12:14,16,17)]
str(Char_Var_set)
str(Master_attrition_data_V4)

#************************************************************************************************#
#Creating dummy variables
#************************************************************************************************#

Char_Var_set2<- data.frame(sapply(Char_Var_set, function(x) factor(x)))
str(Char_Var_set2)

dummies<- data.frame(sapply(Char_Var_set2, 
                            function(x) data.frame(model.matrix(~x-1,data =Char_Var_set2))))


str(dummies)

#************************************************************************************************#
#Creating Final data set
#************************************************************************************************#

Master_Emp_Attrition <- cbind(Master_attrition_data_V4[,c(7,8,11,15,18,19,20,21,22,23,24,25,26,27,28)],dummies) 
View(Master_Emp_Attrition)

str(Master_Emp_Attrition)


#************************************************************************************************#
#Logistic Regression
#************************************************************************************************#

# splitting the data between train and test
Master_Emp_Attrition_V2 <- Master_Emp_Attrition
set.seed(100)

tainindeces <- sample(1:nrow(Master_Emp_Attrition_V2),0.7*nrow(Master_Emp_Attrition_V2))

# ---------- Training dataset ---------------------------
Emp_Attrition_train <- Master_Emp_Attrition_V2[tainindeces,]
str(Emp_Attrition_train)

# ----------Test Dataset-------------------------------

Emp_Attrition_test <- Master_Emp_Attrition_V2[-tainindeces,]


# --------------------------------------- Model Building -------------------------------------------

model_1 = glm(Attrition ~ ., data = Emp_Attrition_train, family = "binomial")
summary(model_1)

#Residual deviance: 2038.8  on 3033  degrees of freedom
#AIC: 2146.8
#------------ Dropping least significant varables using StepAIC-------------------#

step_1 <- stepAIC(model_1,direction = "both")

step_1

#Degrees of Freedom: 3086 Total (i.e. Null);  3054 Residual
#Null Deviance:	    2748 
#Residual Deviance: 2045 	AIC: 2111

#---------------------Create Model 2--------------------------------------------#

model_2 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 WorkLifeBalance.xGood + JobInvolvement.xHigh + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                 Education.xDoctor + EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                 JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = Emp_Attrition_train)
summary(model_2)

# null deviance: 2747.7  on 3086  degrees of freedom
#Residual deviance: 2045.3  on 3054  degrees of freedom
#AIC: 2111.3


# using vif to remove insignificant variables

vif(model_2)


#---------------------Create Model 3--------------------------------------------#
# Removed StockOptionLevel

model_3 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked 
               + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 WorkLifeBalance.xGood + JobInvolvement.xHigh + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                 Education.xDoctor + EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                 JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = Emp_Attrition_train)

summary(model_3)


# using vif to remove insignificant variables

vif(model_3)


#---------------------Create Model 4 --------------------------------------------#
# Remved WorkLifeBalance.xGood , Department.xHuman.Resources , Education.xDoctor 
model_4 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked 
               + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
                 JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = Emp_Attrition_train)


summary(model_4)


vif(model_4)
#Residual deviance: 2058.6  on 3058  degrees of freedom
#AIC: 2116.6

#---------------------Create Model 5 --------------------------------------------#
# Removed EducationField.xLife.Sciences  and JobRole.xLaboratory.Technician

model_5 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked 
               + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources +  
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = Emp_Attrition_train)

summary(model_5)

#Residual deviance: 2065.6  on 3060  degrees of freedom
#AIC: 2119.6

# using vif to remove insignificant variables

vif(model_5)

#---------------------Create Model 6 --------------------------------------------#

model_6 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked 
               + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources +  
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = Emp_Attrition_train)

summary(model_6)

#Residual deviance: 2068.6  on 3061  degrees of freedom
#AIC: 2120.6

# using vif to remove insignificant variables

vif(model_6)

#---------------------Create Model 7 --------------------------------------------#
# Removed JobRole.xResearch.Director 

model_7 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked 
               + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources +  
                 JobRole.xManufacturing.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = Emp_Attrition_train)

summary(model_7)

#Residual deviance: 2073.9  on 3062  degrees of freedom
#AIC: 2123.9

# using vif to remove insignificant variables

vif(model_7)

#---------------------Create Model 8 --------------------------------------------#
# Removed Monthly income

model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked 
               + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources +  
                 JobRole.xManufacturing.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = Emp_Attrition_train)

summary(model_8)


#Residual deviance: 2075.8  on 3063  degrees of freedom
#AIC: 2123.8

# using vif to remove insignificant variables

vif(model_8)
#---------------------Create Model 9 --------------------------------------------#
# Removed JobRole.xSales.Executive

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked 
               + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                 JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                 JobInvolvement.xHigh + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xHuman.Resources +  
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = Emp_Attrition_train)

summary(model_9)
#Residual deviance: 2082.0  on 3064  degrees of freedom
#AIC: 2128

# using vif to remove insignificant variables

vif(model_9)

#---------------------Create Model 10 --------------------------------------------#
# Removed obInvolvement.xHigh

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked 
                + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  hours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                  JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                  BusinessTravel.xNon.Travel + 
                  BusinessTravel.xTravel_Frequently + 
                  EducationField.xHuman.Resources +  
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                family = "binomial", data = Emp_Attrition_train)

summary(model_10)

#Residual deviance: 2088.0  on 3065  degrees of freedom
#AIC: 2132

# using vif to remove insignificant variables

vif(model_10)

#---------------------Create Model 11 --------------------------------------------#
# Removed EnvironmentSatisfaction.xLow

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked 
                + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  hours  + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                  JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                  BusinessTravel.xNon.Travel + 
                  BusinessTravel.xTravel_Frequently + 
                  EducationField.xHuman.Resources +  
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                family = "binomial", data = Emp_Attrition_train)

summary(model_11)


#Residual deviance: 2094.1  on 3066  degrees of freedom
#AIC: 2136.1

# using vif to remove insignificant variables

vif(model_11)

#---------------------Create Model 12 --------------------------------------------#
# Removed EnvironmentSatisfaction.xMedium 

model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked 
                + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  hours  + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xHigh + 
                  JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                  BusinessTravel.xNon.Travel + 
                  BusinessTravel.xTravel_Frequently + 
                  EducationField.xHuman.Resources +  
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                family = "binomial", data = Emp_Attrition_train)

summary(model_12)


#Residual deviance: 2099.9  on 3067  degrees of freedom
#AIC: 2139.9

# using vif to remove insignificant variables

vif(model_12)

#---------------------Create Model 13 --------------------------------------------#
# Removed YearsAtCompany

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked 
                + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  hours  + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xHigh + 
                  JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
                  BusinessTravel.xNon.Travel + 
                  BusinessTravel.xTravel_Frequently + 
                  EducationField.xHuman.Resources +  
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                family = "binomial", data = Emp_Attrition_train)

summary(model_13)


#Residual deviance: 2110.2  on 3068  degrees of freedom
#AIC: 2148.2
# using vif to remove insignificant variables

vif(model_13)

#Model 13 has all 18 significant vatiables.
#************************************************************************************************#
final_model <- model_13
str(Emp_Attrition_test)

test_pred <- predict(final_model, type = "response", 
                     newdata = Emp_Attrition_test[,-2])

summary(test_pred)
#************************************************************************************************#

Emp_Attrition_test$prob <- test_pred
View(Emp_Attrition_test)

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))

test_actual_attrition <- factor(ifelse(Emp_Attrition_test$Attrition==1,"Yes","No"))
table(test_pred_attrition,test_actual_attrition)

confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")

#************************************************************************************************#

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")

table(test_pred_attrition,test_actual_attrition)

confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")


perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition,test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 



plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]



test_cutoff_attrition <- factor(ifelse(test_pred >=0.51, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
acc
#acc : 85.5
sens <- conf_final$byClass[1]
#sens : 28.01
spec <- conf_final$byClass[2]
#spec : 96.1


roc(Attrition~final_model$fitted.values, data = Emp_Attrition_train, plot = TRUE, main = "ROC CURVE", col= "blue")
auc(Attrition~final_model$fitted.values, data = Emp_Attrition_train)

#Predictive power of the model is  82.84




test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  # 0.24


# --------------------------------------- Lift & Gain Chart ------------------------------------------- #


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile <- lift(test_actual_attrition, test_pred, groups = 10) 
Attrition_decile



plot_grid(ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)
