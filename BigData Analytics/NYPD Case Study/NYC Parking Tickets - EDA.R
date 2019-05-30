#New York City is a thriving metropolis. 
#Just like most other metros that size, one of the biggest problems its citizens face is parking.
#The classic combination of a huge number of cars and a cramped geography is the exact recipe that leads to a huge number of parking tickets.
#For the scope of this analysis, we wish to compare the phenomenon related to parking tickets over three different years - 2015, 2016, 2017
#We will try and perform some exploratory analysis on this data. 
getwd()
# 1. Load SparkR
spark_path <- '/usr/local/spark'
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

# Initialise the sparkR session
sparkR.session(master = "yarn-client", sparkConfig = list(spark.driver.memory = "1g"))

# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

#Importing the source data files
NYC_parkingTkt_2015 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", "CSV", header="true", inferSchema = "true")
NYC_parkingTkt_2016 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", "CSV", header="true", inferSchema = "true")
NYC_parkingTkt_2017 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", "CSV", header="true", inferSchema = "true")

#*********************************************Checking the structure of newly created dataframes****************************************************************#
head(NYC_parkingTkt_2015)
#Summons_numbers seems to be the unique id that need to e analyses for as they represent a ticket.
#The plate_ID is a combination of Alphanumeric value,however there is one value that is fully alphabetic.
#The Registration state column seems to have values that are in numeric form as well which seems to be erroneous
#The street code columns seems to have values that are zeroes.
#The Vehicle expriration date seems to have dates in erroneous manner as some numeric values are concatenated after year part of the timestamp
#The violation time column seems to have values such as 0953A,0520P etc and the A and P were meant to be AM and PM
#Following rows appear to have NAs -Violation_Post_Code Violation_Description No_Standing_or_Stopping_Violation Hydrant_Violation, Double_Parking_Violation Latitude Longitude Community_Board Community_Council Census_Tract  BIN  BBL  NTA
#Columns From_Hours_In_Effect and To_Hours_In_Effect seem to have time value entered erroneously
#Date_First_Observed  column seems to have year part of the date erroneous

#Explore the data types 
str(NYC_parkingTkt_2015) # All columns seems to be int or Char type,The date columns need to be converted into date type where required

dim(NYC_parkingTkt_2015)

# There are 11809233 rows and  51 columns in NYC_parkingTkt_2015

head(NYC_parkingTkt_2016)

dim(NYC_parkingTkt_2016)

# There are 10626899 rows and  51 columns in NYC_parkingTkt_2016

head(NYC_parkingTkt_2017)

dim(NYC_parkingTkt_2017)

# There are 10803028 rows and  43 columns in NYC_parkingTkt_2017
# So it is clear that number of columns are different in the data set.

colnames(NYC_parkingTkt_2015)
colnames(NYC_parkingTkt_2016)
colnames(NYC_parkingTkt_2017)


#The columns "Latitude","Longitude","Community Board","Community Council ","Census Tract","BIN","BBL" and "NTA" are not present  in 2017 dataset.


#------ Loading required libraries-------#

library(dplyr)

library(stringr)

library(ggplot2)


#**************************************************EDA of NYC_parkingTkt_2015**********************************************************************************#

# Cleaning column names by Removing space in the columns since it was creating issue in the sql query and unwanted characters

colnames(NYC_parkingTkt_2015)<- str_trim(colnames(NYC_parkingTkt_2015), side= "both")
colnames(NYC_parkingTkt_2015)
colnames(NYC_parkingTkt_2015)<- str_replace_all(colnames(NYC_parkingTkt_2015), pattern=" ", replacement = "_")

colnames(NYC_parkingTkt_2015)<- str_replace_all(colnames(NYC_parkingTkt_2015), pattern="\\?", replacement = "")

colnames(NYC_parkingTkt_2015)
#Creating table view for sql operations
createOrReplaceTempView(NYC_parkingTkt_2015, "2015_NYC_tkt")

#Check data_types of the variables in the table


#Check how many  unique Summons_number are there,if there are duplicates then that needs to be removed for better analysis
unique_summon <- SparkR::sql("select count(distinct Summons_Number) from 2015_NYC_tkt")
head(unique_summon) #There are 10951256 unique Summons_Number  as apposed to 11809233 which is the total number of rows
# Removing duplicates in Summons_Number

NYC_parkingTkt_2015<- dropDuplicates(NYC_parkingTkt_2015, "Summons_Number")

dim(NYC_parkingTkt_2015)

#There are 10951256 rows after removing duplicates.


# Exploring ticket "Issue Date" in 2015


head(SparkR::sql("select count(*) from 2015_NYC_tkt where Issue_Date is null")) # There are no NAs or nulls


#Issue date is not null in any of the records in 2015

# Converting date columns to same format "MMDDYYYY"

NYC_parkingTkt_2015$Issue_Date <- SparkR::to_date(NYC_parkingTkt_2015$Issue_Date, 'MM/dd/yyyy')

# Checking maximum and minimum of issue date.

createOrReplaceTempView(NYC_parkingTkt_2015, "2015_NYC_tkt")

Issue_Date_2015_Range <- SparkR::sql("SELECT min(Issue_Date)as Min_Issue_Date_2015,
                                     
                                     max(Issue_Date)as Max_Issue_Date_2015
                                     
                                     FROM 2015_NYC_tkt")

head(Issue_Date_2015_Range)


# Min Issue_Date = 1985-07-16
# Max Issue_Date = 2015-06-30
# Min issue date is from 1985. 
# Hence we will extract the year and month from the issue date to find out only 2015 data.

NYC_parkingTkt_2015$Year_of_issuedt <- year(NYC_parkingTkt_2015$Issue_Date)

NYC_parkingTkt_2015$Month_of_issuedt <- month(NYC_parkingTkt_2015$Issue_Date)

# Let us consider the fiscal year from Jul 1st, 2014 to Jun 30, 2015

NYC_PrkTkt_2015 <- NYC_parkingTkt_2015[
  
  NYC_parkingTkt_2015$Issue_Date >= "2014-07-01" & 
    
    NYC_parkingTkt_2015$Issue_Date <= "2015-06-30"]


nrow(NYC_PrkTkt_2015)
dim(NYC_PrkTkt_2015)

# There are 10598035 records in the above dataset.


#************************************************************************************************************************************#
#The columns "Latitude","Longitude","Community Board","Community Council ","Census Tract","BIN","BBL" and "NTA" are not present  in 2017 dataset.
#So let us check if we can drop these columns from 2015 dataset.

#************************************************************************************************************************************#


createOrReplaceTempView(NYC_PrkTkt_2015, "NYC_PrkTkt_2015_V2")

ExtraCol_2015_nullcount <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       
                                       SUM(CASE WHEN Latitude IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Latitude_nulls,
                                       
                                       SUM(CASE WHEN Longitude IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Longitude_nulls,
                                       
                                       SUM(CASE WHEN Community_Board IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Community_Board_nulls,
                                       
                                       SUM(CASE WHEN Community_Council IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Community_Council_nulls,
                                       
                                       SUM(CASE WHEN Census_Tract IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Census_Tract_nulls,
                                       
                                       SUM(CASE WHEN BIN IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) BIN_nulls,
                                       
                                       SUM(CASE WHEN BBL IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) BBL_nulls,
                                       
                                       SUM(CASE WHEN NTA IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) NTA_nulls     
                                       
                                       FROM NYC_PrkTkt_2015_V2")



head(ExtraCol_2015_nullcount)

#Num_of_Rows Latitude_nulls Longitude_nulls Community_Board_nulls Community_Council_nulls
# 10598035       10598035        10598035              10598035                10598035
# Census_Tract_nulls BIN_nulls BBL_nulls NTA_nulls
#      10598035  10598035  10598035  10598035

# The null values in extra columns in  the 2015 data set is equal to total no. of rows. Hence it is clear that we can drop these extra columns since it contains only nulls

# So removing these extra columns

NYC_PrkTkt_2015<- drop(NYC_PrkTkt_2015, c("Latitude","Longitude","Community_Board", "Community_Council","Census_Tract","BIN" , "BBL",  "NTA") )

colnames(NYC_PrkTkt_2015)

# In Violation Time, we need to append M since it is missing in AM/PM.

NYC_PrkTkt_2015$Append_M <- "M"

NYC_PrkTkt_2015$Violation_Time<-concat(NYC_PrkTkt_2015$Violation_Time,  NYC_PrkTkt_2015$Append_M)

NYC_PrkTkt_2015$Time_First_Observed<- concat(NYC_PrkTkt_2015$Time_First_Observed, NYC_PrkTkt_2015$Append_M)

NYC_PrkTkt_2015$From_Hours_In_Effect<- concat(NYC_PrkTkt_2015$From_Hours_In_Effect, NYC_PrkTkt_2015$Append_M)

NYC_PrkTkt_2015$To_Hours_In_Effect<- concat(NYC_PrkTkt_2015$To_Hours_In_Effect, NYC_PrkTkt_2015$Append_M)

NYC_PrkTkt_2015<- drop(NYC_PrkTkt_2015, c("Append_M"))


# Extract Violation Hour, Violation Minute and AM/PM from Violation Time.

NYC_PrkTkt_2015$Violation_Hour <- substr(NYC_PrkTkt_2015$Violation_Time, 1, 2)

NYC_PrkTkt_2015$Violation_Minute <- substr(NYC_PrkTkt_2015$Violation_Time, 3, 4)

NYC_PrkTkt_2015$Violation_AM_PM <- substr(NYC_PrkTkt_2015$Violation_Time, 5, 6)

head(NYC_PrkTkt_2015)

# Replacing 00xxAM with 12xxAM in violation time

NYC_PrkTkt_2015$Violation_Hour <- regexp_replace(x = NYC_PrkTkt_2015$Violation_Hour,pattern = "\\00",replacement = "12")

# Apply standardized format to Violation Time

NYC_PrkTkt_2015$Violation_Time <- concat(NYC_PrkTkt_2015$Violation_Hour, NYC_PrkTkt_2015$Violation_Minute, NYC_PrkTkt_2015$Violation_AM_PM)


#Converting Violation Time into a TimeStamp

NYC_PrkTkt_2015$Violation_Time<-to_timestamp(x = NYC_PrkTkt_2015$Violation_Time, format = "hhmma")


#Converting the other time columns into a TimeStamp.

NYC_PrkTkt_2015$Time_First_Observed<- to_timestamp(x= NYC_PrkTkt_2015$Time_First_Observed, format = "hhmma")

NYC_PrkTkt_2015$From_Hours_In_Effect<- to_timestamp(x= NYC_PrkTkt_2015$From_Hours_In_Effect, format = "hhmma")

NYC_PrkTkt_2015$To_Hours_In_Effect<- to_timestamp(x= NYC_PrkTkt_2015$To_Hours_In_Effect, format = "hhmma")

head(NYC_PrkTkt_2015)

dim(NYC_PrkTkt_2015)
#After dat caleanig such as dropping NAs,Removing duplicate Summon_Numbers,subsettign with correct fiscal date the
#no.of columns remained are Rows : 10598035    col:48 

#**************************************************EDA of NYC_parkingTkt_2016**********************************************************************************#
head(NYC_parkingTkt_2016)
#Summons_numbers seems to be the unique id that need to e analyses for as they represent a ticket.
#The Registration state column seems to have values that are in numeric form as well which seems to be erroneous
#The street code columns seems to have values that are zeroes.
#The Vehicle expiration date seems to have dates in erroneous manner as some numeric values are concatenated after year part of the timestamp
#The violation time column seems to have values such as 0953A,0520P etc and the A and P were meant to be AM and PM
#Following rows appear to have NAs -Violation_Post_Code Violation_Description No_Standing_or_Stopping_Violation Hydrant_Violation, Double_Parking_Violation Latitude Longitude Community_Board Community_Council Census_Tract  BIN  BBL  NTA
#Columns From_Hours_In_Effect and To_Hours_In_Effect seem to have time value entered erroneously
#Date_First_Observed  column seems to have year part of the date erroneous


# Removing space in the columns since it was creating issue in the sql query

colnames(NYC_parkingTkt_2016)<- str_trim(colnames(NYC_parkingTkt_2016), side= "both")

colnames(NYC_parkingTkt_2016)<- str_replace_all(colnames(NYC_parkingTkt_2016), pattern=" ", replacement = "_")

colnames(NYC_parkingTkt_2016)<- str_replace_all(colnames(NYC_parkingTkt_2016), pattern="\\?", replacement = "")

colnames(NYC_parkingTkt_2016)

#Create a view table

createOrReplaceTempView(NYC_parkingTkt_2016, "2016_NYC_tkt")

# Removing duplicates in Summons_Number

#Explore unique summons_number

head(SparkR::sql("select count(distinct Summons_Number) from 2016_NYC_tkt")) # all rows are unique as they match with the total number of rows
#no need top drop any rows
#NYC_parkingTkt_2016<- dropDuplicates(NYC_parkingTkt_2016, "Summons_Number") 

dim(NYC_parkingTkt_2016)

#There are 10626899 rows after removing duplicates.


# Checking ticket "Issue Date"


Issudate_null_2016<- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                 
                                 THEN 1
                                 
                                 ELSE 0
                                 
                                 END) Issue_Date_nullcount,
                                 
                                 COUNT(*) Num_of_Rows
                                 
                                 FROM 2016_NYC_tkt")

head(SparkR::sql("select count(*) from 2016_NYC_tkt where Issue_Date is null "))
head(Issudate_null_2016)

#Issue date do not have any null values in any of the records in 2016

# Converting date columns to same format "MMDDYYYY"

NYC_parkingTkt_2016$Issue_Date <- SparkR::to_date(NYC_parkingTkt_2016$Issue_Date, 'MM/dd/yyyy')

# Checking maximum and minimum of issue date.

createOrReplaceTempView(NYC_parkingTkt_2016, "2016_NYC_tkt")

Issue_Date_2016_Range <- SparkR::sql("SELECT min(Issue_Date)as Min_Issue_Date_2016,
                                     
                                     max(Issue_Date)as Max_Issue_Date_2016
                                     
                                     FROM 2016_NYC_tkt")

head(Issue_Date_2016_Range)

#There are dates entered erroneously as we see below which needs to be corrected 
# Min Issue_Date = 1970-04-13
# Max Issue_Date = 2069-10-02
# Min issue date is from 1985. 

# We will extract the year and month from the issue date to find out only 2016 data.

NYC_parkingTkt_2016$Year_of_issuedt <- year(NYC_parkingTkt_2016$Issue_Date)

NYC_parkingTkt_2016$Month_of_issuedt <- month(NYC_parkingTkt_2016$Issue_Date)

# Let us consider the fiscal year from Jul 1st, 2015 to Jun 30, 2016



NYC_PrkTkt_2016 <- NYC_parkingTkt_2016[
  
  NYC_parkingTkt_2016$Issue_Date >= "2015-07-01" & 
    
    NYC_parkingTkt_2016$Issue_Date <= "2016-06-30"]

nrow(NYC_PrkTkt_2016)

# There are  10396894  records in the above dataset.


#************************************************************************************************************************************#
#The columns "Latitude","Longitude","Community Board","Community Council ","Census Tract","BIN","BBL" and "NTA" are not present  in 2017 dataset.
#So let us check if we can drop these columns from 2016 dataset.

#************************************************************************************************************************************#


createOrReplaceTempView(NYC_PrkTkt_2016, "NYC_PrkTkt_2016_V2")

colnames(NYC_PrkTkt_2016)

ExtraCol_2016_nullcount <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                       
                                       SUM(CASE WHEN Latitude IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Latitude_nulls,
                                       
                                       SUM(CASE WHEN Longitude IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Longitude_nulls,
                                       
                                       SUM(CASE WHEN Community_Board IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Community_Board_nulls,
                                       
                                       SUM(CASE WHEN Community_Council IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Community_Council_nulls,
                                       
                                       SUM(CASE WHEN Census_Tract IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) Census_Tract_nulls,
                                       
                                       SUM(CASE WHEN BIN IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) BIN_nulls,
                                       
                                       SUM(CASE WHEN BBL IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) BBL_nulls,
                                       
                                       SUM(CASE WHEN NTA IS NULL
                                       
                                       THEN 1
                                       
                                       ELSE 0
                                       
                                       END) NTA_nulls     
                                       
                                       FROM NYC_PrkTkt_2016_V2")



head(ExtraCol_2016_nullcount)

#Num_of_Rows Latitude_nulls Longitude_nulls Community_Board_nulls Community_Council_nulls
# 10396894       10396894        10396894              10396894                10396894
# Census_Tract_nulls BIN_nulls BBL_nulls NTA_nulls
#      10396894  10396894  10396894  10396894

# The null values in extra columns in  the 2016 data set is equal to total no. of rows. Hence it is clear that we can drop these extra columns since it contains only nulls

# So removing these extra columns

NYC_PrkTkt_2016<- drop(NYC_PrkTkt_2016, c("Latitude","Longitude","Community_Board", "Community_Council","Census_Tract","BIN" , "BBL",  "NTA") )

colnames(NYC_PrkTkt_2016)


# In Violation Time, we need to append M since it is missing in AM/PM.

NYC_PrkTkt_2016$Append_M <- "M"

NYC_PrkTkt_2016$Violation_Time<-concat(NYC_PrkTkt_2016$Violation_Time,  NYC_PrkTkt_2016$Append_M)

NYC_PrkTkt_2016$Time_First_Observed<- concat(NYC_PrkTkt_2016$Time_First_Observed, NYC_PrkTkt_2016$Append_M)

NYC_PrkTkt_2016$From_Hours_In_Effect<- concat(NYC_PrkTkt_2016$From_Hours_In_Effect, NYC_PrkTkt_2016$Append_M)

NYC_PrkTkt_2016$To_Hours_In_Effect<- concat(NYC_PrkTkt_2016$To_Hours_In_Effect, NYC_PrkTkt_2016$Append_M)

NYC_PrkTkt_2016<- drop(NYC_PrkTkt_2016, c("Append_M"))


# Extract Violation Hour, Violation Minute and AM/PM from Violation Time.

NYC_PrkTkt_2016$Violation_Hour <- substr(NYC_PrkTkt_2016$Violation_Time, 1, 2)

NYC_PrkTkt_2016$Violation_Minute <- substr(NYC_PrkTkt_2016$Violation_Time, 3, 4)

NYC_PrkTkt_2016$Violation_AM_PM <- substr(NYC_PrkTkt_2016$Violation_Time, 5, 6)


# Replacing 00xxAM with 12xxAM in violation time

NYC_PrkTkt_2016$Violation_Hour <- regexp_replace(x = NYC_PrkTkt_2016$Violation_Hour,pattern = "\\00",replacement = "12")

# Apply standardized format to Violation Time

NYC_PrkTkt_2016$Violation_Time <- concat(NYC_PrkTkt_2016$Violation_Hour, NYC_PrkTkt_2016$Violation_Minute, NYC_PrkTkt_2016$Violation_AM_PM)


#Converting Violation Time into a TimeStamp

NYC_PrkTkt_2016$Violation_Time<-to_timestamp(x = NYC_PrkTkt_2016$Violation_Time, format = "hhmma")


#Converting the other time columns into a TimeStamp.

NYC_PrkTkt_2016$Time_First_Observed<- to_timestamp(x= NYC_PrkTkt_2016$Time_First_Observed, format = "hhmma")

NYC_PrkTkt_2016$From_Hours_In_Effect<- to_timestamp(x= NYC_PrkTkt_2016$From_Hours_In_Effect, format = "hhmma")

NYC_PrkTkt_2016$To_Hours_In_Effect<- to_timestamp(x= NYC_PrkTkt_2016$To_Hours_In_Effect, format = "hhmma")

head(NYC_PrkTkt_2016)

dim(NYC_PrkTkt_2016)
#After dat caleanig such as dropping NAs,Removing duplicate Summon_Numbers,subsettign with correct fiscal date the
#no.of columns remained are Rows : 10396894    col:48 

#**************************************************EDA of NYC_parkingTkt_2017**********************************************************************************#

# Removing space in the columns since it was creating issue in the sql query

colnames(NYC_parkingTkt_2017)<- str_trim(colnames(NYC_parkingTkt_2017), side= "both")

colnames(NYC_parkingTkt_2017)<- str_replace_all(colnames(NYC_parkingTkt_2017), pattern=" ", replacement = "_")

colnames(NYC_parkingTkt_2017)<- str_replace_all(colnames(NYC_parkingTkt_2017), pattern="\\?", replacement = "")

colnames(NYC_parkingTkt_2017)

createOrReplaceTempView(NYC_parkingTkt_2017,'2017_NYC_tkt')
#Explore Summons_Number for unique numbers

head(SparkR::sql("select count(distinct Summons_Number) unique,count(*) total from 2017_NYC_tkt"))
#unique    total                                                             
#10803028 10803028
#There are no duplicates as the number of unique Summons_Number matches with the Number of rows of the dataset

# Checking ticket "Issue Date"

createOrReplaceTempView(NYC_parkingTkt_2017, "2017_NYC_tkt")

Issudate_2017__null<- SparkR::sql("SELECT SUM(CASE WHEN Issue_Date IS NULL
                                  
                                  THEN 1
                                  
                                  ELSE 0
                                  
                                  END) Issue_Date_nullcount,
                                  
                                  COUNT(*) Num_of_Rows
                                  
                                  FROM 2017_NYC_tkt")


head(Issudate_2017__null)

#Issue date do not have any null values in any of the records in 2017

# Converting date columns to same format "MMDDYYYY"

NYC_parkingTkt_2017$Issue_Date <- SparkR::to_date(NYC_parkingTkt_2017$Issue_Date, 'MM/dd/yyyy')

# Checking maximum and minimum of issue date.

createOrReplaceTempView(NYC_parkingTkt_2017, "2017_NYC_tkt")

Issue_Date_2017_Range <- SparkR::sql("SELECT min(Issue_Date)as Min_Issue_Date_2017,
                                     
                                     max(Issue_Date)as Max_Issue_Date_2017
                                     
                                     FROM 2017_NYC_tkt")

head(Issue_Date_2017_Range)


# Min Issue_Date = 1972-03-30
# Max Issue_Date = 2069-11-19
# Min issue date is from 1972. 

# Hence we will extract the year and month from the issue date to find out only 2017 data.

NYC_parkingTkt_2017$Year_of_issuedt <- year(NYC_parkingTkt_2017$Issue_Date)

NYC_parkingTkt_2017$Month_of_issuedt <- month(NYC_parkingTkt_2017$Issue_Date)

# Let us consider the fiscal year from Jul 1st, 2016 to Jun 30, 2017


NYC_PrkTkt_2017 <- NYC_parkingTkt_2017[
  
  NYC_parkingTkt_2017$Issue_Date >= "2016-07-01" & 
    
    NYC_parkingTkt_2017$Issue_Date <= "2017-06-30"]
nrow(NYC_PrkTkt_2017)

# There are 10539563 records in the above dataset.

# In Violation Time, we need to append M since it is missing in AM/PM.

NYC_PrkTkt_2017$Append_M <- "M"

NYC_PrkTkt_2017$Violation_Time<-concat(NYC_PrkTkt_2017$Violation_Time,  NYC_PrkTkt_2017$Append_M)

NYC_PrkTkt_2017$Time_First_Observed<- concat(NYC_PrkTkt_2017$Time_First_Observed, NYC_PrkTkt_2017$Append_M)

NYC_PrkTkt_2017$From_Hours_In_Effect<- concat(NYC_PrkTkt_2017$From_Hours_In_Effect, NYC_PrkTkt_2017$Append_M)

NYC_PrkTkt_2017$To_Hours_In_Effect<- concat(NYC_PrkTkt_2017$To_Hours_In_Effect, NYC_PrkTkt_2017$Append_M)

NYC_PrkTkt_2017<- drop(NYC_PrkTkt_2017, c("Append_M"))


# Extract Violation Hour, Violation Minute and AM/PM from Violation Time.

NYC_PrkTkt_2017$Violation_Hour <- substr(NYC_PrkTkt_2017$Violation_Time, 1, 2)

NYC_PrkTkt_2017$Violation_Minute <- substr(NYC_PrkTkt_2017$Violation_Time, 4, 5)

NYC_PrkTkt_2017$Violation_AM_PM <- substr(NYC_PrkTkt_2017$Violation_Time, 6, 7)


# Replacing 00xxAM with 12xxAM in violation time

NYC_PrkTkt_2017$Violation_Hour <- regexp_replace(x = NYC_PrkTkt_2017$Violation_Hour,pattern = "\\00",replacement = "12")

# Apply standardized format to Violation Time

NYC_PrkTkt_2017$Violation_Time <- concat(NYC_PrkTkt_2017$Violation_Hour, NYC_PrkTkt_2017$Violation_Minute, NYC_PrkTkt_2017$Violation_AM_PM)


#Converting Violation Time into a TimeStamp

NYC_PrkTkt_2017$Violation_Time<-to_timestamp(x = NYC_PrkTkt_2017$Violation_Time, format = "hhmma")


#Converting the other time columns into a TimeStamp.

NYC_PrkTkt_2017$Time_First_Observed<- to_timestamp(x= NYC_PrkTkt_2017$Time_First_Observed, format = "hhmma")

NYC_PrkTkt_2017$From_Hours_In_Effect<- to_timestamp(x= NYC_PrkTkt_2017$From_Hours_In_Effect, format = "hhmma")

NYC_PrkTkt_2017$To_Hours_In_Effect<- to_timestamp(x= NYC_PrkTkt_2017$To_Hours_In_Effect, format = "hhmma")

head(NYC_PrkTkt_2017)
dim(NYC_PrkTkt_2017)
#After dat caleanig such as dropping NAs,Removing duplicate Summon_Numbers,subsettign with correct fiscal date the
#no.of columns remained are Rows : 10539563    col:48 

#**************************Solution to Questions asked in the Case study***********************************************
#Cleaned dataframes for each fiscal years i.e 2015,2016,2017 respectively are as below,
#NYC_PrkTkt_2015
#NYC_PrkTkt_2016
#NYC_PrkTkt_2017

#-----------------------------------------------------------------------------------------------------------------------------
#                                            Examine the data 
#-----------------------------------------------------------------------------------------------------------------------------
#Q1.Find the total number of tickets for each year

Total_no_of_tickets_year <- data.frame(FY=c('FY_2015','FY_2016','FY_2017'),Total_tickets=c(nrow(NYC_PrkTkt_2015),nrow(NYC_PrkTkt_2016),
                                                                                           nrow(NYC_PrkTkt_2017)))
head(Total_no_of_tickets_year)
#Ans:
#       FY       Total_tickets
#     FY_2015      10598035
#     FY_2016      10396894
#     FY_2017      10539563

#Plot

ggplot(Total_no_of_tickets_year,aes(x=FY,y=Total_tickets,color=FY))+geom_bar(stat='identity',fill='grey')+
  xlab("Fiscal Year")+ylab("Total No. of Tickets")+ggtitle("Fiscal Year Vs No. of tickets")+
  geom_text(aes(label=Total_tickets),vjust=-0.3)


#Q2.Find out the number of unique states from where the cars got parking tickets

#From EDA we know that there is a Registration_State called '99'.This needs to be replaced by the state with maximum entries

#create a view for SQL analysis for all three cleaned dataframes

createOrReplaceTempView(NYC_PrkTkt_2015,'NYC_PrkTkt_2015_View')
createOrReplaceTempView(NYC_PrkTkt_2016,'NYC_PrkTkt_2016_View')
createOrReplaceTempView(NYC_PrkTkt_2017,'NYC_PrkTkt_2017_View')
#Registration_State will be used for analysis
#For Year 2015
unique_states_2015 <- SparkR::sql("select Registration_State,count(*) as No_tickets_state from NYC_PrkTkt_2015_View 
                                  group by Registration_State order by No_tickets_state desc")
dim(unique_states_2015) #69 rows retrieved which means there are 69 unique states
head(unique_states_2015,70)
#Ans
"1                  NY          8255530
2                  NJ           969222
3                  PA           261192
4                  CT           144719
5                  FL           133980
6                  MA            91211
7                  IN            77473
8                  VA            70479
9                  MD            56946
10                 NC            53083
11                 99            39238
12                 IL            38670
13                 GA            34330
14                 TX            30712
15                 AZ            27674
16                 OH            24787
17                 CA            22484
18                 ME            22430
19                 SC            22117
20                 OK            21735
21                 TN            20630
22                 MI            17964
23                 MN            16804
24                 DE            15520
25                 RI            14180
26                 NH             9284
27                 VT             7560
28                 AL             6974
29                 WA             6329
30                 QB             5798
31                 ON             5721
32                 OR             5586
33                 IA             5522
34                 ID             5371
35                 WI             4959
36                 DP             4747
37                 KY             4247
38                 DC             4198
39                 MS             4134
40                 CO             4112
41                 MO             3653
42                 NM             3429
43                 AR             3267
44                 LA             3158
45                 NV             2268
46                 WV             2133
47                 NE             1846
48                 KS             1502
49                 GV             1400
50                 SD             1322
51                 UT             1109
52                 AK             1023
53                 NS              810
54                 MT              764
55                 ND              564
56                 HI              484
57                 AB              341
58                 WY              322
59                 PR              260
60                 BC              257
61                 PE              196
62                 NB              165
63                 MX               34
64                 MB               28
65                 SK               22
66                 FO                8
67                 NF                7
68                 NT                6
69                 YT                5"

#In 2015 Dataset we have a state named 99 which needs tobe replcaed by NY as NY has maximum entries

NYC_PrkTkt_2015$Registration_State <- ifelse(NYC_PrkTkt_2015$Registration_State %in% c('99'),'NY',NYC_PrkTkt_2015$Registration_State)
createOrReplaceTempView(NYC_PrkTkt_2015,'NYC_PrkTkt_2015_View')
dim(NYC_PrkTkt_2015)
unique_states_2015 <- SparkR::sql("select Registration_State,count(*) as No_tickets_state from NYC_PrkTkt_2015_View 
                                  group by Registration_State order by No_tickets_state desc")
dim(unique_states_2015) #68 rows retrieved which means there are 69 unique states
head(unique_states_2015,70)
"   Registration_State No_tickets_state                                          
1                  NY          8294768
2                  NJ           969222
3                  PA           261192
4                  CT           144719
5                  FL           133980
6                  MA            91211
7                  IN            77473
8                  VA            70479
9                  MD            56946
10                 NC            53083
11                 IL            38670
12                 GA            34330
13                 TX            30712
14                 AZ            27674
15                 OH            24787
16                 CA            22484
17                 ME            22430
18                 SC            22117
19                 OK            21735
20                 TN            20630
21                 MI            17964
22                 MN            16804
23                 DE            15520
24                 RI            14180
25                 NH             9284
26                 VT             7560
27                 AL             6974
28                 WA             6329
29                 QB             5798
30                 ON             5721
31                 OR             5586
32                 IA             5522
33                 ID             5371
34                 WI             4959
35                 DP             4747
36                 KY             4247
37                 DC             4198
38                 MS             4134
39                 CO             4112
40                 MO             3653
41                 NM             3429
42                 AR             3267
43                 LA             3158
44                 NV             2268
45                 WV             2133
46                 NE             1846
47                 KS             1502
48                 GV             1400
49                 SD             1322
50                 UT             1109
51                 AK             1023
52                 NS              810
53                 MT              764
54                 ND              564
55                 HI              484
56                 AB              341
57                 WY              322
58                 PR              260
59                 BC              257
60                 PE              196
61                 NB              165
62                 MX               34
63                 MB               28
64                 SK               22
65                 FO                8
66                 NF                7
67                 NT                6
68                 YT                5"
#For year 2016
unique_states_2016 <- SparkR::sql("select Registration_State,count(*) as No_tickets_state from NYC_PrkTkt_2016_View 
                                  group by Registration_State order by No_tickets_state desc")
dim(unique_states_2016) #69 rows retrieved which means there are 69 unique states
head(unique_states_2016,70)
" Registration_State No_tickets_state                                          
1                  NY          8083903
2                  NJ           949163
3                  PA           252681
4                  CT           142006
5                  FL           135273
6                  MA            96866
7                  IN            79629
8                  VA            73298
9                  MD            58735
10                 NC            54177
11                 99            39656
12                 IL            36289
13                 GA            34305
14                 TX            31905
15                 AZ            25724
16                 ME            23646
17                 OH            23203
18                 CA            23032
19                 OK            21270
20                 SC            20612
21                 TN            18422
22                 MI            16959
23                 DE            15466
24                 MN            15161
25                 RI            12909
26                 NH             9425
27                 AL             7586
28                 WA             6786
29                 VT             6619
30                 OR             6437
31                 ON             5436
32                 QB             4938
33                 ID             4482
34                 WI             4450
35                 KY             4362
36                 IA             4253
37                 DC             4175
38                 MS             4021
39                 DP             4011
40                 CO             3830
41                 MO             3772
42                 NM             3209
43                 AR             3107
44                 LA             3053
45                 WV             2700
46                 NV             2403
47                 SD             2001
48                 NE             1690
49                 UT             1442
50                 KS             1383
51                 NS             1352
52                 GV             1318
53                 AK              922
54                 MT              888
55                 ND              685
56                 HI              558
57                 WY              339
58                 AB              269
59                 PR              217
60                 BC              155
61                 NB              133
62                 PE              122
63                 MB               31
64                 SK               18
65                 FO               13
66                 MX                6
67                 YT                5
68                 NT                2"
#There is a registration state called 99 and that needs to be replaced by NY as NY has aximum entries
NYC_PrkTkt_2016$Registration_State <- ifelse(NYC_PrkTkt_2016$Registration_State %in% c('99'),'NY',NYC_PrkTkt_2016$Registration_State)
createOrReplaceTempView(NYC_PrkTkt_2016,'NYC_PrkTkt_2016_View')
unique_states_2016 <- SparkR::sql("select Registration_State,count(*) as No_tickets_state from NYC_PrkTkt_2016_View 
                                  group by Registration_State order by No_tickets_state desc")
dim(unique_states_2016) #67 rows retrieved which means there are 68 unique states
head(unique_states_2016,70)
"   Registration_State No_tickets_state                                          
1                  NY          8123559
2                  NJ           949163
3                  PA           252681
4                  CT           142006
5                  FL           135273
6                  MA            96866
7                  IN            79629
8                  VA            73298
9                  MD            58735
10                 NC            54177
11                 IL            36289
12                 GA            34305
13                 TX            31905
14                 AZ            25724
15                 ME            23646
16                 OH            23203
17                 CA            23032
18                 OK            21270
19                 SC            20612
20                 TN            18422
21                 MI            16959
22                 DE            15466
23                 MN            15161
24                 RI            12909
25                 NH             9425
26                 AL             7586
27                 WA             6786
28                 VT             6619
29                 OR             6437
30                 ON             5436
31                 QB             4938
32                 ID             4482
33                 WI             4450
34                 KY             4362
35                 IA             4253
36                 DC             4175
37                 MS             4021
38                 DP             4011
39                 CO             3830
40                 MO             3772
41                 NM             3209
42                 AR             3107
43                 LA             3053
44                 WV             2700
45                 NV             2403
46                 SD             2001
47                 NE             1690
48                 UT             1442
49                 KS             1383
50                 NS             1352
51                 GV             1318
52                 AK              922
53                 MT              888
54                 ND              685
55                 HI              558
56                 WY              339
57                 AB              269
58                 PR              217
59                 BC              155
60                 NB              133
61                 PE              122
62                 MB               31
63                 SK               18
64                 FO               13
65                 MX                6
66                 YT                5
67                 NT                2"  
#For year 2017
unique_states_2017 <- SparkR::sql("select Registration_State,count(*) as No_tickets_state from NYC_PrkTkt_2017_View 
                                  group by Registration_State order by No_tickets_state desc")
dim(unique_states_2017) #67 rows retrieved which means there are 69 unique states
head(unique_states_2017,70)
"   Registration_State No_tickets_state                                          
1                  NY          8272874
2                  NJ           905942
3                  PA           278005
4                  FL           140624
5                  CT           137963
6                  MA            83377
7                  IN            79351
8                  VA            70686
9                  MD            60242
10                 NC            54309
11                 IL            36524
12                 GA            35755
13                 TX            35722
14                 99            34720
15                 AZ            25669
16                 OH            24681
17                 CA            23736
18                 SC            21176
19                 ME            21002
20                 MN            17929
21                 OK            17749
22                 TN            16862
23                 DE            15936
24                 MI            15340
25                 RI            11978
26                 NH             8561
27                 VT             7200
28                 AL             6671
29                 WA             6201
30                 ON             5500
31                 OR             5382
32                 MO             4384
33                 QB             4311
34                 DC             4168
35                 IA             4110
36                 WI             4093
37                 CO             4005
38                 KY             3726
39                 MS             3501
40                 LA             3388
41                 DP             3121
42                 ID             2839
43                 AR             2542
44                 WV             2477
45                 NM             2286
46                 NV             1800
47                 SD             1797
48                 KS             1465
49                 NE             1424
50                 UT             1123
51                 MT             1013
52                 AK              735
53                 NS              718
54                 GV              678
55                 ND              593
56                 WY              404
57                 HI              396
58                 AB              214
59                 NB              134
60                 PE              128
61                 BC              126
62                 PR              108
63                 MB               40
64                 SK               30
65                 FO               11
66                 MX                4
67                 NT                4"

dim(NYC_PrkTkt_2017)
NYC_PrkTkt_2017$Registration_State <- ifelse(NYC_PrkTkt_2017$Registration_State %in% c('99'),'NY',NYC_PrkTkt_2017$Registration_State)
createOrReplaceTempView(NYC_PrkTkt_2017,'NYC_PrkTkt_2017_View')
unique_states_2017 <- SparkR::sql("select Registration_State,count(*) as No_tickets_state from NYC_PrkTkt_2017_View 
                                  group by Registration_State order by No_tickets_state desc")
dim(unique_states_2017) #66 rows retrieved which means there are 69 unique states
head(unique_states_2017,70)
"   Registration_State No_tickets_state                                          
1                  NY          8307594
2                  NJ           905942
3                  PA           278005
4                  FL           140624
5                  CT           137963
6                  MA            83377
7                  IN            79351
8                  VA            70686
9                  MD            60242
10                 NC            54309
11                 IL            36524
12                 GA            35755
13                 TX            35722
14                 AZ            25669
15                 OH            24681
16                 CA            23736
17                 SC            21176
18                 ME            21002
19                 MN            17929
20                 OK            17749
21                 TN            16862
22                 DE            15936
23                 MI            15340
24                 RI            11978
25                 NH             8561
26                 VT             7200
27                 AL             6671
28                 WA             6201
29                 ON             5500
30                 OR             5382
31                 MO             4384
32                 QB             4311
33                 DC             4168
34                 IA             4110
35                 WI             4093
36                 CO             4005
37                 KY             3726
38                 MS             3501
39                 LA             3388
40                 DP             3121
41                 ID             2839
42                 AR             2542
43                 WV             2477
44                 NM             2286
45                 NV             1800
46                 SD             1797
47                 KS             1465
48                 NE             1424
49                 UT             1123
50                 MT             1013
51                 AK              735
52                 NS              718
53                 GV              678
54                 ND              593
55                 WY              404
56                 HI              396
57                 AB              214
58                 NB              134
59                 PE              128
60                 BC              126
61                 PR              108
62                 MB               40
63                 SK               30
64                 FO               11
65                 MX                4
66                 NT                4"

#Plot number of unique states 

df <- data.frame(Yr=c('yr_2015','yr_2016','yr_2017'),no_of_states=c(nrow(unique_states_2015),nrow(unique_states_2016),nrow(unique_states_2017)))
ggplot(df,aes(x=Yr,y=no_of_states,color=Yr))+geom_bar(stat='identity',fill='grey')+geom_text(aes(label=no_of_states),vjust=-0.3)+ 
  xlab("Fiscal Year") + ylab("Count of unique Registration States") + ggtitle("Fiscal Year vs. Count of Unique Registration States")
#Sense check all three datasets
dim(NYC_PrkTkt_2015) #10598035       48  
dim(NYC_PrkTkt_2016) #10396894       48  
dim(NYC_PrkTkt_2017) #10539563       48 
#Views created NYC_PrkTkt_2015_View,NYC_PrkTkt_2016_View,NYC_PrkTkt_2017_View


#--------------------Q3. Some parking tickets don’t have the address for violation location on them, which is a cause for concern. 
colnames(NYC_PrkTkt_2015)
head(NYC_PrkTkt_2015)
#Write a query to check the number of such tickets.

#Missing Address in 2015

Missing_Address_2015 <- SparkR::sql("SELECT count(*) as Total_No_Records, 
                                    
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    
                                    THEN 1 
                                    
                                    ELSE 0 END)as 2015_MissingAddress_tickets, 
                                    
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    
                                    THEN 1 
                                    
                                    ELSE 0 
                                    
                                    END)/count(*) as Percent_Tickets_2015_with_MissingAddress
                                    
                                    from NYC_PrkTkt_2015_View")

head(Missing_Address_2015)
#Total_No_Records 2015_MissingAddress_tickets Percent_Tickets_2015_with_MissingAddress
#1         10598035                     1622076                                 15.30544
# 15.30544 % of the dataset have missing addresses

Missing_Addrs_2015<- data.frame(head(Missing_Address_2015))

Missing_Addrs_2015$Fiscal_Year<- 2015
dim(Missing_Addrs_2015)
colnames(Missing_Addrs_2015)<- c("Total_No_Records", "Count_of_Tickets_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")
head(Missing_Addrs_2015)




#Missing Address in 2016

Missing_Address_2016 <- SparkR::sql("SELECT count(*) as Total_No_Records, 
                                    
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    
                                    THEN 1 
                                    
                                    ELSE 0 END)as 2016_MissingAddress_tickets, 
                                    
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    
                                    THEN 1 
                                    
                                    ELSE 0 
                                    
                                    END)/count(*) as Percent_Tickets_2016_with_MissingAddress
                                    
                                    from NYC_PrkTkt_2016_View")

head(Missing_Address_2016)
#Total_No_Records 2016_MissingAddress_tickets Percent_Tickets_2016_with_MissingAddress
#1         10396894                     1963921                                  18.8895
#18.8895 % of the dataset have missing values

Missing_Addrs_2016<- data.frame(head(Missing_Address_2016))

Missing_Addrs_2016$Fiscal_Year<- 2016

colnames(Missing_Addrs_2016)<- c("Total_No_Records", "Count_of_Tickets_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")





#Missing Address in 2017

Missing_Address_2017 <- SparkR::sql("SELECT count(*) as Total_No_Records, 
                                    
                                    SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL 
                                    
                                    THEN 1 
                                    
                                    ELSE 0 END)as 2017_MissingAddress_tickets, 
                                    
                                    100*SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL
                                    
                                    THEN 1 
                                    
                                    ELSE 0 
                                    
                                    END)/count(*) as Percent_Tickets_2017_with_MissingAddress
                                    
                                    from NYC_PrkTkt_2017_View")

head(Missing_Address_2017)



Missing_Addrs_2017<- data.frame(head(Missing_Address_2017))

Missing_Addrs_2017$Fiscal_Year<- 2017

colnames(Missing_Addrs_2017)<- c("Total_No_Records", "Count_of_Tickets_MissingAddress", "Percent_Tickets_with_MissingAddress", "Fiscal_Year")



#Comparison of Missing Addresses in each year

Combined_Missing_Address<- rbind(Missing_Addrs_2015, Missing_Addrs_2016, Missing_Addrs_2017)

Combined_Missing_Address

ggplot(Combined_Missing_Address, aes(x=Fiscal_Year, y=Count_of_Tickets_MissingAddress))+ geom_col() + xlab("Fiscal Year") + ylab("Count of tickets with Missing Address") + ggtitle("Fiscal Year vs. Count of Missing Address Tickets") + geom_text(aes(label=Count_of_Tickets_MissingAddress),vjust=-0.3)

#-----------------------------------------------------------------------------------------------------------------------------
#                                            Aggregation tasks
#-----------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------
#                                                     Question 1
#-----------------------------------------------------------------------------------------------------------------------------


#Q1. How often does each violation code occur? Display the frequency of the top five violation codes.


# Top Violation Codes for 2015

violationcode_frequency_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                           
                                           from NYC_PrkTkt_2015_View 
                                           
                                           group by Violation_Code
                                           
                                           order by Frequency_of_Tickets desc")
dim(violationcode_frequency_2015)
head(violationcode_frequency_2015,5)
"
  Violation_Code Frequency_of_Tickets                                           
1             21              1469228
2             38              1305007
3             14               908418
4             36               747098
5             37               735600
"


Top5_violncd_2015<- data.frame(head(violationcode_frequency_2015,5))


ggplot(Top5_violncd_2015, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col(fill="yellow") + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Top 5 Violation Code in 2015 vs Frequency of Ticket") + 
  geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


# Top Violation Codes for 2016

violationcode_frequency_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                           
                                           from NYC_PrkTkt_2016_View 
                                           
                                           group by Violation_Code
                                           
                                           order by Frequency_of_Tickets desc")

head(violationcode_frequency_2016,5)
"  Violation_Code Frequency_of_Tickets                                           
1             21              1497269
2             36              1232952
3             38              1126835
4             14               860045
5             37               677805"


Top5_violncd_2016<- data.frame(head(violationcode_frequency_2016,5))


ggplot(Top5_violncd_2016, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col(fill="orange") + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Top 5 Violation Code in 2016 vs Frequency of Ticket") + 
  geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



# Top Violation Codes for 2017

violationcode_frequency_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                           
                                           from NYC_PrkTkt_2017_View 
                                           
                                           group by Violation_Code
                                           
                                           order by Frequency_of_Tickets desc")

head(violationcode_frequency_2017,5)
"  Violation_Code Frequency_of_Tickets                                           
1             21              1500396
2             36              1345237
3             38              1050418
4             14               880152
5             20               609231"


Top5_violncd_2017<- data.frame(head(violationcode_frequency_2017,5))


ggplot(Top5_violncd_2017, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col(fill="green") + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Top 5 Violation Code in 2017 vs Frequency of Ticket") + 
  geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



# Comparison of Top-5 Violation Codes in each year

Combined_volationcode<- rbind(Top5_violncd_2015, Top5_violncd_2016, Top5_violncd_2017)

Combined_volationcode$fiscal_year<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")

ggplot(Combined_volationcode, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets,color=fiscal_year))+ geom_col(fill="white")+ facet_grid(~fiscal_year) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Comparison of Top 5 Violation Code vs Frequency of Tickets between Fiscal Years") + 
  geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#-----------------------------------------------------------------------------------------------------------------------------
#                                                     Question 2
#-----------------------------------------------------------------------------------------------------------------------------

#Q2.1 How often does each 'vehicle body type' get a parking ticket?

#Top Vehicle Body Type for 2015

vehiclebodytype_frequency_2015<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                             
                                             from NYC_PrkTkt_2015_View 
                                             
                                             group by Vehicle_Body_Type
                                             
                                             order by Frequency_of_Tickets desc")

head(vehiclebodytype_frequency_2015,5)
"  Vehicle_Body_Type Frequency_of_Tickets                                        
1              SUBN              3341110
2              4DSD              3001810
3               VAN              1570227
4              DELV               822040
5               SDN               428571"


Top5_vehiclebdtype_2015<- data.frame(head(vehiclebodytype_frequency_2015,5))



ggplot(Top5_vehiclebdtype_2015, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets,color="blue"))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("2015 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#Top Vehicle Body Type for 2016

vehiclebodytype_frequency_2016<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                             
                                             from NYC_PrkTkt_2016_View 
                                             
                                             group by Vehicle_Body_Type
                                             
                                             order by Frequency_of_Tickets desc")

head(vehiclebodytype_frequency_2016,5)
"  Vehicle_Body_Type Frequency_of_Tickets                                        
1              SUBN              3393838
2              4DSD              2936729
3               VAN              1489924
4              DELV               738747
5               SDN               401750"


Top5_vehiclebdtype_2016<- data.frame(head(vehiclebodytype_frequency_2016,5))



ggplot(Top5_vehiclebdtype_2016, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets,color="green"))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("2016 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#Top Vehicle Body Type for 2017

vehiclebodytype_frequency_2017<- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as Frequency_of_Tickets
                                             
                                             from NYC_PrkTkt_2017_View 
                                             
                                             group by Vehicle_Body_Type
                                             
                                             order by Frequency_of_Tickets desc")

head(vehiclebodytype_frequency_2017,5)

"  Vehicle_Body_Type Frequency_of_Tickets                                        
1              SUBN              3632003
2              4DSD              3017372
3               VAN              1384121
4              DELV               672123
5               SDN               414984"

Top5_vehiclebdtype_2017<- data.frame(head(vehiclebodytype_frequency_2017,5))



ggplot(Top5_vehiclebdtype_2017, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets,color="orange"))+ geom_col() + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("2017 Top 5 Vehicle Body Type vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#Combined Comparison for Top-5 Body types

Combined_vehiclebody<- rbind(Top5_vehiclebdtype_2015, Top5_vehiclebdtype_2016, Top5_vehiclebdtype_2017)

Combined_vehiclebody$Fiscal_year<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")


ggplot(Combined_vehiclebody, aes(x=as.factor(Vehicle_Body_Type), y=Frequency_of_Tickets,color=Fiscal_year))+ geom_col(fill="white")+ facet_grid(~Fiscal_year) + xlab("Vehicle Body Type") + ylab("Frequency of Tickets") + ggtitle("Comparison of Top 5 Violation Bodytype vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)




#Q2.2 How about the 'vehicle make'? 


#Top Vehicle Make for 2015

vehiclemake_frequency_2015<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                         
                                         from NYC_PrkTkt_2015_View 
                                         
                                         group by Vehicle_Make
                                         
                                         order by Frequency_of_Tickets desc")

head(vehiclemake_frequency_2015, 5)
"  Vehicle_Make Frequency_of_Tickets                                             
1         FORD              1373157
2        TOYOT              1082206
3        HONDA               982130
4        CHEVR               811659
5        NISSA               805572"


Top5_vehiclemake_2015<- data.frame(head(vehiclemake_frequency_2015, 5))



ggplot(Top5_vehiclemake_2015, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("2015 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Top Vehicle Make for 2016

vehiclemake_frequency_2016<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                         
                                         from NYC_PrkTkt_2016_View 
                                         
                                         group by Vehicle_Make
                                         
                                         order by Frequency_of_Tickets desc")

head(vehiclemake_frequency_2016, 5)
"  Vehicle_Make Frequency_of_Tickets                                             
1         FORD              1297363
2        TOYOT              1128909
3        HONDA               991735
4        NISSA               815963
5        CHEVR               743416"


Top5_vehiclemake_2016<- data.frame(head(vehiclemake_frequency_2016, 5))



ggplot(Top5_vehiclemake_2016, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("2016 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#Top Vehicle Make for 2017

vehiclemake_frequency_2017<- SparkR::sql("SELECT Vehicle_Make, count(*)as Frequency_of_Tickets
                                         
                                         from NYC_PrkTkt_2017_View 
                                         
                                         group by Vehicle_Make
                                         
                                         order by Frequency_of_Tickets desc")

head(vehiclemake_frequency_2017, 5)
"  Vehicle_Make Frequency_of_Tickets                                             
1         FORD              1250777
2        TOYOT              1179265
3        HONDA              1052006
4        NISSA               895225
5        CHEVR               698024"


Top5_vehiclemake_2017<- data.frame(head(vehiclemake_frequency_2017, 5))



ggplot(Top5_vehiclemake_2017, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col() + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("2017 Top 5 Vehicle Make vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#Combined Comparison for Top-5 Body Makes

Combined_vehicle_make<- rbind(Top5_vehiclemake_2015, Top5_vehiclemake_2016, Top5_vehiclemake_2017)

Combined_vehicle_make$FiscalYear<- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")

ggplot(Combined_vehicle_make, aes(x=as.factor(Vehicle_Make), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~FiscalYear) + xlab("Vehicle Make") + ylab("Frequency of Tickets") + ggtitle("Comparison of Top 5 Vehicle Make vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#-----------------------------------------------------------------------------------------------------------------------------
#                                                     Question 3
#-----------------------------------------------------------------------------------------------------------------------------
#A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:

#Violating Precincts (this is the precinct of the zone where the violation occurred)

#Issuing Precincts (this is the precinct that issued the ticket)



#Violation Precinct vs Frequency of Tickets

#Top Violation Precinct for 2015

violnprect_frequency_2015<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                        
                                        from NYC_PrkTkt_2015_View 
                                        
                                        group by Violation_Precinct
                                        
                                        order by Frequency_of_Tickets desc")

head(violnprect_frequency_2015,6)
"Violation_Precinct Frequency_of_Tickets                                       
1                  0              1455166
2                 19               550797
3                 18               393802
4                 14               377750
5                  1               302737
6                114               295855"


vioprect_2015_top5<- data.frame(head(violnprect_frequency_2015,6))

ggplot(vioprect_2015_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("2015 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Top Violation Precinct for 2016

violnprect_frequency_2016<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                        
                                        from NYC_PrkTkt_2016_View 
                                        
                                        group by Violation_Precinct
                                        
                                        order by Frequency_of_Tickets desc")

head(violnprect_frequency_2016,6)
"  Violation_Precinct Frequency_of_Tickets                                       
1                  0              1807139
2                 19               545669
3                 18               325559
4                 14               318193
5                  1               299074
6                114               286741"


vioprect_2016_top5<- data.frame(head(violnprect_frequency_2016,6))



ggplot(vioprect_2016_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("2016 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#Top Violation Precinct for 2017

violnprect_frequency_2017<- SparkR::sql("SELECT Violation_Precinct, count(*)as Frequency_of_Tickets
                                        
                                        from NYC_PrkTkt_2017_View 
                                        
                                        group by Violation_Precinct
                                        
                                        order by Frequency_of_Tickets desc")

head(violnprect_frequency_2017,6)

"  Violation_Precinct Frequency_of_Tickets                                       
1                  0              1950083
2                 19               528317
3                 14               347736
4                  1               326961
5                 18               302008
6                114               292682"

vioprect_2017_top5<- data.frame(head(violnprect_frequency_2017,6))



ggplot(vioprect_2017_top5, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violation Precinct") + ylab("Frequency of Tickets") + ggtitle("2017 Top 5 Violation Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#Combined Comparison for Violation Precinct

vioprect_combined<- rbind(vioprect_2015_top5,vioprect_2016_top5,vioprect_2017_top5)
head(vioprect_combined,20)
vioprect_combined$Fiscal_year<- c("2015","2015","2015","2015", "2015","2015","2016","2016","2016","2016", "2016","2016","2017","2017","2017","2017", "2017","2017")

ggplot(vioprect_combined, aes(x=as.factor(Violation_Precinct), y=Frequency_of_Tickets))+ geom_col()+ 
  facet_grid(~Fiscal_year) + xlab("Violation Precinct") + ylab("Frequency of Tickets") + 
  ggtitle("Comparison of Top 5 Violation Precinct vs Frequency of Ticket between Fiscal Years") + 
  geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Issuer Precinct vs Frequency of Tickets

#Top Issuer Precinct for 2015

isuprect_frequency_2015<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      
                                      from NYC_PrkTkt_2015_View 
                                      
                                      group by Issuer_Precinct
                                      
                                      order by Frequency_of_Tickets desc")

head(isuprect_frequency_2015,6)
"  Issuer_Precinct Frequency_of_Tickets                                          
1               0              1648671
2              19               536627
3              18               384863
4              14               363734
5               1               293942
6             114               291100"


isuprect_2015_top5<- data.frame(head(isuprect_frequency_2015,6))

ggplot(isuprect_2015_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("2015 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


# Top Issuer Precinct for 2016

isuprect_frequency_2016<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      
                                      from NYC_PrkTkt_2016_View 
                                      
                                      group by Issuer_Precinct
                                      
                                      order by Frequency_of_Tickets desc")

head(isuprect_frequency_2016,6)
"  Issuer_Precinct Frequency_of_Tickets                                          
1               0              2067219
2              19               532298
3              18               317451
4              14               309727
5               1               290472
6             114               282493"
isuprect_2016_top5<- data.frame(head(isuprect_frequency_2016,6))

ggplot(isuprect_2016_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("2016 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Top Issuer Precinct for 2017

isuprect_frequency_2017<- SparkR::sql("SELECT Issuer_Precinct, count(*)as Frequency_of_Tickets
                                      
                                      from NYC_PrkTkt_2017_View 
                                      
                                      group by Issuer_Precinct
                                      
                                      order by Frequency_of_Tickets desc")

head(isuprect_frequency_2017,6)
"  Issuer_Precinct Frequency_of_Tickets                                          
1               0              2255086
2              19               514786
3              14               340862
4               1               316776
5              18               292237
6             114               286316"
isuprect_2017_top5<- data.frame(head(isuprect_frequency_2017,6))

ggplot(isuprect_2017_top5, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issuer Precinct") + ylab("Frequency of Tickets") + ggtitle("2017 Top 5 Issuer Precinct vs Frequency of Ticket") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#Combined Comparison for Issuer Precinct

isuprect_combined<- rbind(isuprect_2015_top5, isuprect_2016_top5, isuprect_2017_top5)

isuprect_combined$Fiscal_year<- c("2015","2015","2015","2015", "2015","2015","2016","2016","2016","2016", "2016","2016","2017","2017","2017","2017", "2017","2017")
head(isuprect_combined,20)
ggplot(isuprect_combined, aes(x=as.factor(Issuer_Precinct), y=Frequency_of_Tickets))+ 
  geom_col()+ facet_grid(~Fiscal_year) + xlab("Issuer Precinct") + ylab("Frequency of Tickets")+
  ggtitle("Comparison of Top 5 Issuer Precinct vs Frequency of Ticket between Fiscal Years")+
  geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#-----------------------------------------------------------------------------------------------------------------------------
#                                                     Question 4
#-----------------------------------------------------------------------------------------------------------------------------

#Find the violation code frequency across three precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high #frequency of certain violation codes? Are these codes common across precincts? 

#In Year 2015 [Top Three Issuer Precinct's : 19 , 18 and 14] (precint zero is ignored as it is not valid)

#Violation Code Distribution in Issuer Precinct 19

one_issueruprecinct_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                       
                                       from NYC_PrkTkt_2015_View 
                                       
                                       where Issuer_Precinct = 19
                                       
                                       group by Violation_Code, Issuer_Precinct
                                       
                                       order by Frequency_of_Tickets desc")
dim(one_issueruprecinct_2015)
head(one_issueruprecinct_2015,5)
"  Violation_Code Frequency_of_Tickets Issuer_Precinct                           
1             38                89102              19
2             37                78716              19
3             14                59915              19
4             16                55762              19
5             21                55296              19"


one_isuprect_top5_2015<- data.frame(head(one_issueruprecinct_2015, 5))



#Violation Code Distribution in Issuer Precinct 18

two_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                
                                from NYC_PrkTkt_2015_View 
                                
                                where Issuer_Precinct = 18
                                
                                group by Violation_Code, Issuer_Precinct
                                
                                order by Frequency_of_Tickets desc")

head(two_isuprect_2015, 5)

"  Violation_Code Frequency_of_Tickets Issuer_Precinct                           
1             14               119078              18
2             69                56436              18
3             31                30030              18
4             47                28724              18
5             42                19522              18"


two_isuprect_top5_2015<- data.frame(head(two_isuprect_2015, 5))



#Violation Code Distribution in Issuer Precinct 14

three_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  
                                  from NYC_PrkTkt_2015_View 
                                  
                                  where Issuer_Precinct = 14
                                  
                                  group by Violation_Code, Issuer_Precinct
                                  
                                  order by Frequency_of_Tickets desc")

head(three_isuprect_2015,5)
"  Violation_Code Frequency_of_Tickets Issuer_Precinct                           
1             69                79330              14
2             14                75985              14
3             31                40410              14
4             42                27755              14
5             47                26811              14"


three_isuprect_top5_2015<- data.frame(head(three_isuprect_2015,5))



#Violation Code Distribution in Other Issuer Precincts

other_isuprect_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  
                                  from NYC_PrkTkt_2015_View 
                                  
                                  where Issuer_Precinct NOT IN (0,19,18,14)
                                  
                                  group by Violation_Code
                                  
                                  order by Frequency_of_Tickets desc")

head(other_isuprect_2015,5)



other_isuprect_top5_2015<- data.frame(head(other_isuprect_2015,5))

other_isuprect_top5_2015$Issuer_Precinct<- c("Other","Other","Other","Other","Other")



#Combined Violation Code Distribution vs Issuer Precincts in 2015



#combined_violnissueprect_2015<- rbind(one_isuprect_top5_2015, two_isuprect_top5_2015, three_isuprect_top5_2015, other_isuprect_top5_2015)
combined_violnissueprect_2015<- rbind(one_isuprect_top5_2015, two_isuprect_top5_2015, three_isuprect_top5_2015)


head(combined_violnissueprect_2015,20)

ggplot(combined_violnissueprect_2015, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets,colour=Violation_Code))+ 
  geom_col(fill="white")+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + 
  ggtitle("2015 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + 
  geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)
#Violation code 14 is common across Precinct 14,18,19
#69,14,31 are the top three in precinct 14
#69,14,31 are again top 3 in precinct 18
#38,37,14 are top 3 in precinct 19

#In Year 2016 [Top Three Issuer Precinct's : 19,18 and 14]

#Violation Code Distribution in Issuer Precinct 19

one_issueruprecinct_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                       
                                       from NYC_PrkTkt_2016_View 
                                       
                                       where Issuer_Precinct = 19
                                       
                                       group by Violation_Code, Issuer_Precinct
                                       
                                       order by Frequency_of_Tickets desc")

head(one_issueruprecinct_2016, 5)



one_isuprect_top5_2016<- data.frame(head(one_issueruprecinct_2016, 5))



#Violation Code Distribution in Issuer Precinct 18

two_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                
                                from NYC_PrkTkt_2016_View 
                                
                                where Issuer_Precinct = 18
                                
                                group by Violation_Code, Issuer_Precinct
                                
                                order by Frequency_of_Tickets desc")

head(two_isuprect_2016, 5)



two_isuprect_top5_2016<- data.frame(head(two_isuprect_2016, 5))



#Violation Code Distribution in Issuer Precinct 14

three_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  
                                  from NYC_PrkTkt_2016_View 
                                  
                                  where Issuer_Precinct = 14
                                  
                                  group by Violation_Code, Issuer_Precinct
                                  
                                  order by Frequency_of_Tickets desc")

head(three_isuprect_2016,5)



three_isuprect_top5_2016<- data.frame(head(three_isuprect_2016,5))



#Violation Code Distribution in Other Issuer Precincts

other_isuprect_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  
                                  from NYC_PrkTkt_2016_View 
                                  
                                  where Issuer_Precinct NOT IN (19,18,14)
                                  
                                  group by Violation_Code
                                  
                                  order by Frequency_of_Tickets desc")

head(other_isuprect_2016,5)



other_isuprect_top5_2016<- data.frame(head(other_isuprect_2016,5))

other_isuprect_top5_2016$Issuer_Precinct<- c("Other","Other","Other","Other","Other")



#Combined Violation Code Distribution vs Issuer Precincts in 2016



#combined_violnissueprect_2016<- rbind(one_isuprect_top5_2016, two_isuprect_top5_2016, three_isuprect_top5_2016, other_isuprect_top5_2016)

combined_violnissueprect_2016<- rbind(one_isuprect_top5_2016, two_isuprect_top5_2016, three_isuprect_top5_2016)


ggplot(combined_violnissueprect_2016, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets,color=Violation_Code))+ geom_col()+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2016 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#69,14,31 are top 3 in precinct 14
#14,69,47 are top 3 in precinct 18
#38,37,46 are top 3 in precinct 19

#In Year 2017 [Top Three Issuer Precinct's : 19 , 14 and 1]



#Violation Code Distribution in Issuer Precinct 19

one_issueruprecinct_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                       
                                       from NYC_PrkTkt_2017_View 
                                       
                                       where Issuer_Precinct = 19
                                       
                                       group by Violation_Code, Issuer_Precinct
                                       
                                       order by Frequency_of_Tickets desc")

head(one_issueruprecinct_2017, 5)



one_isuprect_top5_2017<- data.frame(head(one_issueruprecinct_2017, 5))



#Violation Code Distribution in Issuer Precinct 14

two_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                
                                from NYC_PrkTkt_2017_View 
                                
                                where Issuer_Precinct = 14
                                
                                group by Violation_Code, Issuer_Precinct
                                
                                order by Frequency_of_Tickets desc")

head(two_isuprect_2017, 5)



two_isuprect_top5_2017<- data.frame(head(two_isuprect_2017, 5))



#Violation Code Distribution in Issuer Precinct 1

three_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets, Issuer_Precinct
                                  
                                  from NYC_PrkTkt_2017_View 
                                  
                                  where Issuer_Precinct = 1
                                  
                                  group by Violation_Code, Issuer_Precinct
                                  
                                  order by Frequency_of_Tickets desc")

head(three_isuprect_2017,5)



three_isuprect_top5_2017<- data.frame(head(three_isuprect_2017,5))



#Violation Code Distribution in Other Issuer Precincts

other_isuprect_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                  
                                  from NYC_PrkTkt_2017_View 
                                  
                                  where Issuer_Precinct NOT IN (1,19,14)
                                  
                                  group by Violation_Code
                                  
                                  order by Frequency_of_Tickets desc")

head(other_isuprect_2017,5)



other_isuprect_top5_2017<- data.frame(head(other_isuprect_2017,5))

other_isuprect_top5_2017$Issuer_Precinct<- c("Other","Other","Other","Other","Other")



#Combined Violation Code Distribution vs Issuer Precincts in 2017



#combined_violnissueprect_2017<- rbind(one_isuprect_top5_2017, two_isuprect_top5_2017, three_isuprect_top5_2017, other_isuprect_top5_2017)

combined_violnissueprect_2017<- rbind(one_isuprect_top5_2017, two_isuprect_top5_2017, three_isuprect_top5_2017)


ggplot(combined_violnissueprect_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col(fill="blue")+ facet_grid(~Issuer_Precinct) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2017 Comparison of Violation Code Distribution vs. Top Issuer Precinct") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)
#top 3 violation codes in precinct 1 are 14,16,20
#top 3 violation codes in precinct 14 are 14,69,31
#top 3 violation codes in precinct 19 are 46,37,38 .Also 14 is seen in this precinct as 4th
#violation code 14 is common across all 3 precincts

#-----------------------------------------------------------------------------------------------------------------------------
#                                                     Question 5
#-----------------------------------------------------------------------------------------------------------------------------

#Find a way to deal with missing values, if any.

null_violation_time_in_2015<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                          
                                          SUM(CASE WHEN Violation_Time is NULL
                                          
                                          THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                          
                                          100*SUM(CASE WHEN Violation_Time IS NULL
                                          
                                          THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2015_ViolationTimeMissing
                                          
                                          from NYC_PrkTkt_2015_View")

head(null_violation_time_in_2015)

#2015 dataset 0.5812% records with Missing Violation Time is Negligable and will therefore be removed before analysis.



adjusted_NYC_PrkTkt_2015_v3parking<- subset(NYC_PrkTkt_2015, isNotNull(NYC_PrkTkt_2015$Violation_Time))

adjusted_NYC_PrkTkt_2015_v3parking$Violation_Hour <- hour(cast(adjusted_NYC_PrkTkt_2015_v3parking$Violation_Time,dataType = "string"))

createOrReplaceTempView(adjusted_NYC_PrkTkt_2015_v3parking, "violntkt_2015")





#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 

Bin_violation_hour_2015 <- SparkR::sql("SELECT Violation_Hour,
                                       
                                       Violation_Code,
                                       
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       
                                       THEN '0_3'
                                       
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       
                                       THEN '4_7'
                                       
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       
                                       THEN '8_11'
                                       
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       
                                       THEN '12_15' 
                                       
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       
                                       THEN '16_19' 
                                       
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       
                                       THEN '20_23' 
                                       
                                       END AS Violation_Hour_Bin
                                       
                                       FROM violntkt_2015")

dim(Bin_violation_hour_2015)
head(Bin_violation_hour_2015)
createOrReplaceTempView(Bin_violation_hour_2015, "NYC_violt_hour_2015")



hour_bin_tkts_2015 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  Frequency_of_Tickets
                                  
                                  FROM (SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  Frequency_of_Tickets,
                                  
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  
                                  FROM (SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  count(*)as Frequency_of_Tickets
                                  
                                  FROM NYC_violt_hour_2015
                                  
                                  GROUP BY Violation_Hour_Bin,
                                  
                                  Violation_Code))
                                  
                                  WHERE Rnk <= 3")


dim(hour_bin_tkts_2015)
head(hour_bin_tkts_2015)
df_hour_bin_tkts_2015 <- data.frame(head(hour_bin_tkts_2015, nrow(hour_bin_tkts_2015)))



ggplot(df_hour_bin_tkts_2015, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col(fill="green")+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2015 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#2016 Dataset: Violation Time Bin vs. Violation Code Analysis



#Find a way to deal with missing values, if any.

null_violation_time_in_2016<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                          
                                          SUM(CASE WHEN Violation_Time is NULL
                                          
                                          THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                          
                                          100*SUM(CASE WHEN Violation_Time IS NULL
                                          
                                          THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2016_ViolationTimeMissing
                                          
                                          from NYC_PrkTkt_2016_View")

head(null_violation_time_in_2016)

#2016 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.



adjusted_NYC_PrkTkt_2016_v3parking<- subset(NYC_PrkTkt_2016, isNotNull(NYC_PrkTkt_2016$Violation_Time))

adjusted_NYC_PrkTkt_2016_v3parking$Violation_Hour <- hour(cast(adjusted_NYC_PrkTkt_2016_v3parking$Violation_Time,dataType = "string"))

createOrReplaceTempView(adjusted_NYC_PrkTkt_2016_v3parking, "violtkt_2016")





#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 

Bin_violation_hour_2016 <- SparkR::sql("SELECT Violation_Hour,
                                       
                                       Violation_Code,
                                       
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       
                                       THEN '0_3'
                                       
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       
                                       THEN '4_7'
                                       
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       
                                       THEN '8_11'
                                       
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       
                                       THEN '12_15' 
                                       
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       
                                       THEN '16_19' 
                                       
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       
                                       THEN '20_23' 
                                       
                                       END AS Violation_Hour_Bin
                                       
                                       FROM violtkt_2016")



createOrReplaceTempView(Bin_violation_hour_2016, "NYC_violt_hour_2016")



hour_bin_tkts_2016 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  Frequency_of_Tickets
                                  
                                  FROM (SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  Frequency_of_Tickets,
                                  
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  
                                  FROM (SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  count(*)as Frequency_of_Tickets
                                  
                                  FROM NYC_violt_hour_2016
                                  
                                  GROUP BY Violation_Hour_Bin,
                                  
                                  Violation_Code))
                                  
                                  WHERE Rnk <= 3")



df_hour_bin_tkts_2016 <- data.frame(head(hour_bin_tkts_2016, nrow(hour_bin_tkts_2016)))



ggplot(df_hour_bin_tkts_2016, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col(fill="blue")+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2016 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)



#2017 Dataset: Violation Time Bin vs. Violation Code Analysis



#Find a way to deal with missing values, if any.

null_violation_time_in_2017<- SparkR::sql("SELECT count(*)as Total_Num_of_Rows, 
                                          
                                          SUM(CASE WHEN Violation_Time is NULL
                                          
                                          THEN 1 ELSE 0 END)as Nulls_Violation_Time,
                                          
                                          100*SUM(CASE WHEN Violation_Time IS NULL
                                          
                                          THEN 1 ELSE 0 END)/count(*) as Percent_Tickets_2017_ViolationTimeMissing
                                          
                                          from NYC_PrkTkt_2017_View")

head(null_violation_time_in_2017)

#2017 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.



adjusted_NYC_PrkTkt_2017_v3parking<- subset(NYC_PrkTkt_2017, isNotNull(NYC_PrkTkt_2017$Violation_Time))

adjusted_NYC_PrkTkt_2017_v3parking$Violation_Hour <- hour(cast(adjusted_NYC_PrkTkt_2017_v3parking$Violation_Time,dataType = "string"))

createOrReplaceTempView(adjusted_NYC_PrkTkt_2017_v3parking, "violtkt_2017")


#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 

Bin_violation_hour_2017 <- SparkR::sql("SELECT Violation_Hour,
                                       
                                       Violation_Code,
                                       
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3
                                       
                                       THEN '0_3'
                                       
                                       WHEN Violation_Hour BETWEEN 4 AND 7
                                       
                                       THEN '4_7'
                                       
                                       WHEN Violation_Hour BETWEEN 8 AND 11
                                       
                                       THEN '8_11'
                                       
                                       WHEN Violation_Hour BETWEEN 12 AND 15
                                       
                                       THEN '12_15' 
                                       
                                       WHEN Violation_Hour BETWEEN 16 AND 19
                                       
                                       THEN '16_19' 
                                       
                                       WHEN Violation_Hour BETWEEN 20 AND 23
                                       
                                       THEN '20_23' 
                                       
                                       END AS Violation_Hour_Bin
                                       
                                       FROM violtkt_2017")



createOrReplaceTempView(Bin_violation_hour_2017, "NYC_violt_hour_2017")



hour_bin_tkts_2017 <- SparkR::sql("SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  Frequency_of_Tickets
                                  
                                  FROM (SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  Frequency_of_Tickets,
                                  
                                  dense_rank() over (partition by Violation_Hour_Bin order by Frequency_of_Tickets desc) Rnk
                                  
                                  FROM (SELECT Violation_Hour_Bin,
                                  
                                  Violation_Code,
                                  
                                  count(*)as Frequency_of_Tickets
                                  
                                  FROM NYC_violt_hour_2017
                                  
                                  GROUP BY Violation_Hour_Bin,
                                  
                                  Violation_Code))
                                  
                                  WHERE Rnk <= 3")



df_hour_bin_tkts_2017 <- data.frame(head(hour_bin_tkts_2017, nrow(hour_bin_tkts_2017)))



ggplot(df_hour_bin_tkts_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col(fill="orange")+ facet_grid(~Violation_Hour_Bin) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2017 Comparison of Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)




#Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)



#Part 2: Violation Code [Top-3] vs. Violation Time Bin Distribution

#2015 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis

top_3_violations_2015 <- SparkR::sql("SELECT Violation_Code,
                                     
                                     count(*) no_of_tickets
                                     
                                     FROM NYC_violt_hour_2015
                                     
                                     GROUP BY Violation_Code
                                     
                                     ORDER BY no_of_tickets desc")



head(top_3_violations_2015,3)

#Top-3 Violation Code for 2015 are 21, 38 and 14

common_times_2015 <- SparkR::sql("SELECT Violation_Code,
                                 
                                 Violation_Hour_Bin,
                                 
                                 count(*) no_of_tickets
                                 
                                 FROM NYC_violt_hour_2015
                                 
                                 WHERE violation_code IN (21,38,14)
                                 
                                 GROUP BY Violation_Code, 
                                 
                                 Violation_Hour_Bin
                                 
                                 ORDER BY Violation_Code, 
                                 
                                 Violation_Hour_Bin,
                                 
                                 no_of_tickets desc")	



df_common_times_violation_2015 <- data.frame(head(common_times_2015, nrow(common_times_2015)))



ggplot(df_common_times_violation_2015, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col()+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("2015 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)



#2016 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis

top_3_violations_2016 <- SparkR::sql("SELECT Violation_Code,
                                     
                                     count(*) no_of_tickets
                                     
                                     FROM NYC_violt_hour_2016
                                     
                                     GROUP BY Violation_Code
                                     
                                     ORDER BY no_of_tickets desc")



head(top_3_violations_2016,3)

#Top-3 Violation Codes for 2016 are 21, 36 and 38.



common_times_2016 <- SparkR::sql("SELECT Violation_Code,
                                 
                                 Violation_Hour_Bin,
                                 
                                 count(*) no_of_tickets
                                 
                                 FROM NYC_violt_hour_2016
                                 
                                 WHERE violation_code IN (21,36,38)
                                 
                                 GROUP BY Violation_Code, 
                                 
                                 Violation_Hour_Bin
                                 
                                 ORDER BY Violation_Code, 
                                 
                                 Violation_Hour_Bin,
                                 
                                 no_of_tickets desc")	



df_common_times_violation_2016 <- data.frame(head(common_times_2016, nrow(common_times_2016)))



ggplot(df_common_times_violation_2016, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col(fill="orange")+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle("2016 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)



#2017 Dataset: Top-3 Violation Code vs. Violation Time Bin Analysis

top_3_violations_2017 <- SparkR::sql("SELECT Violation_Code,
                                     
                                     count(*) no_of_tickets
                                     
                                     FROM NYC_violt_hour_2017
                                     
                                     GROUP BY Violation_Code
                                     
                                     ORDER BY no_of_tickets desc")



head(top_3_violations_2017,3)

#Top-3 Violation Codes for 2017 are 21, 36 and 38.



common_times_2017 <- SparkR::sql("SELECT Violation_Code,
                                 
                                 Violation_Hour_Bin,
                                 
                                 count(*) no_of_tickets
                                 
                                 FROM NYC_violt_hour_2017
                                 
                                 WHERE violation_code IN (21,36,38)
                                 
                                 GROUP BY Violation_Code, 
                                 
                                 Violation_Hour_Bin
                                 
                                 ORDER BY Violation_Code, 
                                 
                                 Violation_Hour_Bin,
                                 
                                 no_of_tickets desc")	



df_common_times_violation_2017 <- data.frame(head(common_times_2017, nrow(common_times_2017)))



ggplot(df_common_times_violation_2017, aes(x= as.factor(Violation_Hour_Bin), y=no_of_tickets))+ geom_col(fill="blue")+ facet_grid(~Violation_Code) + xlab("Violation Hour Bin") + ylab("Frequency of Tickets") + ggtitle(" 2017 Comparison of Violation_Hour_Bin vs. No_of_tickets") + geom_text(aes(label=no_of_tickets),vjust=-0.3)


#-----------------------------------------------------------------------------------------------------------------------------
#                                                     Question 6
#-----------------------------------------------------------------------------------------------------------------------------

#Let’s try and find some seasonality in this data

#First, divide the year into some number of seasons, and find frequencies of tickets for each season. (Hint: Use Issue Date to segregate into seasons)

#Then, find the three most common violations for each of these seasons.

head(SparkR::sql("select * from NYC_PrkTkt_2015_View limit 1"))
colnames(NYC_PrkTkt_2015)
Season_Binning_2015 <- SparkR::sql("SELECT Summons_Number,
                                   
                                   Violation_Code,
                                   
                                   CASE WHEN Month_of_issuedt IN (1,2,12)
                                   
                                   THEN 'Winter'
                                   
                                   WHEN Month_of_issuedt BETWEEN 3 AND 5
                                   
                                   THEN 'Spring'
                                   
                                   WHEN Month_of_issuedt BETWEEN 6 AND 8
                                   
                                   THEN 'Summer'
                                   
                                   WHEN Month_of_issuedt BETWEEN 9 AND 12
                                   
                                   THEN 'Fall' 
                                   
                                   END AS Season
                                   
                                   FROM NYC_PrkTkt_2015_View")

createOrReplaceTempView(Season_Binning_2015, "season_NYC_PrkTkt_2015_v3")



seasonwise_tickets_2015<- SparkR::sql("SELECT Season,
                                      
                                      Count(*)as Frequency_of_Tickets
                                      
                                      FROM season_NYC_PrkTkt_2015_v3
                                      
                                      GROUP BY Season
                                      
                                      ORDER BY Frequency_of_Tickets desc")

head(seasonwise_tickets_2015)



freq_seasonwise_tickets_2015<- data.frame(head(seasonwise_tickets_2015))

freq_seasonwise_tickets_2015$Fiscal_Year<- c(2015,2015,2015,2015)

freq_seasonwise_tickets_2015



#2016 Season vs. Frequency Analysis

Season_Binning_2016 <- SparkR::sql("SELECT Summons_Number,
                                   
                                   Violation_Code,
                                   
                                   CASE WHEN Month_of_issuedt IN (1,2,12)
                                   
                                   THEN 'Winter'
                                   
                                   WHEN Month_of_issuedt BETWEEN 3 AND 5
                                   
                                   THEN 'Spring'
                                   
                                   WHEN Month_of_issuedt BETWEEN 6 AND 8
                                   
                                   THEN 'Summer'
                                   
                                   WHEN Month_of_issuedt BETWEEN 9 AND 12
                                   
                                   THEN 'Fall' 
                                   
                                   END AS Season
                                   
                                   FROM NYC_PrkTkt_2016_View")

createOrReplaceTempView(Season_Binning_2016, "season_NYC_PrkTkt_2016_v3")



seasonwise_tickets_2016<- SparkR::sql("SELECT Season,
                                      
                                      Count(*)as Frequency_of_Tickets
                                      
                                      FROM season_NYC_PrkTkt_2016_v3
                                      
                                      GROUP BY Season
                                      
                                      ORDER BY Frequency_of_Tickets desc")

head(seasonwise_tickets_2016)



freq_seasonwise_tickets_2016<- data.frame(head(seasonwise_tickets_2016))

freq_seasonwise_tickets_2016$Fiscal_Year<- c(2016,2016,2016,2016)

freq_seasonwise_tickets_2016



#2017 Season vs. Frequency Analysis

Season_Binning_2017 <- SparkR::sql("SELECT Summons_Number,
                                   
                                   Violation_Code,
                                   
                                   CASE WHEN Month_of_issuedt IN (1,2,12)
                                   
                                   THEN 'Winter'
                                   
                                   WHEN Month_of_issuedt BETWEEN 3 AND 5
                                   
                                   THEN 'Spring'
                                   
                                   WHEN Month_of_issuedt BETWEEN 6 AND 8
                                   
                                   THEN 'Summer'
                                   
                                   WHEN Month_of_issuedt BETWEEN 9 AND 12
                                   
                                   THEN 'Fall' 
                                   
                                   END AS Season
                                   
                                   FROM NYC_PrkTkt_2017_View")

createOrReplaceTempView(Season_Binning_2017, "season_NYC_PrkTkt_2017_v3")



seasonwise_tickets_2017<- SparkR::sql("SELECT Season,
                                      
                                      Count(*)as Frequency_of_Tickets
                                      
                                      FROM season_NYC_PrkTkt_2017_v3
                                      
                                      GROUP BY Season
                                      
                                      ORDER BY Frequency_of_Tickets desc")

head(seasonwise_tickets_2017)



freq_seasonwise_tickets_2017<- data.frame(head(seasonwise_tickets_2017))

freq_seasonwise_tickets_2017$Fiscal_Year<- c(2017,2017,2017,2017)

freq_seasonwise_tickets_2017



#Comparison of Season vs. Frequency of Tickets ocer the Years

freq_tktseason_combined<- rbind(freq_seasonwise_tickets_2015, freq_seasonwise_tickets_2016, freq_seasonwise_tickets_2017)



ggplot(freq_tktseason_combined, aes(x= as.factor(Season), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Seasons of Year") + ylab("Frequency of Tickets") + ggtitle("Comparison of Seasons vs. Frequency of Tickets between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)





#Season vs. Violation Code Distribution Analysis



#2015 Season vs. Violation Code Distribution Analysis



season_violation_2015 <- SparkR::sql("SELECT  Season,
                                     
                                     Violation_Code,
                                     
                                     Frequency_of_Tickets
                                     
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     
                                     Season,
                                     
                                     Violation_Code,
                                     
                                     Frequency_of_Tickets
                                     
                                     FROM (SELECT Season,
                                     
                                     Violation_Code,
                                     
                                     Count(*) Frequency_of_Tickets
                                     
                                     FROM season_NYC_PrkTkt_2015_v3
                                     
                                     GROUP BY Season, Violation_Code))
                                     
                                     WHERE rk <= 3
                                     
                                     ORDER BY Season, Frequency_of_Tickets desc")



df_2015_season_violation <-  data.frame(head(season_violation_2015, nrow(season_violation_2015)))

df_2015_season_violation



#Seasonwise Violation Code Distribution 2015

ggplot(df_2015_season_violation, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col(fill="red")+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2015 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#2016 Season vs. Violation Code Distribution Analysis

season_violation_2016 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_NYC_PrkTkt_2016_v3
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2016 <-  data.frame(head(season_violation_2016, nrow(season_violation_2016)))df_season_violation_2016

#Seasonwise Violation Code Distribution 2016
ggplot(df_season_violation_2016, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2016 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#2017 Season vs. Violation Code Distribution Analysis

season_violation_2017 <- SparkR::sql("SELECT  Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT dense_rank() over (partition by Season order by Frequency_of_Tickets desc) rk,
                                     Season,
                                     Violation_Code,
                                     Frequency_of_Tickets
                                     FROM (SELECT Season,
                                     Violation_Code,
                                     Count(*) Frequency_of_Tickets
                                     FROM season_NYC_PrkTkt_2017_v3
                                     GROUP BY Season, Violation_Code))
                                     WHERE rk <= 3
                                     ORDER BY Season, Frequency_of_Tickets desc")

df_season_violation_2017 <-  data.frame(head(season_violation_2017, nrow(season_violation_2017)))
df_season_violation_2017

#Seasonwise Violation Code Distribution 2017
ggplot(df_season_violation_2017, aes(x= as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Season) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2017 Comparison of Seasons vs. Frequency of Violation Codes") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

#-----------------------------------------------------------------------------------------------------------------------------
#                                                     Question 7 
#-----------------------------------------------------------------------------------------------------------------------------
#The fines collected from all the parking violation constitute a revenue source for the NYC police department. 
#Let’s take an example of estimating that for the three most commonly occurring codes.

#2015
violationcd_frequency_2015<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from NYC_PrkTkt_2015_View 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2015,3)
#The top three violations are 21,36 and 14 that have highest number of tickets
fine_top3_2015<- data.frame(head(violationcd_frequency_2015,3))
fine_top3_2015$Fiscal_Year <- c(2015,2015,2015)
#The average fine for violation 21 is 50
#The average fine for violation 36 is 50
#The average fine for violation 14 is 115
#lets add the average fine to the dataframe
fine_top3_2015$Average_Fine_PerTicket<- c(55,50,115)
#Calculate the fine amount by multiplying teh fine amount with the number of tickets
fine_top3_2015$Total_Fine_Amount<- fine_top3_2015$Frequency_of_Tickets * fine_top3_2015$Average_Fine_PerTicket
fine_top3_2015

#2016
violationcd_frequency_2016<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from NYC_PrkTkt_2016_View 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2016,3)
#The top three violations are 21,36 and 38 that have highest number of tickets
fine_top3_2016<- data.frame(head(violationcd_frequency_2016,3))
fine_top3_2016$Fiscal_Year <- c(2016,2016,2016)
#The average fine for violation 21 is 50
#The average fine for violation 36 is 50
#The average fine for violation 38 is 50
#lets add the average fine to the dataframe
fine_top3_2016$Average_Fine_PerTicket<- c(55,50,50)
#Calculate the fine amount by multiplying teh fine amount with the number of tickets
fine_top3_2016$Total_Fine_Amount<- fine_top3_2016$Frequency_of_Tickets * fine_top3_2016$Average_Fine_PerTicket
fine_top3_2016

#2017
violationcd_frequency_2017<- SparkR::sql("SELECT Violation_Code, count(*)as Frequency_of_Tickets
                                         from NYC_PrkTkt_2017_View 
                                         group by Violation_Code
                                         order by Frequency_of_Tickets desc")
head(violationcd_frequency_2017,3)
#The top three violations are 21,36 and 38 that have highest number of tickets
fine_top3_2017<- data.frame(head(violationcd_frequency_2017,3))
fine_top3_2017$Fiscal_Year <- c(2017,2017,2017)
#The average fine for violation 21 is 50
#The average fine for violation 36 is 50
#The average fine for violation 38 is 50
#lets add the average fine to the dataframe
fine_top3_2017$Average_Fine_PerTicket<- c(55,50,50)
#Calculate the fine amount by multiplying the fine amount with the number of tickets
fine_top3_2017$Total_Fine_Amount<- fine_top3_2017$Frequency_of_Tickets * fine_top3_2017$Average_Fine_PerTicket
fine_top3_2017

fine_top3_combined<- rbind(fine_top3_2015, fine_top3_2016, fine_top3_2017)

ggplot(fine_top3_combined, aes(x=as.factor(Violation_Code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("Comparison of Top 3 Violation Code vs Frequency of Ticket between Fiscal Years") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)

ggplot(fine_top3_combined, aes(x=as.factor(Violation_Code), y=Total_Fine_Amount))+ geom_col()+ facet_grid(~Fiscal_Year) + xlab("Violation Code") + ylab("Total Fine Amount") + ggtitle("Comparison of Top 3 Violation Code vs Total Fine Amount between Fiscal Years") + geom_text(aes(label=Total_Fine_Amount),vjust=-0.3)

#Violation code 14 - i.e General No Standing: Standing or parking where standing is not allowed by sign, street marking or; traffic control device was
#the highest fine collection in the fiscal year 2014 - 2015,However that dimished in the later years,it appears improved disciple in people
#Violation code 36 - i.e Exceeding the posted speed limit in or near a designated school zone.It seems to be introduced and in the ficsal year 2015-2016 howver fiscal year 2014-2015 did not show top collections
#Violation code 21 - i.e Street Cleaning: No parking where parking is not allowed by sign, street marking or traffic control device.
#There is no improvement in the Violation 21,38 in all the ficsal years 2014 - 2015,2015 - 2016 and 2016 - 2017



#***********************************************************************************END*******************************************************************************************************************************************************





