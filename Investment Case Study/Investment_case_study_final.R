# Setting up directory
setwd('~/desktop/investment case study')
#Loading additional libraries
library(tidyr)
library(dplyr)
library(stringr)
#import data
companies <- read.delim("companies.txt",stringsAsFactors = F)
nrow(companies) #66368 rows
rounds2 <- read.csv("rounds2.csv",stringsAsFactors = F)
nrow(rounds2) #114949 rows
mapping <- read.csv("mapping.csv",stringsAsFactors = F)
nrow(mapping) #688 rows
companies_copy <- read.delim("companies.txt",stringsAsFactors = F)
colnames(companies_copy)

#**************** DATA CLEANING****************************************************

#Before starting the analysis,the permalink column in companies and company_permalink column in rounds2 file needs to be converted into lower cases.
#converting the permalink column in companies and company_permalink column in rounds2 file into lower cases
companies$permalink <- tolower(companies$permalink)
View(companies)
rounds2$company_permalink <- tolower(rounds2$company_permalink)
View(rounds2)


#*****This section is to remove the special characters rendered as '\ ,<,>' in Mac machines as the imported file has special characters treated differently.This block 
#should not be run if the code is evaluated using a windows machine. Executing this step was needed because the permalink values of companies file wasn't macthing rounds2 file as 
#the special characters rendered with '\ < >' were with different numbers in both the files *****
companies<-mutate(companies,Perma_link=(gsub('[^a-zA-Z0-9/-]',"",companies$permalink)))
companies<-mutate(companies,Name=(gsub('[^a-zA-Z0-9/-]',"",companies$name)))
companies<-companies[,-1]
ncol(companies) #10
View(tail(companies,10))
rounds2<-mutate(rounds2,Perma_link=(gsub('[^a-zA-Z0-9/-]',"",rounds2$company_permalink)))
rounds2<-rounds2[-1]
ncol(rounds2) #6
View(tail(rounds2))
#**********************************************************************************




# *************************CheckPoints 1 ****************************************************

#                     Table 1.1: Understand the Data Set 

#Q1 How many unique companies are present in rounds2?
unique_companies_inrounds2 <- length(unique(companies$Perma_link))
unique_companies_inrounds2 #66368 unique companies in companies file which matches the number of rows in companies
#Q2 How many unique companies are present in companies?
unique_companies_incompanies <- length(unique(rounds2$Perma_link))
unique_companies_incompanies #66368 unique companies in rounds2 which macthes with unique companies in companies file
#Q3 *****In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
#ans : since column name 'permalink' in compnaies are all unique values,Permalink can be used a unique key.Permalink column is now renamed as Perma_link in both companies and
#rounds2 files******
#Q4 Are there any companies in the rounds2 file which are not present in companies? 
companies_in_rounds2_notin_companies <- length(rounds2[!rounds2$Perma_link %in% companies$Perma_link])
companies_in_rounds2_notin_companies #0
# Answer : N
#Q5 Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
# Name the merged frame master_frame. How many observations are present in master_frame?
#Answer : 114949 observations
master_frame <- merge(companies,rounds2,by='Perma_link')
nrow(master_frame) #114949 rows
ncol(master_frame) #15 columns
# Analysing NAs and Blank values in the dataframe
length(which(is.na(master_frame$raised_amount_usd)==T)) #19990 NAs in raised_amount_usd
length(which(master_frame$raised_amount_usd=="")==T) #0blank values in raised_amount_usd
length(which(is.na(master_frame$country_code)==T)) #0 NAs in country_code
length(which(master_frame$country_code=="")==T) # 8678 blank values in country_code
length(which(is.na(master_frame$funding_round_type)==T)) #0 NAs in funding_round_type
length(which(master_frame$funding_round_type=="")==T) #0 blank values
# Keeping NAs in the dataset will not be valuable,so it makes sense to remove them
master_frame <- na.omit(master_frame)
nrow(master_frame)

#*************************Checkpoints - 2 *************************************

#           Table 2.1: Average Values of Investments for Each of these Funding Types 

# Q1 Average funding amount of venture type
# Ans : 11748949
venture_type <- filter(master_frame,funding_round_type=='venture')
nrow(venture_type) #50228 observations
avg_venture_type <- mean(venture_type$raised_amount_usd,na.rm = T)
avg_venture_type # 11748949
# Q2 Average funding amount of angel type
# Ans : 958694.5
angel_type <- filter(master_frame,funding_round_type=='angel')
nrow(angel_type) #4860 observations
avg_angel_type <- mean(angel_type$raised_amount_usd,na.rm = T)
avg_angel_type # 958694.5
# Q3 Average funding amount of seed type
#Ans : 719818
seed_type <- filter(master_frame,funding_round_type=='seed')
nrow(seed_type) #23603 observations
avg_seed_type <- mean(seed_type$raised_amount_usd,na.rm = T)
avg_seed_type # 719818
#Q4 Average funding amount of private equity type
#Ans : 73308593 
private_equity_type <- filter(master_frame,funding_round_type=='private_equity')
nrow(private_equity_type) #1936 observations
avg_private_equity <- mean(private_equity_type$raised_amount_usd,na.rm = T)
avg_private_equity # 73308593
#Q5 Considering that Spark Funds wants to invest between 5 to 15 million USD per investment 
#round, which investment type is the most suitable for it?
#Ans : Venture which is between 5-15 million USD
most_suitable_investment_spark <- group_by(master_frame,funding_round_type) %>% summarise(avg_fund = mean(raised_amount_usd,na.rm = T))%>%
  filter(avg_fund>=5000000&avg_fund<=15000000)
most_suitable_investment_spark

#*************************Checkpoints - 3 *************************************

#            Table 3.1: Analysing the Top 3 English-Speaking Countries
#Creating Top 9 countries dataframe for investment type 'venture',df=venture_type
top9<- group_by(venture_type,country_code) %>% summarise(highest_amount_funding = sum(raised_amount_usd,na.rm = T)) %>%
  arrange(desc(highest_amount_funding)) %>% head(.,n=10)
#removing blank country codes
top9 <- top9[!top9$country_code=="",]
View(top9)
#Upon inspecting the dataframe top9,it is undersood that USA(united states),GBR(United Kingdon) and IND(India) are the top 3 countries
#Q1  Top English-speaking country
#Ans: USA
#Q2  Second English-speaking country
#Ans: GBR
#Q4  Third English-speaking country
#Ans: IND

#*************************Checkpoints - 4 *************************************

#Q1 Extract the primary sector of each category list from the category_list column
master_frame <- separate(master_frame,category_list,into = c('Primary Sector','Unwanted'),sep = '\\|')
View(master_frame)
master_frame <- master_frame[,-4]
nrow(master_frame)
master_frame$`Primary Sector`<-tolower(master_frame$`Primary Sector`)
#Q2 Use the mapping file 'mapping.csv' to map each primary sector to one of the 
# eight main sectors (Note that ???Others??? is also considered one of the main sectors)
#converting the wide format of the mapping.csv file into long format
mapping<-gather(mapping,main_sector,values,2:ncol(mapping))
mapping<-filter(mapping,values==1)
mapping<-mapping[,-3]
colnames(mapping)[1]<-c('primary_sector')
View(mapping)
length(mapping[grep(0,mapping$primary_sector),1]) #53 rows hve 0 in place of 'na'
mapping$primary_sector<-tolower(mapping$primary_sector)
mapping$main_sector<-tolower(mapping$main_sector)
mapping<-mutate(mapping,ref_sector=(gsub("[0]","na",mapping$primary_sector)))
mapping<-mapping[,-1]
nrow(mapping)
colnames(mapping)[2]<-c('Primary Sector')
View(mapping)

#merging master_frame with mapping using primary sector

merged_master_mapping <- merge(master_frame,mapping,by='Primary Sector')
View(merged_master_mapping)
nrow(merged_master_mapping)
merged_master_mapping <- filter(merged_master_mapping,main_sector!='blanks')
View(merged_master_mapping)
nrow(merged_master_mapping) # 93828 observations

#*************************Checkpoints - 5 *************************************
#forming D1,D2,D3 ----> D1 = USA,D2 = GBR,D3 = IND
D1 <- filter(merged_master_mapping,raised_amount_usd>=5000000 & raised_amount_usd<=15000000,country_code=='USA',funding_round_type=='venture')
nrow(D1) #12063
View(head(D1))
View(tail(D1))

D2 <- filter(merged_master_mapping,raised_amount_usd>=5000000 & raised_amount_usd<=15000000,country_code=='GBR',funding_round_type=='venture')
nrow(D2) #621
View(head(D2))
View(tail(D2))

D3 <- filter(merged_master_mapping,raised_amount_usd>=5000000 & raised_amount_usd<=15000000,country_code=='IND',funding_round_type=='venture')
nrow(D3) #328

#converting all company names to lower just in case to avoid  names displayed in different case sensitivity
D1$Name<-tolower(D1$Name)
D2$Name<-tolower(D2$Name)
D3$Name<-tolower(D3$Name)

#             Table 5.1 : Sector-wise Investment Analysis

#Q1  Total number of investments (count)
total_investment_D1 <- length(D1$raised_amount_usd)
total_investment_D1 # Ans : 12063 number of investments in USA

total_investment_D2 <- length(D2$raised_amount_usd)
total_investment_D2 # Ans : 621 number of investments in GBR

total_investment_D3 <- length(D3$raised_amount_usd)
total_investment_D3 # Ans : 328 number of investments in IND
#Q2 Total amount of investment (USD)
total_amount_D1_USA <- sum(D1$raised_amount_usd,na.rm = T)
total_amount_D1_USA #There is USD 107757097294 of total invetments in USA

total_amount_D1_GBR <- sum(D2$raised_amount_usd,na.rm = T)
total_amount_D1_GBR #There is USD 5379078691 of total invetments in GBR

total_amount_D1_IND <- sum(D3$raised_amount_usd,na.rm = T)
total_amount_D1_IND #There is USD 2949543602 of total invetments in IND

#Q3 Top sector (based on count of investments)
Top_sector_D1_USA <- group_by(D1,main_sector) %>% summarise(no_investments=length(raised_amount_usd)) %>%
  arrange(desc(no_investments)) %>% head(.,5)
View(Top_sector_D1_USA)
Top_sector_D1_USA[1,] # Top sector = Others

Top_sector_D2_GBR <- group_by(D2,main_sector) %>% summarise(no_investments=length(raised_amount_usd)) %>%
  arrange(desc(no_investments)) %>% head(.,5)
View(Top_sector_D2_GBR)
Top_sector_D2_GBR[1,] # Top sector = Others

Top_sector_D3_IND <- group_by(D3,main_sector) %>% summarise(no_investments=length(raised_amount_usd)) %>%
  arrange(desc(no_investments)) %>% head(.,5)
View(Top_sector_D3_IND)
Top_sector_D3_IND[1,] # Top sector = Others


#Q4 Second-best sector (based on count of investments)
sec_best_sector_USA<-Top_sector_D1_USA[2,]
sec_best_sector_USA # social..finance..analytics..advertising for USA

sec_best_sector_GBR<-Top_sector_D2_GBR[2,]
sec_best_sector_GBR # social..finance..analytics..advertising  for GBR

sec_best_sector_IND<-Top_sector_D3_IND[2,]
sec_best_sector_IND # social..finance..analytics..advertising   for India

#Q5 Third-best sector (based on count of investments)

third_best_sector_USA<-Top_sector_D1_USA[3,]
third_best_sector_USA # cleantech...semiconductors  for USA

third_best_sector_GBR<-Top_sector_D2_GBR[3,]
third_best_sector_GBR # cleantech...semiconductors  for GBR

third_best_sector_IND<-Top_sector_D3_IND[3,]
third_best_sector_IND # news..search.and.messaging  for India

#Q6 Number of investments in the top sector (refer to point 3)
Top_sector_D1_USA[1,2] # 2950 investments in USA for top sector(others)
Top_sector_D2_GBR[1,2] # 147 investments in GBR for top sector(others)
Top_sector_D3_IND[1,2] # 110 investments in india for top sector(others)

#Q7 Number of investments in the second-best sector (refer to point 4)
Top_sector_D1_USA[2,2] # 2714 investments in USA for top sector(social..finance..analytics..advertising for USA)
Top_sector_D2_GBR[2,2] # 133 investments in GBR for top sector(social..finance..analytics..advertising for USA)
Top_sector_D3_IND[2,2] # 60 investments in india for top sector(social..finance..analytics..advertising for india)

#Q8 Number of investments in the third-best sector (refer to point 5)
Top_sector_D1_USA[3,2] # 2350 investments in USA for top sector(cleantech...semiconductors for USA)
Top_sector_D2_GBR[3,2] # 130 investments in GBR for top sector(cleantech...semiconductors for GBR)
Top_sector_D3_IND[3,2] # 52 investments in india for top sector(news..search.and.messaging for india)

#Q9 For the top sector count-wise (point 3), which company received the highest investment?

top_sector_company_highinvestment_usa <- filter(D1,main_sector=='others') %>% group_by(Name) %>% summarise(no_of_invest = sum(raised_amount_usd,na.rm = T))%>%
  arrange(desc(no_of_invest))
top_sector_company_highinvestment_usa[1,] #Ans : virtustream in USA


top_sector_company_highinvestment_gbr <- filter(D2,main_sector=='others') %>% group_by(Name) %>% summarise(no_of_invest = sum(raised_amount_usd,na.rm = T))%>%
  arrange(desc(no_of_invest))
View(top_sector_company_highinvestment_gbr)
top_sector_company_highinvestment_gbr[1,] # Ans : electric cloud in gbr

top_sector_company_highinvestment_ind <- filter(D3,main_sector=='others') %>% group_by(Name) %>% summarise(no_of_invest = sum(raised_amount_usd,na.rm = T))%>%
  arrange(desc(no_of_invest))
View(top_sector_company_highinvestment_ind)
top_sector_company_highinvestment_ind[1,] # Ans  : firstcry.com in india

#Q10 For the second-best sector count-wise (point 4), which company received the highest investment?

sec_sector_company_highinvestment_usa <- filter(D1,main_sector=='social..finance..analytics..advertising') %>% group_by(Name) %>% summarise(no_of_invest = sum(raised_amount_usd,na.rm = T))%>%
  arrange(desc(no_of_invest))
View(sec_sector_company_highinvestment_usa)
sec_sector_company_highinvestment_usa[1,] #Ans : sstincformerlyshotspotter in USA


sec_sector_company_highinvestment_gbr <- filter(D2,main_sector=='social..finance..analytics..advertising') %>% group_by(Name) %>% summarise(no_of_invest = sum(raised_amount_usd,na.rm = T))%>%
  arrange(desc(no_of_invest))
View(sec_sector_company_highinvestment_gbr)
sec_sector_company_highinvestment_gbr[1,] # Ans : cellticktechnologies  in gbr

sec_sector_company_highinvestment_ind <- filter(D3,main_sector=='social..finance..analytics..advertising') %>% group_by(Name) %>% summarise(no_of_invest = sum(raised_amount_usd,na.rm = T))%>%
  arrange(desc(no_of_invest))
View(sec_sector_company_highinvestment_ind)
sec_sector_company_highinvestment_ind[1,] # Ans  : manthansystems in india

# ************************Saving work for tableau *************************************


write.csv(merged_master_mapping,"spark_funds_tableau.csv")

# *********************** END **********************************************************
