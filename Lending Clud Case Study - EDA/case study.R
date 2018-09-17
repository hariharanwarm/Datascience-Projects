rm(list = ls())
options(scipen=999)
#Libraries required for the work
library(tidyverse)
library(lubridate)
library(reshape2)
library(naniar)
#Data
data <- read.csv("loan.csv",stringsAsFactors = F)
#sense check no of observations and number of variables in the  data_purified
ncol(data) #111
nrow(data) #39717
summary(data)

#the dataframe appears to have a lot of columns withoout any values which are denoted as NA an dsome columns with only zeros and NAs
sum(colSums(is.na(data)) == nrow(data)) # 54 columns with only NAs,these columns can be removed
sum(colSums(is.na(data)) != nrow(data))

#determining the names of columns that entirely have NAs and 0's
names(data)[sapply(data, setequal, c(NA))] #displays column names that entirely have NAs
names(data)[sapply(data, setequal, c(0,NA))] #displays column names that entirely have 0s

#remove columns that have entirely NAs or zeros as they would not be of any significance for analysis
data_purified <- data[,(colSums(is.na(data)) != nrow(data))] # selects columns that entirely have NAs
data_purified <- data[,colSums(data != 0,na.rm = T)>0] # selects columns that entirely have 0's
#sense check no of observations and number of variables in the  data_purified
ncol(data_purified) #52
nrow(data_purified) #39717
summary(data_purified)


#other columns with NAs,to decide later what needs to be done with these as we proceed
sum(is.na(data_purified$title)) #1 NA
sum(is.na(data_purified$mths_since_last_delinq)) #25682 NAs
sum(is.na(data_purified$mths_since_last_record)) #36931 NAs
sum(is.na(data_purified$pub_rec_bankruptcies)) #697 NAs
colnames(data_purified)[colSums(is.na(data_purified))>0]

#Visualise for NAs in the existing columns 
vis_miss(data_purified,warn_large_data = FALSE)+ ggplot2::scale_fill_brewer(palette = "Dark2")


#fixing date for column issue_d i.e converting it to data type from character type
data_purified$issue_d <- paste0('01-',data_purified$issue_d)
data_purified$issue_d <- dmy(data_purified$issue_d)
summary(data_purified$issue_d) # data range "2007-06-01" to "2011-12-01" 
class(data_purified$issue_d) # date

#fixing date for column last_pymnt_d i.e converting it to data type from character type
sum(data_purified$last_pymnt_d == '') #71 zeroes
sum(is.na(data_purified$last_pymnt_d)) #Nil NAs
data_purified$last_pymnt_d <- paste0('01-',data_purified$last_pymnt_d)
data_purified$last_pymnt_d  <- dmy(data_purified$last_pymnt_d ) #71 are parsed because they are blank
sum(is.na(data_purified$last_pymnt_d )) #blanks are replaced with NAs
class(data_purified$last_pymnt_d )

#fixing earliest_cr_line
 sum(data_purified$earliest_cr_line=='') #Nil Blanks
 sum(is.na(data_purified$earliest_cr_line)) #Nil NAs
 data_purified$earliest_cr_line <- paste0('01-',data_purified$earliest_cr_line)
 data_purified$earliest_cr_line <- dmy(data_purified$earliest_cr_line)
 head(data_purified$earliest_cr_line)
 class( data_purified$earliest_cr_line)
 
#fixing last_credit_pull_d
 sum(data_purified$last_credit_pull_d=='')#2 blanks
 sum(is.na(data_purified$last_credit_pull_d)) #Nil NAs
 head(data_purified$last_credit_pull_d)
 data_purified$last_credit_pull_d <- paste0('01-',data_purified$last_credit_pull_d)
 data_purified$last_credit_pull_d <- dmy(data_purified$last_credit_pull_d)
 class(data_purified$last_credit_pull_d)
 
#Convert int_rate into a numeric field
 
 head(data_purified$int_rate)
 data_purified$int_rate <- gsub('%','',data_purified$int_rate)
 data_purified$int_rate <- as.numeric(data_purified$int_rate)
 summary(data_purified$int_rate)

#Convert revol_util into a numeric field 
 head(data_purified$revol_util)
 data_purified$revol_util <- gsub('%','',data_purified$revol_util)
 data_purified$revol_util <- as.numeric(data_purified$revol_util)
 summary(data_purified$revol_util)
 
#Adding levels to employmenet length
table( data_purified$emp_length)
 data_purified$emp_length<-factor(data_purified$emp_length,levels = c('< 1 year','1 year','2 years','3 years','4 years',
                                                                      '5 years','6 years','7 years','8 years','9 years','10+ years','n/a'))
 
#Create a new column for grade and assign a number,this can be used to correlate with interest rate
unique(data_purified$grade)
data_purified$grade_num <- ifelse(data_purified$grade %in% c('A'),1,ifelse(data_purified$grade %in% c('B'),2,
                                                                               ifelse(data_purified$grade %in% c('C'),3,
                                                                                      ifelse(data_purified$grade %in% c('D'),4,
                                                                                             ifelse(data_purified$grade %in% c('E'),5,
                                                                                                    ifelse(data_purified$grade %in% c('F'),6,
                                                                                                           ifelse(data_purified$grade %in% c('G'),7,NA)))))))
 
   



########################### Analysing Employment Length ##############################

#Employment length distribution
ggplot(data_purified,aes(x=emp_length,fill=emp_length))+geom_bar()+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),stat = 'count',vjust=-0.5,size=2,position = position_dodge(width = 1))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))+labs(title = 'Employment length distribution')

# Employment length Vs Loan Status - overall percentage
ggplot(data_purified,aes(x=emp_length,fill=loan_status))+geom_bar(position = 'dodge')+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),stat = 'count',vjust=-0.5,size=2,position = position_dodge(width = 1))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))+labs(title = 'Employment length Vs Loan Status')

# Employment length Vs Loan Status - percentage per employment length
group_by(data_purified,emp_length,loan_status)%>%summarise(count=n())%>%mutate(perc=paste0(round((count/sum(count))*100),'%'))%>%
  ggplot(aes(x=emp_length,y=count,fill=loan_status))+geom_bar(stat='identity',position = 'dodge')+
  geom_text(aes(label=perc),position = position_dodge(width = 1),vjust=-0.5,size=2,hjust=0.2)+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))

# Employment length Vs Loan amount

group_by(data_purified,emp_length,loan_status)%>%summarise(avg = round(mean(loan_amnt)))%>%
  ggplot(aes(x=emp_length,y=avg,fill=loan_status))+geom_bar(stat='identity',position = 'dodge')+
  geom_text(aes(label=avg),position = position_dodge(width = 1),vjust=-0.5,size=2,hjust=0.2)+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))
#mutate(perc=paste0(round((avg/sum(avg))*100),'%'))%>%

# Employment length Vs Annual Income

group_by(data_purified,emp_length,loan_status)%>%summarise(avg = round(mean(annual_inc)))%>%
  ggplot(aes(x=emp_length,y=avg,fill=loan_status))+geom_bar(stat='identity',position = 'dodge')+
  geom_text(aes(label=avg),position = position_dodge(width = 1),vjust=-0.5,size=2,hjust=0.2)+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))

  

###################### Analysing Grade ###################################  

#Overall distribution of Grade in the dataset,Grade 'B' shows the highest distribution followed by Grade 'A' then C and so on

ggplot(data_purified,aes(x=grade,fill=grade))+geom_bar()+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),
            stat='count',size=2,vjust=-0.5,hjust=0.5)+labs(title='Overall distribution of Grade')

#Grade Vs Loan Status,More charged off customers are in the grade 'B' then in 'C','D','E','A' and os on
ggplot(data_purified,aes(x=grade,fill=loan_status))+geom_bar(position = 'dodge')+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),
            stat='count',position = position_dodge(width = 1),size=2,vjust=-0.5,hjust=0.5)+labs(title='Grade Vs Loan Status')

#Grade vs employement length
ggplot(data_purified,aes(x=emp_length,fill=grade))+geom_bar(position = 'dodge')+facet_wrap(~loan_status)+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))  



################################ Analysing "home_ownership" ###################################
#Overall dustribution of 'Home Owership',Plots shows that about 47.6% of the customers stay on a rented home,
#44.5% of customers stay on a mortage home,only 7.7% of the customers stay in thoer own home.0.2% of the home ownership is not known

ggplot(data_purified,aes(x=home_ownership,fill=home_ownership))+geom_bar(position = 'dodge')+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),
            stat='count',size=2,vjust=-0.5,hjust=0.5)+labs(title='Overall distribution of Home Ownership')


#Home Ownership Vs loan status ----- Majority of the 'Charged off' customers stay in rented home i.e about 7.1%
#second highest charged off customers stay in mortgaged homes i.e about 5.9%
ggplot(data_purified,aes(x=home_ownership,fill=loan_status))+geom_bar(position = 'dodge')+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),
            stat='count',size=2,vjust=-0.5,hjust=0.5,position = position_dodge(width = 1))+
  labs(title='Home Ownership Vs loan status')

#Home Ownership Vs Loan status per Employment length,remove outliers that have 'None' and 'Other' as Home ownership values
#customers whose employmenet length is greter than 10 years and live in mortgage homes are majorily charged off compared to customers living in Rented home
data_purified %>% filter(!home_ownership%in%c('NONE','OTHER'))%>%
ggplot(aes(x=emp_length,fill=loan_status))+geom_bar(position = 'dodge')+
  facet_wrap(~home_ownership)+theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))+
  labs(title='Home Ownership Vs Loan status per Employment length')



################ Analysing Term ###################

#Overall dustribution of Term,About 73.3% of the customers have opted for 36 months and 26.7 % have opted for 60 months

ggplot(data_purified,aes(x=term,fill=term))+geom_bar(position = 'dodge')+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),
            stat='count',size=2,vjust=-0.5,hjust=0.5)+labs(title='Overall dustribution of Term')


#Term Vs loan status,About 23% of the customers who opted for 60 months term are charged off which is higher than
# the customers who opted for 36 months which is about 11%

term<-group_by(data_purified,term,loan_status)%>%summarise(count=n())%>%mutate(perc=paste0(round((count/sum(count)*100)),'%'))
ggplot(term,aes(x=term,y=count,fill=loan_status))+geom_bar(stat = 'identity')+
  geom_text(aes(label=perc),size=4,position = position_stack(vjust=0.5))+
  labs(title='Term Vs loan status')

#Term Vs loan status per Employent length,customers whose employmenet length is greater than 10 years are highly charged off when thier term for 
# payment is 60 months , the second highest is when employement length is greater than 10 years  term is 36  and the third being employement length less than 1 year and term 36 months
ggplot(data_purified,aes(x=emp_length,fill=loan_status))+geom_bar(position = 'dodge')+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),
            stat='count',size=2,vjust=-0.5,hjust=0.5,position = position_dodge(width = 1))+facet_wrap(~term)+
  labs(title='Term Vs loan status per Employent length')


 
##################Analysing "purpose" ####################  

#Overall distribution,more people have taken loan for the purpose of Debt consolidation i.e about 46.9%,followed by credit card and other
ggplot(data_purified,aes(x=purpose,fill=purpose))+geom_bar() +
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),stat = 'count',vjust=-0.5)+
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))

#Purpose Vs Loan status per Employmenet length(comprehensive details in presentation document)

ggplot(data_purified,aes(x=purpose,fill=loan_status))+geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))+facet_wrap(~emp_length)+
  labs(title='Purpose Vs Loan status per Employment length')



################ Analysis Verification status #######################

#Overall distribuion  of "verification_status",about 42.6% of the people are not verified
ggplot(data_purified,aes(x=verification_status,fill=verification_status))+geom_bar()+
geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),stat = 'count',vjust=-0.5)+
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))+
  labs(title='Overall distribuion  of "verification_status"')


#Verification status Vs loan status,customers wh are 'not verified' and just'verified' have higher proportion for charged off

ggplot(data_purified,aes(x=verification_status,fill=loan_status))+geom_bar()+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),stat = 'count',size=2,position = position_stack(vjust=0.5))+
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))

#Verification status Vs loan status per employment length,plot shows customers whose employment length is less than 1 year
# who are not verified have higher charged off rate,howver employees whose employmenet length is graeter than 10 years and are verified 
#but not source verified have higher charge off rate

ggplot(data_purified,aes(x=emp_length,fill=loan_status))+geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))+facet_wrap(~verification_status)



#############Analysing Location ########################
#Overall distribution , California has the highest number of customers
ggplot(data_purified,aes(x=addr_state,fill=addr_state))+geom_bar()+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),stat = 'count',vjust=-0.5,size=2)+
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))+
  labs(title='Overall distribuion  of Location')

#Location VS Charged off,Califirmnia followed by Florida and then Ney york have higher charged off rate

ggplot(data_purified,aes(x=addr_state,fill=loan_status))+geom_bar()+
  geom_text(aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),stat = 'count',size=2,position = position_stack(vjust=0.5))+
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust = 0.5))+
  labs(title='Location vs Loan status')

########################Plot a correlation matrix aginst all numeric columns##########################

#create a correlation matrix with all numeric variables
for_cor <- select_if(data_purified,is.numeric)
for_cor <- for_cor[,-which(names(for_cor) %in% c('policy_code'))]


# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

cor_data <- round(cor(for_cor,use = 'pairwise.complete.obs'),2)
cor_data <- get_upper_tri(cor_data)
melt_cor_data <- melt(cor_data)
ggplot(melt_cor_data,aes(x=Var1,y=Var2,fill=value))+geom_tile(color='white')+theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")+coord_fixed()



###############################END##################################
