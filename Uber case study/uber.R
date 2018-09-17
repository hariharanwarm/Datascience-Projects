# Clear the memory 

rm(list=ls())

##############Libraries to assist analyses#############################

library(lubridate)  # to format data and split dates conveiently into days/months/hours etc
library(tidyverse)  # tidy packages to group,summarise,mutate,plot etc
library(gridExtra)  # to plot more than one graphs in a page
library(data.table) # to rename columns conveniently
library(reshape2)   # to work on correlations
library(grid)       # for anotations in the graph
library(stats)      # for stats function

##############Import Data#####################################################

uber <- read.csv("Uber Request Data.csv",stringsAsFactors = F)
head(uber)

##############Observe data at data frame level#################################

nrow(uber) #6745
ncol(uber) #6
total_na <- sum(is.na(uber)) # 6564
Total_values <- (ncol(uber)*nrow(uber)) #40470
percentage_na <- total_na/Total_values *100 # 16% of values are NAs in the dataframe

##############Find unique values################################################

length(unique(uber$Request.id)) #6745
length(unique(uber$Pickup.point)) #2
length(unique(uber$Driver.id)) #301
length(unique(uber$Status)) #3
length(unique(uber$Request.timestamp)) #5618
length(unique(uber$Drop.timestamp)) #2599

#The data provided is for 5 days between 11th July 2016 - 15th July 2016

########################Column names##################################################

names(uber) #"Request.id" "Pickup.point" "Driver.id"  "Status"  "Request.timestamp" "Drop.timestamp"  

########################Analyse NAs/Blanks column wise################################

#Column Request.id
sum(is.na(uber$Request.id)) #Nil NAs
which(uber$Request.id=="") #Nil Blanks
#Column Pickup.point
sum(is.na(uber$Pickup.point)) #Nil NAs
which(uber$Pickup.point=="") #Nil Blanks
#Column Driver.id
sum(is.na(uber$Driver.id)) # 2650 NAs in column 'Driver.id'
which(uber$Driver.id=="") #Nil Blanks
#Column Status
sum(is.na(uber$Status))#Nil NAs
which(uber$Status=="") #Nil Blanks
#Column Request.timestamp
sum(is.na(uber$Request.timestamp))#Nil NAs
which(uber$Request.timestamp=="") #Nil Blanks
#Column Drop.timestamp
sum(is.na(uber$Drop.timestamp)) # 3914 NAs in column 'Drop.timestamp'
which(uber$Drop.timestamp=="") #Nil blanks

#1)No blank values observed in the dataframe,2)16% of the values are NAs,3)Driver.id and Drop.timestamp
#columns have the 16% of the NAs observed and the reason being those requests are either cancelled or due to
#unavailability of cars 4)NAs in Drop.timestamp are due to 'cancellations' and 'unavailablity of cars' whereas NAs
#in Driver.id is due to 'unavailablity of cars'.
#5)It is observed that the date column has date values in different lenght and is
#not suitable for analyses.Details of analysis shown as below.

#Observing the different lenghts of Dates in Column 'Request.timestamp'
req_date_length <- sapply(uber$Request.timestamp, nchar) 
unique(req_date_length) # Dates in column 'Request.timestamp' are of different lengths such as 15,14,19
#Dates in Column 'Drop.timestamp'
drop_date_length <- sapply(uber$Drop.timestamp, nchar)
unique(drop_date_length)# Dates in column 'Drop.timestamp' are of different lengths such as 15 ,14, 19, NA(this is due to absense of booking)
#the dates are reprented as timestamos i.e with a combination of date and time, difference in length is due to some dates do not have 'seconds' in them

#######################Derive new columns to work on dates #####################################


#Using 'Lubridate' functions 'dmy_hms()' and 'dmy_hm()' to fix the date format and bring all dates in a consistent format
dmyhms <- dmy_hms(uber$Request.timestamp,tz='Asia/Kolkata')
dmyhm <- dmy_hm(uber$Request.timestamp,tz='Asia/Kolkata')
dmyhms[is.na(dmyhms)] <- dmyhm[is.na(dmyhms)]
sum(is.na(dmyhms)) # Nil
length(dmyhms) #6745 which matches with the number if columns
uber$f_req_time <- dmyhms #formatted 'Request_timestamp' is added to a new column called 'f_req_time '


#fixing format of 'Drop.timestamp' 
dmyhms_d <- dmy_hms(uber$Drop.timestamp,tz='Asia/Kolkata')
dmyhm_d <- dmy_hm(uber$Drop.timestamp,tz='Asia/Kolkata')
dmyhms_d[is.na(dmyhms_d)] <- dmyhm_d[is.na(dmyhms_d)]
sum(is.na(dmyhms_d)) # the original NAs are retained which 3914
uber$f_drop_time <- dmyhms_d # Adding the formatted dates in a new column called 'f_drop_time'


#remove the original date columns which is now not needed as they are formatted to new ones
uber <- uber[,-which(names(uber)==c("Request.timestamp", "Drop.timestamp"))]

View(uber)
# Add new columns such as requested/drop date,day,hour etc
uber<-mutate(uber,request_date=date(f_req_time),request_day=weekdays(f_req_time),request_hour=hour(f_req_time),drop_date = date(f_drop_time),drop_day=weekdays(f_drop_time),
       drop_hour = hour(f_drop_time))

#Creating a dataframe with time slots
timeslot_df <- data.frame(request_hour=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                          Time_slot=factor(c('Late Night','Late Night','Late Night','Late Night',
                                             'Early Morning','Early Morning','Early Morning','Morning','Morning','Morning',
                                             'Late Morning','Late Morning','Late Morning','Afternoon','Afternoon','Afternoon','Afternoon',
                                             'Evening','Evening','Evening','Evening','Night','Night','Night'),levels = c('Early Morning','Morning','Late Morning','Afternoon','Evening','Night','Late Night')))

#Add time slots to the master data
uber <- merge(uber,timeslot_df)
#adding levels to time slot
uber$Time_slot <- factor(uber$Time_slot,levels = c('Early Morning','Morning','Late Morning','Afternoon','Evening','Night','Late Night'))


#######################Univariate Analysis - pickpoint and statuses variables#########################################
#to understand various pickup points and number of requests
table(uber$Pickup.point) #Airport-3238,City-3507
#to understand various statuses of the request and number of requests
table(uber$Status) # 'Trip Completed' - 2831,'No Cars Available' - 2650,'Cancelled'-1264

#Overall request distribution Airport/City
#Percentahe of 'no of trips completed'
round(length(which(uber$Status=='Trip Completed'))/length(uber$Status)*100)   #42% 
#Percentahe of 'no of trips cancelled'
round(length(which(uber$Status=='Cancelled'))/length(uber$Status)*100) #19%
#Percentahe of 'no of cars unavailable'
round(length(which(uber$Status=='No Cars Available'))/length(uber$Status)*100) #39%
#Overall airport/city distribution based on status
(overall_city_air_status <- ggplot(uber,aes(x=Status,fill=Status))+geom_bar()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+labs(title='Overall City/Airport Status Distribution',subtitle="It appears only 42 % of the requests are fullfilled and the rest are either \ncancelled or are not fullfilled due to unavailability of cars. ",x='Request Status',y="No. of Requests",caption='Uber case study for requests between 11July2016 - 15July2016')+
    geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',vjust=-0.5)+scale_fill_brewer(type='seq',palette = 2,direction = 1)+
    scale_y_continuous(breaks = seq(0,3000,500))+coord_cartesian(ylim = c(0,3000))+theme(panel.background = element_rect(fill = 'white',colour = 'grey50')))

#A)
#Percentage of requests from Aiport
round(length(which(uber$Pickup.point=='Airport'))/length(uber$Pickup.point)*100) #48%
#Percentage of requests from Aiport
round(length(which(uber$Pickup.point=='City'))/length(uber$Pickup.point)*100) #52%
#Plot
(overall_city_air_Pickup <- ggplot(uber,aes(x=Pickup.point,fill=Pickup.point))+geom_bar()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+labs(title='Overall City/Airport Status Distribution',subtitle="It appears only 42 % of the requests are fullfilled and the rest are either \ncancelled or are not fullfilled due to unavailability of cars. ",x='Request Status',y="No. of Requests",caption='Uber case study for requests between 11July2016 - 15July2016')+
    geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',vjust=-0.5)+scale_fill_brewer(type='seq',palette = 2,direction = 1)+
    theme(panel.background = element_rect(fill = 'white',colour = 'grey50')))

#Group by pickup points,then obtain ratios for different pickup points
(a<-group_by(uber,Pickup.point)%>%summarise(No_of_request=length(Pickup.point)) %>% mutate(Percentage=paste(round(No_of_request/sum(No_of_request)*100),'%')))
#Plot
(overall_air_city_req <- ggplot(a,aes(x=Pickup.point,y=No_of_request,fill=Pickup.point))+
    geom_bar(stat = 'identity')+
    labs(title='Overall City/Airport Pickpoint - Request Distribution',subtitle='Nearly,there is equal amount of requests flowing for City/Airport pickups \na)Airport pickup points received 48% of total requests \nb)City pickup points received 52% of total requests',caption='Uber case study for requests between 11July2016 - 15July2016',x='Pickup Point',y='No. of requests')+
    coord_cartesian(ylim = c(0,3500))+scale_y_continuous(breaks = seq(0,4000,500))+theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+geom_text(aes(label=Percentage),size=3,vjust=-0.5)+scale_fill_manual(values = c('#6498D0','#3798CC')))


#B)
#Group by different statuses,then obtain ratios for different statuses of the requests made
(b<-group_by(uber,Status)%>%summarise(No_of_request=n())%>%mutate(Percentage=paste(round(No_of_request/sum(No_of_request)*100),"%")))
#Plot
(overall_statuses_air.city<-ggplot(b,aes(x=Status,y=No_of_request,fill=Status))+geom_bar(stat = 'identity')+labs(title='Overall request status - Airport/City pickup points',subtitle='a)Only 42% of overall request is fullfilled. \nb)39% of requests are not fullfilled due to unavailability of cars. \nc)19% of requests are not fullfilled due to cancellation of request by car drivers.',caption='Uber case study for requests between 11July2016 - 15July2016',x='Pickup Point',y='No. of requests')+
    geom_text(aes(label=Percentage),size=3,vjust=-0.5)+scale_fill_manual(values = c('#AF1C32','#EC435C','#1C92C6'))+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
    coord_cartesian(ylim = c(0,3000))+theme(panel.background = element_rect(fill = 'white',colour = 'grey50')))

#C)
#Group by pickup points and status of requests ,then obtain ratios for different pickup points against different statuses
(c <- group_by(uber,Pickup.point,Status)%>%summarise(No_of_request = n()) %>% mutate(Percentage= paste0(round((No_of_request/sum(No_of_request)*100)),'%')))
#Plot
(air_city_status_per <- ggplot(c,aes(x=Pickup.point,y=No_of_request,fill=Status))+geom_bar(stat = "identity")+labs(title='City/Airport Pickpoint - Percentage Distribution',x='Pickup Point',y='No. of requests',subtitle="a)Airports suffers from 53% of 'Unavailability of Cars'. \nb)Cities suffer from 30% of 'Cancellation' of requests. \nc)Roughly,only ~40% of Airport pick-up/drop requests are fullfilled." ,caption='Uber case study for requests between 11July2016 - 15July2016')+
    geom_text(aes(label=(Percentage)),size=3,vjust=-0.5)+
    facet_wrap(~ Status,ncol = 3)+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
    coord_cartesian(ylim = c(0,1750))+scale_y_continuous(breaks = seq(0,1750,250))+
    theme(strip.background = element_rect(fill = "#686163"))+
    theme(strip.text = element_text(colour = 'white',size = 7.5))+theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_manual(values=c('#AF1C32','#EC435C','#1C92C6')))

#overall request  across all days

(overall_city_air_Dates <- ggplot(uber,aes(x=request_date,fill=request_date))+geom_bar()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
    labs(title='Overall City/Airport Status Distribution',subtitle="It appears all days have almost equal demands for cabs.",
         x='Request Status',y="No. of Requests",caption='Uber case study for requests between 11July2016 - 15July2016')+
    geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',vjust=-0.5)+
    theme(panel.background = element_rect(fill = 'white',colour = 'grey50')))



#overall request status across all days
(alldays_status<-ggplot(uber,aes(x=Status,fill=Status))+geom_bar()+facet_wrap(~request_date,nrow = 5)+
    labs(x='Request Status',y='No. of requests',title='Overall request status -  From Pickup points City/Airport '
         ,subtitle='There seeems to equal distribution of requests against various statuses on all days \na)As shown only ~40% of requests are fullfilled \nb)Unavailability of Cars is the major concern on all days \nc)Cancellation is another major concern affecting the revenue for Uber'
         ,caption='Uber case study for requests between 11July2016 - 15July2016')+
    theme(strip.background = element_rect(fill = "#686163"))+theme(strip.text = element_text(colour = 'white'))+
    theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_manual(values=c('#AF1C32','#EC435C','#1C92C6')))


##############################Analysis based on hours of the day##############################################


#group data based on request date,hour and status
(h<-group_by(uber,request_date,request_hour,Status)%>%summarise(No_of_request=n()))
#create highlights for plotting percentage figures only for problematic areas
(highligh_hr <- group_by(uber,request_date,request_hour,Status)%>%summarise(No_of_request=n())%>%mutate(Percentage=round(No_of_request/sum(No_of_request)*100),Perc=paste0(Percentage,'%'))%>%
    arrange(desc(Percentage))%>%filter(Status!='Trip Completed' & (request_hour %in% c(4,5,6,7,8,9)& Status=='Cancelled' | request_hour %in% c(17,18,19,20,21) & Status=='No Cars Available')))

View(highligh_hr)
#Plot
(hourly_alldays_status<-ggplot(h,aes(x=factor(request_hour),y=No_of_request,fill=Status))+geom_bar(stat='identity',position = 'dodge')+facet_wrap(~request_date,nrow = 5))+
  labs(x='Hour of the day(0-24 hrs format)',y='No. of Requests',title='Overall Analysis of requests - hourly basis across all dates'
       ,subtitle='a)Between 5AM-9AM there are a lot of cancellations. \nPeaks between 4AM-9AM - Annotated with Percentage of Cancellations \nat a particular hour as "High Cancellations" \nb)Between 5PM-11PM there are a lot of unavailability of cars. \nPeaks between 5PM-9PM - Annotated with percentage of unavailability \nof cars at a particular hour as "High Unavailability of cars" '
       ,caption='Uber case study for requests between 11July2016 - 15July2016')+
  geom_text(aes(label=Perc),data = highligh_hr,size=1.5,vjust=-0.7,hjust=0.7 )+theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
  theme(strip.background = element_rect(fill = "#686163"))+theme(strip.text = element_text(colour = 'white'))+
  coord_cartesian(ylim = c(0,100))+
  annotate("text",label="'High Cancellations'",x=7,y=75,size=2)+
  annotate("text",label="'High Unavailability of Cars'",size=2,x=20,y=90)+scale_fill_manual(values=c('#AF1C32','#EC435C','#CAC3C5'))

#group data based on request date,Time_slot and status
(h_timeslot<-group_by(uber,request_date,Time_slot,Status)%>%summarise(No_of_request=n())%>%mutate(Percentage=paste0(round(No_of_request/sum(No_of_request)*100),'%')))
#create highlights for plotting percentage figures only for problematic areas
highlight <- filter(h_timeslot,(Time_slot%in%c('Early Morning','Morning')&Status=='Cancelled')|(Time_slot%in%c('Evening','Night')&Status=='No Cars Available'))
View(highlight)
(Timeslot_alldays_status<-ggplot(h_timeslot,aes(x=Time_slot,y=No_of_request,fill=Status))+geom_bar(stat='identity',position  = 'dodge')+facet_wrap(~request_date,nrow = 5))+
  labs(x='Time_Slot of the day',y='No. of Requests',title='Overall Analysis of requests - Time_Slot basis across all dates'
       ,subtitle='a)Early morning and Morning relatively shows a lot of cancellations. \nAnnotated and percentage of cancellations highlighted in the graph \nas "Cancelled cars" \nb) Evening and Night shows relatively a lot of unavailability of cars. \nAnnotated and percentage of unavailability of cars highlighted in the \ngraph as "No Cars available"'
       ,caption='Uber case study for requests between 11July2016 - 15July2016')+geom_text(aes(label=Percentage),size=2,vjust=-0.4,hjust=0.75,data = highlight)+
        theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
  theme(strip.background = element_rect(fill = "#686163"))+
  theme(strip.text = element_text(colour='white'))+theme(axis.text.x = element_text(angle = 90,hjust = 1))+annotate("text",x=5.5,y=370,size=2.5,label="'No Cars available'")+annotate("text",x=1.5,y=250,label="'Cancelled cars'",size=2.5)+
  coord_cartesian(ylim = c(0,400))+scale_fill_manual(values = c('#AF1C32','#EC435C','#CAC3C5'))
     

#Airport pickup point subset
air_p <- filter(uber,Pickup.point=='Airport')
#annotation
my_text_air <- 'Concern Area'
my_grob_air = grid.text(my_text_air,x=.82,y=.94,gp=gpar(col="black",fontsize=7,fontface='bold',alpha=0.7),draw = F)
#create highlights for plotting percentage figures only for problematic areas
highlight_air <- air_p[air_p$request_hour%in%c(17,18,19,20,21)&air_p$Status=='No Cars Available',]
View(highlight_air)
#plot
(air_hourly_alldays<-ggplot(air_p,aes(x=factor(request_hour),fill=Status))+geom_bar(position='dodge')+facet_wrap(~request_date,nrow=5)+
    labs(x='Hour of the day(0-24 hrs format)',y='No. of Requests',title='Hourly Analysis of requests - Airport Pickup point',subtitle='a)Between 5PM-11PM there are a lot of unavailability of cars \n(Peaks between 5PM-9PM,Annotated as "Concern Area) \nb)There are a very few cancellations. \nc)A very few requests are fullfilled for the customers'
         ,caption='Uber case study for requests between 11July2016 - 15July2016')+annotation_custom(my_grob_air)+
    theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
    theme(strip.background = element_rect(fill = '#686163'))+
  geom_text(data =highlight_air ,aes(y=(..count..),label=scales::percent((..count..)/sum(..count..))),stat = 'count',size=1.5,vjust=-0.6,hjust=0.5)+
    theme(strip.text = element_text(colour = 'white'))+scale_fill_manual(values=c('#AF1C32','#EC435C','#CAC3C5'))+coord_cartesian(ylim = c(0,90)))


#Based on timeslot
my_grob_air_timeslot = grid.text(my_text_air,x=.7,y=.9,gp=gpar(col="black",fontsize=5,fontface='bold',alpha=0.7),draw = F)
(air_timeslot_alldays<-ggplot(air_p,aes(x=factor(Time_slot),fill=Status))+geom_bar(position='dodge')+facet_wrap(~request_date,nrow=5)+
    labs(x='Time slot of the day',y='No. of Requests',title='Time_Slot wise Analysis of requests - Airport Pickup point',subtitle='a)Evening and Night show a lot of unavailability of cars \n(Peaks during Evening,Annotated as "Concern Area") \nb)There are a very few cancellations. \nc)A very few requests are fullfilled for the customers'
         ,caption='Uber case study for requests between 11July2016 - 15July2016')+annotation_custom(my_grob_air_timeslot)+
    theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
    theme(strip.background = element_rect(fill = '#686163'))+
    theme(strip.text = element_text(colour = 'white'))+
    scale_fill_manual(values=c('#AF1C32','#EC435C','#CAC3C5'))+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
    coord_cartesian(ylim = c(0,275)))

#City pickup point subset
city_p <- filter(uber,Pickup.point=='City')
#add anotations
my_text_city <- "Concern Area"
my_grob_city = grid.text(my_text_city, x=.32,  y=.91, gp=gpar(col="black", fontsize=7, fontface="bold",alpha=0.7),draw = F)
#plot
(city_hourly_alldays <- ggplot(city_p,aes(x=factor(request_hour),fill=Status))+geom_bar(position = 'dodge')
  +facet_wrap(~request_date,nrow=5)+labs(x='Hour of the day(0-24 hrs format)',y='No. of Requests',title='Hourly Analysis of requests - City Pickup point'
                                         ,subtitle='a)Between 5AM-9AM there are a lot of cancellations. \n(Annotated as "Concern Area") \nb)Between this time there are a unavailability of cars as well \nwhich is relatively lesser than cancellations. \nc)Some requests are fullfilled for the customer.'
                                         ,caption='Uber case study for requests between 11July2016 - 15July2016')+annotation_custom(my_grob_city)+
                                          theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
                                         theme(strip.background = element_rect(fill = '#686163'))+
    theme(strip.text = element_text(colour = 'white'))+scale_fill_manual(values=c('#AF1C32','#EC435C','#CAC3C5'))+coord_cartesian(ylim = c(0,55)))

#timeslot
my_grob_city_timeslot = grid.text(my_text_air,x=.13,y=.92,gp=gpar(col="black",fontsize=5,fontface='bold',alpha=0.7),draw = F)

(city_timeslot_alldays <- ggplot(city_p,aes(x=factor(Time_slot),fill=Status))+geom_bar(position = 'dodge')
  +facet_wrap(~request_date,nrow=5)+labs(x='Time Slot of the day',y='No. of Requests',title='Time_Slot wise Analysis of requests - City Pickup point'
                                         ,subtitle='a)Early morning and Morning show a lot of cancellations. \n(Annotated as "Concern Area") \nb)Between this time slot there are a unavailability of cars as well \nwhich is relatively lesser than cancellations. \nc)Some requests are fullfilled for the customer.'
                                         ,caption='Uber case study for requests between 11July2016 - 15July2016')+annotation_custom(my_grob_city_timeslot)+
    theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
    theme(strip.background = element_rect(fill = '#686163'))+
    theme(strip.text = element_text(colour = 'white'))+
    scale_fill_manual(values=c('#AF1C32','#EC435C','#CAC3C5'))+coord_cartesian(ylim = c(0,120))+theme(axis.text.x = element_text(angle = 90,hjust = 1)))

############################Demand /Supply analysis################################

#Making a dataset for supply by subsetting the status with Trip Completed values
supply=filter(uber,Status=='Trip Completed')
supply$Status <- 'Supply'
table(supply$Status)
#Making a dataset for Demand by chnaging all values of status into demand
demand<-uber
demand$Status <-'Demand'
head(demand)
table(demand$Status)
#Plot for Overall Demand Vs Supply
ggplot(demand,aes(x=Status,fill=Status))+geom_bar(aes(x=Status))+geom_bar(data = supply,aes(x=Status,fill=Status))+
  scale_y_continuous(breaks = seq(0,7000,1000))+theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
  scale_fill_manual(values=c('#3BB333','#1D82C7'))+theme(strip.background = element_rect(fill = '#686163'))+
  theme(strip.text = element_text(colour='white'))+geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',vjust=-0.5)+
  geom_text(data=supply,aes(y=(..count..),label=scales::percent((..count..)/6745)),size=3,stat='count',vjust=-0.5)+
  labs(x='Demand vs Supply',y='Count / Percentage',caption='Uber case study for requests between 11July2016 - 15July2016',
       title='Overall Demand Vs Supply',subtitle='a)Demand is twice than Supply \nb)It appears only 42% of the demand is supplied')


#Plot for Demand Vs Supply on hourly basis across all days
ggplot(demand,aes(x=request_hour,fill=Status))+geom_histogram(binwidth = 1)+
  geom_text(aes(y=(..count..),label=(..count..)),size=1,stat='count',vjust=-0.5)+
  geom_bar(data=supply,aes(fill=Status),position = 'dodge')+
  geom_text(data=supply,aes(y=(..count..),label=(..count..)),size=1,stat='count',vjust=-0.5)+
  facet_wrap(~request_date,nrow=5)+scale_fill_manual(values=c('#3BB333','#1D82C7'))+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+theme(strip.background = element_rect(fill = '#686163'))+
  theme(strip.text = element_text(colour='white'))+scale_x_continuous(breaks = seq(0,23,1))+
  labs(x='Hour of the day(24hrs format)',y='Demand vs Supply',caption='Uber case study for requests between 11July2016 - 15July2016',
       title='Demand Vs Supply - Hourly basis across all Days',subtitle='Demand is twice than Supply')+
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)))


#####################Reason for cancellations and unavailability of cars##########################

#duration of journey from City to Airport
duration_frmcity<-na.omit(uber)
duration_frmcity<-duration_frmcity[which(duration_frmcity$Pickup.point=='City'),]
duration_frmcity$duration <- round(with(duration_frmcity, as.numeric(difftime(f_drop_time,f_req_time, unit = 'min'))))
View(duration_frmcity)
#plot
ggplot(duration_frmcity,aes(x=Pickup.point,y=duration))+geom_boxplot()+coord_cartesian(ylim = c(0,90))+
  labs(x="Pickup point 'City'",y="Duration of Travel",title='Duration of travel from City to Airport',
       subtitle='It appears the median journey from City to Airport is 53 Mins.\nMin : 21 \n1st Quartile : 41 \nMedian : 53 \nMean : 52.57 \n3rd Quartile : 64 \nMax : 83',
       caption='Uber case study for requests between 11July2016 - 15July2016')+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))
summary(duration_frmcity$duration)


#duration of journey from Airport to City
duration_frmairport<-na.omit(uber)
duration_frmairport<-duration_frmairport[which(duration_frmairport$Pickup.point=='Airport'),]
duration_frmairport$duration <- round(with(duration_frmairport, as.numeric(difftime(f_drop_time,f_req_time, unit = 'min'))))
View(duration_frmairport)
#plot
ggplot(duration_frmairport,aes(x=Pickup.point,y=duration))+geom_boxplot()+coord_cartesian(ylim = c(0,90))+
  labs(x="Pickup point 'Airport'",y="Duration of Travel",title='Duration of travel from Airport to City',
       subtitle='It appears the median journey from City to Airport is 52 Mins.\nMin : 21 \n1st Quartile : 41 \nMedian : 52 \nMean : 52.24 \n3rd Quartile : 64 \nMax : 82',
       caption='Uber case study for requests between 11July2016 - 15July2016')+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))
summary(duration_frmairport$duration)

############################################END###############################################################
