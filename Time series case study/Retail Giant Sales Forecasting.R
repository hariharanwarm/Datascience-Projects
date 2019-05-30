#**********************************************************************************************************#
# Business understanding:
#------------------------
#"Global Mart" is an online store super giant having worldwide operations. Objective of this casestudy
#is to  find out 2 most profitable (and consistent) segment from 21 market buckets and forecast the sales 
#and demand for these segments.

#**********************************************************************************************************#

# Load required libraries

library(ggplot2)
library(dplyr)
library(stringr)
library(graphics)
library(forecast)
library(tseries)
library(gridExtra)
library(magrittr)
library(stats)
library(raster)

#**********************************************************************************************************#
#Steps followed during the execution of this Case Study/Project:
#Business Understanding.
#Data Understanding and Feature Engineering.
#Data Preparation and deriving 21 Market Buckets from transactional data.
#Identifying the Optimal Profit Market Buckets as Focus Area of the Forecasting Model. 
#Building Time Series Models to Forecast Demand and Sales of aforementioned Market Bucket segments. 
#Model Evaluation[MAPE], Model Selection, Forecasting Demand and Sales for the future 6 month Period.
#Business Understanding *************************
# GLOBAL MART is an online retail giant having worldwide operations in 147 countries grouped into 7 Global Market Regions.
# GLOBAL MART retails wide range of entities from 3 product categories [1. Technology 2. Furniture 3. Office Supplies]
# It serves customers from 3 consumer segments [1. Consumer 2. Home Office 3. Corporate]
# The Sales/Operations requires to finalize the plan for the next six months by forecasting the demand and sales for the nect six months.
# This forecast would help manage revenue and inventory accordingly.
# Goals of this Case Study
#1. Subset the data from the transactional database to form the 21 Market Buckets [7 Global Market Regions x 3 Consumer Segments].
#2. Identify the top 2 Most Profitable and Consistently Profitable Market Buckets.
#3. Build a Time Series Model to forecast the Demand and Sales for these two Market Buckets using 1. Classical Decomposition and 2.Auto Arima
#4. Perform Model Evaluation using MAPE and Accuracy. Use the final models to predict the Demand and Sales of the next 6 months for the 2 Market Buckets.
#Data Understanding *************************


# Load the dataset

globalmartSuperstore<- read.csv("Global Superstore.csv")

head(globalmartSuperstore)
# Checking the structure of globalmartSuperstore
str(globalmartSuperstore)

dim(globalmartSuperstore)
# No: of rows = 51290
# No: of columns = 24


#**********************************************************************************************************#
#                     Data Understanding
#**********************************************************************************************************#


# Check duplicate values

sum(duplicated(globalmartSuperstore)) # No duplicates 

# Check NA values

sapply(globalmartSuperstore, function(x) sum(is.na(x))) # Only Postal.code has 41296 NA values


# Check missing values

sapply(globalmartSuperstore, function(x) length(which(x == ""))) # No missing/blank valuess

# Check number of unique values in each column

sapply(globalmartSuperstore, function(x) length(unique(x)))

# There are 3 segments and 7 markets

#**********************************************************************************************************#
#                     Data Cleaning
#**********************************************************************************************************#

# Removing postal code as it has a lot of NA values

globalmartSuperstore$Postal.Code <- NULL;

str(globalmartSuperstore)

# Change the date format of Order.Date and Ship.Date

globalmartSuperstore$Order.Date <- as.Date(globalmartSuperstore$Order.Date,"%d-%m-%Y")
globalmartSuperstore$Ship.Date <- as.Date(globalmartSuperstore$Ship.Date,"%d-%m-%Y")

#View(globalmartSuperstore)

# Check unique values of segment and market
unique(globalmartSuperstore$Segment) # 3 segments
#There are 3 unique product categories sold by GLOBAL MART [1]"Technology" [2]"Furniture" [3]"Office Supplies"
unique(globalmartSuperstore$Market) # 7 markets
#There are 7 unique Market Segments as follows:
#1. Africa - African Continent
#2. APAC - Asia Pacific/Asia Central
#3. Canada - Canada
#4. EMEA - Europe, Middle East and Africa
#5. EU - European Union
#6. LATAM - Latin America
#7. US - United States of America





#*******************Create 21 datasegments*************************************************#
globalmart_V2 <- globalmartSuperstore
globalmart_V2$MarketSegment <- paste(globalmart_V2$Market, globalmart_V2$Segment, sep = "-")

unique(globalmart_V2$MarketSegment) 

# Extract the order month since we need to aggregate to month level
globalmart_V2$Order.Month <- as.numeric(format(globalmart_V2$Order.Date , "%m"))
globalmart_V2$Order.Year <- as.numeric(format(globalmart_V2$Order.Date , "%Y"))


#EDA
#Univariate
summary(globalmartSuperstore$Order.Date)
ggplot(globalmartSuperstore,aes(x=Segment))+geom_bar()
ggplot(globalmartSuperstore,aes(x=Country))+geom_bar()+coord_flip()
ggplot(globalmartSuperstore,aes(x=Category))+geom_bar()
ggplot(globalmartSuperstore,aes(x=Sub.Category))+geom_bar()+coord_flip()
ggplot(globalmartSuperstore,aes(x=Region))+geom_bar()+coord_flip()
ggplot(globalmartSuperstore,aes(x=Market))+geom_bar()+coord_flip()
ggplot(globalmartSuperstore,aes(x=Order.Priority))+geom_bar()
summary(globalmartSuperstore$Discount)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.1429  0.2000  0.8500 
summary(globalmartSuperstore$Profit)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -6599.98     0.00     9.24    28.61    36.81  8399.98 
summary(globalmartSuperstore$Quantity)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   3.000   3.477   5.000  14.000 
summary(globalmartSuperstore$Sales)
#     Min.   1st Qu.    Median      Mean    3rd Qu.      Max. 
#   0.444    30.759     85.053     246.491  251.053    22638.480 
summary(globalmartSuperstore$Shipping.Cost)
#   Min.  1st Qu.  Median    Mean  3rd Qu.    Max. 
#   0.00    2.61    7.79    26.38   24.45    933.57 

#Bivariate
ggplot(globalmartSuperstore,aes(x=Order.Date,y=Sales))+geom_point()
ggplot(globalmartSuperstore,aes(x=Ship.Date,y=Sales))+geom_point()
ggplot(globalmartSuperstore,aes(x=Order.Date,y=Sales))+geom_point()
ggplot(globalmartSuperstore,aes(x=Ship.Date,y=Sales))+geom_point()

#Subset of dataset

table(globalmartSuperstore$Market)
# APAC Africa Canada   EMEA     EU  LATAM     US 
#11002   4587    384   5029  10000  10294   9994 
table(globalmartSuperstore$Segment)
#   Consumer   Corporate Home Office 
#    26518       15429        9343 

#Derive new columns

globalmartSuperstore_mod <- mutate(globalmartSuperstore,Market_Segment=paste(Market,Segment,sep = "-"),order_month=month(Order.Date),order_year=year(Order.Date))
dim(globalmartSuperstore_mod)
table(globalmartSuperstore_mod$Market_Segment)
table(globalmartSuperstore_mod$order_month)
table(globalmartSuperstore_mod$order_year)
str(globalmartSuperstore_mod)
#Group Maket and Segment and View Sales,Profit and Quantity

globalmartSuperstore_grouped <- group_by(globalmartSuperstore_mod,Market_Segment,order_year,order_month) %>% 
  summarise(Sales_sum = sum(Sales),Profit_sum = sum(Profit),Quantity_sum = sum(Quantity),CV = sd(Profit)/mean(Profit)*100)
View(globalmartSuperstore_grouped)
dim(globalmartSuperstore_grouped)

table(globalmartSuperstore_grouped$Market_Segment)
globalmartSuperstore_grouped_row_no <- group_by(globalmartSuperstore_grouped,Market_Segment)%>%mutate(in_months=row_number(Market_Segment))
View(globalmartSuperstore_grouped_row_no)
#plots
str(globalmartSuperstore_grouped_row_no)
table(globalmartSuperstore_grouped$Market_Segment)
#Plot Quantity
ggplot(globalmartSuperstore_grouped_row_no,aes(x=in_months,y=Quantity_sum))+geom_line(aes(col=Market_Segment))

#Plot Profit
ggplot(globalmartSuperstore_grouped_row_no,aes(x=in_months,y=Profit_sum))+geom_line(aes(col=Market_Segment))
#Plot CV
ggplot(globalmartSuperstore_grouped_row_no,aes(x=in_months,y=CV))+geom_line(aes(col=Market_Segment))

# Analyse segment wise

#1
APAC <- filter(globalmartSuperstore_grouped_row_no,Market_Segment %in% c('APAC-Consumer','APAC-Corporate','APAC-Home Office'))
dim(APAC)

#Plot Quantity
(APAC_quantity <- ggplot(APAC,aes(x=in_months,y=Quantity_sum))+geom_line(aes(col=Market_Segment)))
#Plot Profit
(APAC_profit <- ggplot(APAC,aes(x=in_months,y=Profit_sum))+geom_line(aes(col=Market_Segment)))
#Plot CV
(APAC_cv<-ggplot(APAC,aes(x=in_months,y=CV))+geom_line(aes(col=Market_Segment)))

#2
AFRICA <- filter(globalmartSuperstore_grouped_row_no,Market_Segment %in% c('Africa-Corporate','Africa-Home Office','Africa-Consumer'))
dim(AFRICA)
#Plot Quantity
(AFRICA_quantity <- ggplot(AFRICA,aes(x=in_months,y=Quantity_sum))+geom_line(aes(col=Market_Segment)))
#Plot Profit
(AFRICA_profit <- ggplot(AFRICA,aes(x=in_months,y=Profit_sum))+geom_line(aes(col=Market_Segment)))
#Plot CV
(AFRICA_cv<-ggplot(AFRICA,aes(x=in_months,y=CV))+geom_line(aes(col=Market_Segment)))

#3
CANADA <- filter(globalmartSuperstore_grouped_row_no,Market_Segment %in% c('Canada-Consumer', 'Canada-Corporate','Canada-Home Office'))
dim(CANADA)
#Plot Quantity
(CANADA_quantity <- ggplot(CANADA,aes(x=in_months,y=Quantity_sum))+geom_line(aes(col=Market_Segment)))
#Plot Profit
(CANADA_profit <- ggplot(CANADA,aes(x=in_months,y=Profit_sum))+geom_line(aes(col=Market_Segment)))
#Plot CV
(CANADA_cv<-ggplot(CANADA,aes(x=in_months,y=CV))+geom_line(aes(col=Market_Segment)))

#4

EMEA <- filter(globalmartSuperstore_grouped_row_no,Market_Segment %in% c('EMEA-Consumer','EMEA-Corporate','EMEA-Home Office' ))
dim(EMEA)
#Plot Quantity
(EMEA_quantity <- ggplot(EMEA,aes(x=in_months,y=Quantity_sum))+geom_line(aes(col=Market_Segment)))
#Plot Profit
(EMEA_profit <- ggplot(EMEA,aes(x=in_months,y=Profit_sum))+geom_line(aes(col=Market_Segment)))
#Plot CV
(EMEA_cv<-ggplot(EMEA,aes(x=in_months,y=CV))+geom_line(aes(col=Market_Segment)))

#5

EU <- filter(globalmartSuperstore_grouped_row_no,Market_Segment %in% c('EU-Consumer','EU-Corporate','EU-Home Office'))
dim(EU)
#Plot Quantity
(EU_quantity <- ggplot(EU,aes(x=in_months,y=Quantity_sum))+geom_line(aes(col=Market_Segment)))
#Plot Profit
(EU_profit <- ggplot(EU,aes(x=in_months,y=Profit_sum))+geom_line(aes(col=Market_Segment)))
#Plot CV
(EU_cv<-ggplot(EU,aes(x=in_months,y=CV))+geom_line(aes(col=Market_Segment)))

#6

LATAM <- filter(globalmartSuperstore_grouped_row_no,Market_Segment %in% c('LATAM-Corporate','LATAM-Home Office','LATAM-Consumer'))
dim(LATAM)
#Plot Quantity
(LATAM_quantity <- ggplot(LATAM,aes(x=in_months,y=Quantity_sum))+geom_line(aes(col=Market_Segment)))
#Plot Profit
(LATAM_profit <- ggplot(LATAM,aes(x=in_months,y=Profit_sum))+geom_line(aes(col=Market_Segment)))
#Plot CV
(LATAM_cv<-ggplot(LATAM,aes(x=in_months,y=CV))+geom_line(aes(col=Market_Segment)))

#7

US <- filter(globalmartSuperstore_grouped_row_no,Market_Segment %in% c('US-Corporate','US-Home Office','US-Consumer'))
dim(US)
#Plot Quantity
(US_quantity <- ggplot(US,aes(x=in_months,y=Quantity_sum))+geom_line(aes(col=Market_Segment)))
#Plot Profit
(US_profit <- ggplot(US,aes(x=in_months,y=Profit_sum))+geom_line(aes(col=Market_Segment)))
#Plot CV
(US_cv<-ggplot(US,aes(x=in_months,y=CV))+geom_line(aes(col=Market_Segment)))

library(gridExtra)


profit_grid<-grid.arrange(APAC_profit,AFRICA_profit,CANADA_profit,EMEA_profit,EU_profit,LATAM_profit,
                          US_profit)

cv_grid <- grid.arrange(APAC_cv,AFRICA_cv,CANADA_cv,EMEA_cv,EU_cv,LATAM_cv,US_cv)


#**********************************************************************************************#

# Aggregating the values monthly and finding 21 market segments 

str(globalmart_V2)
segmented_summary <- globalmart_V2 %>% mutate(month = Order.Month, year = Order.Year) %>%
  group_by(Market,MarketSegment, year, month) %>%
  summarise(monthly_sales = sum(Sales),
            monthly_qty = sum(Quantity), 
            monthly_profit = sum(Profit))

segmented_summary


cv <- segmented_summary %>%  group_by(MarketSegment) %>% summarise(.,CV = cv(monthly_profit))



# Plotting the Coefficent of Variation by MarketSegment
ggplot(cv,aes(MarketSegment,CV,fill="Blue"))+geom_bar(position = "dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Plotting the total profit by MarketSegment
ggplot(Total,aes(MarketSegment,total_profit,fill=Market))+geom_bar(position = "dodge",stat = "identity")+
  xlab("Market")+ylab("TotalProfit")+ggtitle("Total profit by MarketSegment")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Based on the above analysis it is clear that 
# APAC and EU are the most profitable Markets
# Most profitable market segments are EU-Consumer and APAC-Consumer

#------------------------------------------------------------------------------------------------#
# Model Building 

#------------------------------------------------------------------------------------------------#

#[1]. APAC_CONSUMER - Asia Pacific/Asia Central Consumer Market Bucket
#We will create seperate dataframe frames for Demand and Sales. 
#These dataframes will be used to build the time series forecasting model for APAC_Consumer Monthly Demand and Monthly Sales respectively.

#This same Month.Code will be used for aggregation and will serve as the new Time Stamps for our Forecasting Model.
globalmartSuperstore$Month.Code <- sapply(globalmartSuperstore$Order.Date, function(x) length(seq(from= min(globalmartSuperstore$Order.Date), to=x, by='month')))
range(globalmartSuperstore$Month.Code)

apac_consmr<- subset(globalmartSuperstore, globalmartSuperstore$Segment=="Consumer" & globalmartSuperstore$Market=="APAC")
eu_consmr<- subset(globalmartSuperstore, globalmartSuperstore$Segment=="Consumer" & globalmartSuperstore$Market=="EU")
parameter_aggregation<- function(input_df)
{
  input_df$Month.Code<- as.numeric(input_df$Month.Code)
  input_df<- input_df %>% group_by(Month.Code) %>% summarise(Monthly.Profit = sum(Profit), Monthly.Sales= sum(Sales), Monthly.Demand= sum(Quantity)) %>% 
    mutate(Net.Profit = sum(Monthly.Profit), Average.Profit = mean(Monthly.Profit), Coeff.Var= (sd(Monthly.Profit)/mean(Monthly.Profit)))
  input_df<- sapply(input_df, function(x) round(x,2))
  return(data.frame(input_df))
}
apac_consmr<- parameter_aggregation(apac_consmr)
eu_consmr<- parameter_aggregation(eu_consmr)

#[1]. APAC_CONSUMER - Asia Pacific/Asia Central Consumer Market Bucket
#We will create seperate dataframe frames for Demand and Sales. 
#These dataframes will be used to build the time series forecasting model for APAC_Consumer Monthly Demand and Monthly Sales respectively.
demand.apac_consmr<- apac_consmr[,c("Month.Code","Monthly.Demand")]
sales.apac_consmr<- apac_consmr[,c("Month.Code","Monthly.Sales")]
row_count<- nrow(apac_consmr)
#APAC_CONSUMER Demand Modeling Section *****
#We will build two demand forecasting models using the below techniques:
#[1] Classical Decomposition
#[2] Auto ARIMA
#We will then evaluate the two models based on MAPE metric for Forecasts on 6 month out of bag values.
#Therefore, we need to divide the demand.apac_consmr dataframe into 42 rows for model building and 6 rows for testing.
test.demand.apac_consmr<- demand.apac_consmr[(row_count-5):row_count,]
demand.apac_consmr<- demand.apac_consmr[1:(row_count-6),]
demand.apac_consmr.timeser<- ts(demand.apac_consmr[,"Monthly.Demand"])
demand.apac_consmr.timeser
#Let us begin to understand the individual components of our Demand Time Series
#We will decompose the components and view it graphically. Since decomposition cannot happen under the frequency of 1.
#And we are viewing monthly data. It is most logical to look at it in a 12 month/1 year format. So we will set the frequency to 12. And obseve the components of the decomposed timeseries
decomposed.demand.apac_consmr<- ts(demand.apac_consmr[,"Monthly.Demand"], frequency = 12)
decomposed.demand.apac_consmr<- decompose(decomposed.demand.apac_consmr)
decomposed.demand.apac_consmr
plot(decomposed.demand.apac_consmr)
#It is clear from the decomposition that the Demand:
#[1] A linearly increasing trend with a positive slope
#[2] A low amplitude sinusoidal seasonality ranging between -180 to +150 units.
#[3] The Demand Time Series will be and Additive Forecasting Model
#We will now model these components.
#Plotting the Demand Time Series
plot(demand.apac_consmr.timeser, col="red", lwd=2)
#[1] Classical Decomposition Model for Demand Forecasting APAC_Consumer
#Defining a Moving Average Smoothing Function
ts_movavg_smoother<- function(inp_timsr, width)
{
  smothed_timsr<- stats::filter(inp_timsr, filter = rep(1/(2*width+1),(2*width+1)), method = "convolution", sides = 2)
  #In smothed_timesr, the starting and ending records of lenght equal to width are missing therefore we will smooth and impute these missing values
  #Smoothing Left Half
  left_diff<- smothed_timsr[width+2] - smothed_timsr[width+1]
  for(i in seq(from=width, to=1, by=-1))
  {
    smothed_timsr[i]<- smothed_timsr[i+1] - left_diff
  }
  
  #Smoothing Right Half
  row_count<- length(smothed_timsr)
  right_diff<- smothed_timsr[row_count-width] - smothed_timsr[row_count-width-1]
  for(i in seq(from=row_count-width, to=row_count, by=1))
  {
    smothed_timsr[i]<- smothed_timsr[i-1] + right_diff
  }
  
  return(as.vector(smothed_timsr))
}
#Smoothening the demand for APAC_Consumer Demand using the aforementioned ts_movavg_smoother(input_timeseries, width)
smothed_demand.apac_consmr<- ts_movavg_smoother(demand.apac_consmr.timeser, 1)
lines(smothed_demand.apac_consmr, col="blue", lwd=2)
smothed_demand.apac_consmr<- cbind(demand.apac_consmr$Month.Code,smothed_demand.apac_consmr)
smothed_demand.apac_consmr<- data.frame(smothed_demand.apac_consmr)
colnames(smothed_demand.apac_consmr)<- c("Month.Code","Monthly.Demand")
#With the smoothed demand dataframe. Let us begin modeling the global trend aspect
lmfit_demand.apac_consmr <- lm(Monthly.Demand~ sin(0.5*Month.Code)*poly(Month.Code,1)+
                                 cos(0.1*Month.Code)*poly(Month.Code,1)+
                                 tan(0.02*Month.Code), data = smothed_demand.apac_consmr)
lmfit_demand.apac_consmr
globcomp_demand.apac_consmr <- predict(lmfit_demand.apac_consmr, data=smothed_demand.apac_consmr$Month.Code)
#We will now plot the global_component globcomp_demand.apac_consmr over the smoothed Demand Time Series.
#Note the Graph Shows: Monthly Demand Time series in Red, Smoothed Monthly Demand in Blue and the Forecasted Global Component in Green
lines(globcomp_demand.apac_consmr, col="green", lwd=2)
accuracy(globcomp_demand.apac_consmr, smothed_demand.apac_consmr$Monthly.Demand)
#Now that we have modeled the Trend and Seasonality let us isolate the Local Component and analyze its ARMA components
loclcomp_demand.apac_consmr<- demand.apac_consmr.timeser - globcomp_demand.apac_consmr
#Now let's plot this local component and analyze it graphically
plot(loclcomp_demand.apac_consmr, col="red")
#Testing acf and pacf plots for this local component to check for weak stationarity
acf(loclcomp_demand.apac_consmr)
pacf(loclcomp_demand.apac_consmr)
#Let us check if this local component can be modeled as AR(p) or MA(q) series.
locfitarma_demand.apac_consmr<- auto.arima(loclcomp_demand.apac_consmr)
locfitarma_demand.apac_consmr
#ARIMA(0,0,0) with zero mean ; This implies that there is no AR(p) or MA(q) series in the local component
#We will test the residual for white noise
resi_clasdec_demand.apac_consmr<- loclcomp_demand.apac_consmr - fitted(locfitarma_demand.apac_consmr)
#Plotting the Residual
plot(resi_clasdec_demand.apac_consmr, col="red")
#Testing the residual for White Noise
adf.test(resi_clasdec_demand.apac_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the global and local components for demand is stationary.
kpss.test(resi_clasdec_demand.apac_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for demand is stationary.
#Let us forecast the Demand using classical decomposition model for next 6 months [Month Code: 43,44,45,46,47,48].
fcast_clasdec_demand.apac_consmr<- predict.lm(lmfit_demand.apac_consmr, data.frame(Month.Code=test.demand.apac_consmr$Month.Code))
fcast_clasdec_demand.apac_consmr
#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.apac_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_clasdec_demand.apac_consmr<- accuracy(fcast_clasdec_demand.apac_consmr, test.demand.apac_consmr$Monthly.Demand)
acurcy_clasdec_demand.apac_consmr
mape_clasdec_demand.apac_consmr<-acurcy_clasdec_demand.apac_consmr[5]
mape_clasdec_demand.apac_consmr
#We Obtained the following results on the Demand forecasting accuracy of Classical Decomposition model:
#             ME       RMSE      MAE        MPE       MAPE
#Test set -23.16084  127.4945  107.1754  -8.815864   18.79196
#Finally lets plot the Classical Decomposition Demand forecast along with the Actual Demand for APAC_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Classical Decomposition Demand in Blue.
full_clasdec_demand.apac_consmr<- ts(c(globcomp_demand.apac_consmr, fcast_clasdec_demand.apac_consmr))
plot(ts(apac_consmr[,"Monthly.Demand"]), col="red", lwd=2)
lines(full_clasdec_demand.apac_consmr, col="blue", lwd=2)
#[2] Trying the Auto Arima Model 
atoarima_demand.apac_consmr<- auto.arima(demand.apac_consmr.timeser)
atoarima_demand.apac_consmr
#The auto arima model was of ARIMA(0,1,0) ; This implies that 1 stage differencing was performed.
#sigma^2 estimated as 25366  |  log likelihood=-266.07 |  AIC=534.14  |  AICc=534.24  |  BIC=535.85
tsdiag(atoarima_demand.apac_consmr)
#We will now plot the Original Demand Time Series in Red and the Fitted Auto Arima Model in Blue.
plot(atoarima_demand.apac_consmr$x, col="red", lwd=2)
lines(fitted(atoarima_demand.apac_consmr), col="blue", lwd=2)
#The Demand Auto Arima Model shows a distinct lag/offeset to the right. Implying that it reacts later to fluctuations in Demand.
#Let us test the residual values after the removing the fitted auto arima model for Demand.
#Ideally these residual values must represent white noise.
resi_atoarima_demand.apac_consmr <- demand.apac_consmr.timeser - fitted(atoarima_demand.apac_consmr)
adf.test(resi_atoarima_demand.apac_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the fitted auto arima for demand is stationary.
kpss.test(resi_atoarima_demand.apac_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for demand is stationary.
#Let us forecast the Demand using for atoarima_demand.apac_consmr next 6 months [Month Code: 43,44,45,46,47,48].
fcast_atoarima_demand.apac_consmr<- predict(atoarima_demand.apac_consmr, n.ahead = 6)
fcast_atoarima_demand.apac_consmr
#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.apac_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_atoarima_demand.apac_consmr<- accuracy(fcast_atoarima_demand.apac_consmr$pred, test.demand.apac_consmr$Monthly.Demand)
acurcy_atoarima_demand.apac_consmr
mape_atoarima_demand.apac_consmr<-acurcy_atoarima_demand.apac_consmr[5]
mape_atoarima_demand.apac_consmr
#We Obtained the following results on the Demand forecasting accuracy of Auto ARIMA model:
#         ME   RMSE     MAE       MPE     MAPE
#Test set 12 174.3722 147.6667 -7.362467 26.24458
#Finally lets plot the Auto Arima forecast along with the Actual Demand for APAC_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Auto Arima Model in Blue.
full_atoarima_demand.apac_consmr<- ts(c(fitted(atoarima_demand.apac_consmr), fcast_atoarima_demand.apac_consmr$pred))
plot(ts(apac_consmr[,"Monthly.Demand"]), col="red", lwd=2)
lines(full_atoarima_demand.apac_consmr, col="blue", lwd=2)
#***** APAC_CONSUMER Sales Modeling Section *****#
#We will build two sales forecasting models using the below techniques:
#[1] Classical Decomposition
#[2] Auto ARIMA
#We will then evaluate the two models based on MAPE metric for Forecasts on 6 month out of bag values.
#Therefore, we need to divide the sales.apac_consmr dataframe into 42 rows for model building and 6 rows for testing.
test.sales.apac_consmr<- sales.apac_consmr[(row_count-5):row_count,]
sales.apac_consmr<- sales.apac_consmr[1:(row_count-6),]
sales.apac_consmr.timeser<- ts(sales.apac_consmr[,"Monthly.Sales"])
sales.apac_consmr.timeser
#Let us begin to understand the individual components of our Sales Time Series
#We will decompose the components and view it graphically. Since decomposition cannot happen under the frequency of 1.
#And we are viewing monthly data. It is most logical to look at it in a 12 month/1 year format. So we will set the frequency to 12. And obseve the components of the decomposed timeseries
decomposed.sales.apac_consmr<- ts(sales.apac_consmr[,"Monthly.Sales"], frequency = 12)
decomposed.sales.apac_consmr<- decompose(decomposed.sales.apac_consmr)
decomposed.sales.apac_consmr
plot(decomposed.sales.apac_consmr)
#It is clear from the decomposition that the Demand:
#[1] A linearly increasing trend with a positive slope
#[2] A high amplitude sinusoidal seasonality ranging between -15000 to +15000 units.
#[3] The Sales Time Series will be and Additive Forecasting Model
#We will now model these components.
#Plotting the Sales Time Series
plot(sales.apac_consmr.timeser, col="red", lwd=2)
#[1] Classical Decomposition Model of APAC Consumer Sales
#Smoothening the monthly sales for APAC_Consumer Sales using the aforementioned ts_movavg_smoother(input_timeseries, width)
smothed_sales.apac_consmr<- ts_movavg_smoother(sales.apac_consmr.timeser, 1)
lines(smothed_sales.apac_consmr, col="blue", lwd=2)
smothed_sales.apac_consmr<- cbind(sales.apac_consmr$Month.Code,smothed_sales.apac_consmr)
smothed_sales.apac_consmr<- data.frame(smothed_sales.apac_consmr)
colnames(smothed_sales.apac_consmr)<- c("Month.Code","Monthly.Sales")
#With the smoothed sales dataframe. Let us begin modeling the global trend aspect
lmfit_sales.apac_consmr <- lm(Monthly.Sales~ sin(0.5*Month.Code)*poly(Month.Code,1)+
                                cos(0.05*Month.Code)*poly(Month.Code,1)+
                                tan(0.02*Month.Code), data = smothed_sales.apac_consmr)
lmfit_sales.apac_consmr
globcomp_sales.apac_consmr <- predict(lmfit_sales.apac_consmr, data=smothed_sales.apac_consmr$Month.Code)
#We will now plot the global_component globcomp_sales.apac_consmr over the smoothed Sales Time Series.
#Note the Graph Shows: Monthly Sales Time series in Red, Smoothed Monthly Sales in Blue and the Forecasted Global Component in Green
lines(globcomp_sales.apac_consmr, col="green", lwd=2)
accuracy(globcomp_sales.apac_consmr, smothed_sales.apac_consmr$Monthly.Sales)
#Now that we have modeled the Trend and Seasonality let us isolate the Local Component and analyze its ARMA components
loclcomp_sales.apac_consmr<- sales.apac_consmr.timeser - globcomp_sales.apac_consmr
#Now let's plot this local component and analyze it graphically
plot(loclcomp_sales.apac_consmr, col="red")
#Testing acf and pacf plots for this local component to check for weak stationarity
acf(loclcomp_sales.apac_consmr)
pacf(loclcomp_sales.apac_consmr)
#Let us check if this local component can be modeled as AR(p) or MA(q) series.
locfitarma_sales.apac_consmr<- auto.arima(loclcomp_sales.apac_consmr)
locfitarma_sales.apac_consmr
#ARIMA(0,0,0) with zero mean ; This implies that there is no AR(p) or MA(q) series in the local component
#We will test the residual for white noise
resi_clasdec_sales.apac_consmr<- loclcomp_sales.apac_consmr - fitted(locfitarma_sales.apac_consmr)
#Plotting the Residual
plot(resi_clasdec_sales.apac_consmr, col="red")
#Testing the residual for White Noise
adf.test(resi_clasdec_sales.apac_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the global and local components for sales is stationary.
kpss.test(resi_clasdec_sales.apac_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for sales is stationairy.
#Let us forecast the Sales using classical decomposition model for next 6 months [Month Code: 43,44,45,46,47,48].
fcast_clasdec_sales.apac_consmr<- predict.lm(lmfit_sales.apac_consmr, data.frame(Month.Code=test.sales.apac_consmr$Month.Code))
fcast_clasdec_sales.apac_consmr
#We will compare the aforementioned forecasted values with Actual Sales from the test.sales.apac_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Sales vs Actual Monthly Sales of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_clasdec_sales.apac_consmr<- accuracy(fcast_clasdec_sales.apac_consmr, test.sales.apac_consmr$Monthly.Sales)
acurcy_clasdec_sales.apac_consmr
mape_clasdec_sales.apac_consmr<-acurcy_clasdec_sales.apac_consmr[5]
mape_clasdec_sales.apac_consmr
#We Obtained the following results on the Sales forecasting accuracy of Classical Decomposition model:
#             ME       RMSE      MAE        MPE       MAPE
#Test set  6515.587  14997.64  12937.14   5.024684  20.83351
#Finally lets plot the Classical Decomposition Sales forecast along with the Actual Sales for APAC_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Classical Decomposition Demand in Blue.
full_clasdec_sales.apac_consmr<- ts(c(globcomp_sales.apac_consmr, fcast_clasdec_sales.apac_consmr))
plot(ts(apac_consmr[,"Monthly.Sales"]), col="red", lwd=2)
lines(full_clasdec_sales.apac_consmr, col="blue", lwd=2)
#[2] Trying the Auto Arima Model 
atoarima_sales.apac_consmr<- auto.arima(sales.apac_consmr.timeser)
atoarima_sales.apac_consmr
#The auto arima model was of ARIMA(0,1,1)  
#This implies that 1 stage differencing was performed and the resulting timeseries was modeled as MA(1).
#The standard error for the MA(1) coefficient is 18.3% of the coefficient. This is relatively high but we will accept it with caution 
#sigma^2 estimated as 174361546  |  log likelihood=-447.11  |  AIC=898.23  |  AICc=898.55  |  BIC=901.66
tsdiag(atoarima_sales.apac_consmr)
#We will now plot the Original Sales Time Series in Red and the Fitted Auto Arima Model in Blue.
plot(atoarima_sales.apac_consmr$x, col="red", lwd=2)
lines(fitted(atoarima_sales.apac_consmr), col="blue", lwd=2)
#The Sales Auto Arima Model shows a poor fit when overlaid on the Actual Sales.
#This implies that the sales forecasting model is not reponding to fluctuations in the Actual Sales on both the high and low end.
#Let us test the residual values after the removing the fitted auto arima model for Sales.
#Ideally these residual values must represent white noise.
resi_atoarima_sales.apac_consmr <- sales.apac_consmr.timeser - fitted(atoarima_sales.apac_consmr)
adf.test(resi_atoarima_sales.apac_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the fitted auto arima for sales is stationairy.
kpss.test(resi_atoarima_sales.apac_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for sales is stationairy.
#Let us forecast the sales using for atoarima_sales.apac_consmr next 6 months [Month Code: 43,44,45,46,47,48].
fcast_atoarima_sales.apac_consmr<- predict(atoarima_sales.apac_consmr, n.ahead = 6)
fcast_atoarima_sales.apac_consmr
#We will compare the aforementioned forecasted values with Actual sales from the test.sales.apac_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Sales vs Actual Monthly Sales of [Month Code: 43,44,45,46,47,48] using MAPE
acurracy_atoarima_sales.apac_consmr<- accuracy(fcast_atoarima_sales.apac_consmr$pred, test.sales.apac_consmr$Monthly.Sales)
acurracy_atoarima_sales.apac_consmr
mape_atoarima_sales.apac_consmr<-acurracy_atoarima_sales.apac_consmr[5]
mape_atoarima_sales.apac_consmr
#We Obtained the following results on the Sales forecasting accuracy of Auto ARIMA model:
#             ME        RMSE      MAE       MPE       MAPE
#Test set   15848.24  22755.75  18780.19  19.73091  27.68952
#Finally lets plot the Auto Arima forecast along with the Actual Sales for APAC_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Sales Time Series in Red and the Fitted Auto Arima Model in Blue.
full_atoarima_sales.apac_consmr<- ts(c(fitted(atoarima_sales.apac_consmr), fcast_atoarima_sales.apac_consmr$pred))
plot(ts(apac_consmr[,"Monthly.Sales"]), col="red", lwd=2)
lines(full_atoarima_sales.apac_consmr, col="blue", lwd=2)
#************************* [Iv].2. EU_CONSUMER *************************#
#EU_CONSUMER - European Union Consumer Market Bucket
#We will create seperate dataframe frames for Demand and Sales. 
#These dataframes will be used to build the time series forecasting model for EU_Consumer Monthly Demand and Monthly Sales respectively.
demand.eu_consmr<- eu_consmr[,c("Month.Code","Monthly.Demand")]
sales.eu_consmr<- eu_consmr[,c("Month.Code","Monthly.Sales")]
row_count<- nrow(eu_consmr)
#***** EU_CONSUMER Demand Modeling Section *****#
#We will build two demand forecasting models using the below techniques:
#[1] Classical Decomposition
#[2] Auto ARIMA
#We will then evaluate the two models based on MAPE metric for Forecasts on 6 month out of bag values.
#Therefore, we need to divide the demand.eu_consmr dataframe into 42 rows for model building and 6 rows for testing.
test.demand.eu_consmr<- demand.eu_consmr[(row_count-5):row_count,]
demand.eu_consmr<- demand.eu_consmr[1:(row_count-6),]
demand.eu_consmr.timeser<- ts(demand.eu_consmr[,"Monthly.Demand"])
demand.eu_consmr.timeser
#Let us begin to understand the individual components of our Demand Time Series
#We will decompose the components and view it graphically. Since decomposition cannot happen under the frequency of 1.
#And we are viewing monthly data. It is most logical to look at it in a 12 month/1 year format. So we will set the frequency to 12. And obseve the components of the decomposed timeseries
decomposed.demand.eu_consmr<- ts(demand.eu_consmr[,"Monthly.Demand"], frequency = 12)
decomposed.demand.eu_consmr<- decompose(decomposed.demand.eu_consmr)
decomposed.demand.eu_consmr
plot(decomposed.demand.eu_consmr)
#It is clear from the decomposition that the Demand:
#[1] A linearly increasing trend with a positive slope
#[2] A low amplitude sinusoidal seasonality ranging between -150 to +200 units.
#[3] The Demand Time Series will be and Additive Forecasting Model
#We will now model these components.
#Plotting the Demand Time Series
plot(demand.eu_consmr.timeser, col="red", lwd=2)
#Classical Decomposition model for EU Consumer Demand
#Smoothening the demand for EU_Consumer Demand using the aforementioned ts_movavg_smoother(input_timeseries, width)
smothed_demand.eu_consmr<- ts_movavg_smoother(demand.eu_consmr.timeser, 1)
lines(smothed_demand.eu_consmr, col="blue", lwd=2)
smothed_demand.eu_consmr<- cbind(demand.eu_consmr$Month.Code,smothed_demand.eu_consmr)
smothed_demand.eu_consmr<- data.frame(smothed_demand.eu_consmr)
colnames(smothed_demand.eu_consmr)<- c("Month.Code","Monthly.Demand")
#With the smoothed demand dataframe. Let us begin modeling the global trend aspect
lmfit_demand.eu_consmr <- lm(Monthly.Demand~ sin(0.5*Month.Code)*poly(Month.Code,1)+
                               cos(0.09*Month.Code)*poly(Month.Code,1)+
                               tan(0.02*Month.Code), data = smothed_demand.eu_consmr)
lmfit_demand.eu_consmr
globcomp_demand.eu_consmr <- predict(lmfit_demand.eu_consmr, data=smothed_demand.eu_consmr$Month.Code)
#We will now plot the global_component globcomp_demand.eu_consmr over the smoothed Demand Time Series.
#Note the Graph Shows: Monthly Demand Time series in Red, Smoothed Monthly Demand in Blue and the Forecasted Global Component in Green
lines(globcomp_demand.eu_consmr, col="green", lwd=2)
accuracy(globcomp_demand.eu_consmr, smothed_demand.eu_consmr$Monthly.Demand)
#Now that we have modeled the Trend and Seasonality let us isolate the Local Component and analyze its ARMA components
loclcomp_demand.eu_consmr<- demand.eu_consmr.timeser - globcomp_demand.eu_consmr
#Now let's plot this local component and analyze it graphically
plot(loclcomp_demand.eu_consmr, col="red")
#Testing acf and pacf plots for this local component to check for weak stationarty
acf(loclcomp_demand.eu_consmr)
pacf(loclcomp_demand.eu_consmr)
#Let us check if this local component can be modeled as AR(p) or MA(q) series.
locfitarma_demand.eu_consmr<- auto.arima(loclcomp_demand.eu_consmr)
locfitarma_demand.eu_consmr
#ARIMA(0,0,0) with zero mean; This implies that there is no AR(p) or MA(q) series in the local component
#sigma^2 estimated as 15119  |  log likelihood=-261.69  |  AIC=525.39  |  AICc=525.49  |  BIC=527.13
#We will test the residual for white noise
resi_clasdec_demand.eu_consmr<- loclcomp_demand.eu_consmr - fitted(locfitarma_demand.eu_consmr)
#Plotting the Residual
plot(resi_clasdec_demand.eu_consmr, col="red")
#Testing the residual for White Noise
adf.test(resi_clasdec_demand.eu_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.02453. 
#This Implies that the residual after extraxting the global and local components for demand is stationairy.
kpss.test(resi_clasdec_demand.eu_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for demand is stationairy.
#Let us forecast the Demand using classical decomposition model for next 6 months [Month Code: 43,44,45,46,47,48].
fcast_clasdec_demand.eu_consmr<- predict.lm(lmfit_demand.eu_consmr, data.frame(Month.Code=test.demand.eu_consmr$Month.Code))
fcast_clasdec_demand.eu_consmr
#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.eu_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
acurracy_clasdec_demand.eu_consmr<- accuracy(fcast_clasdec_demand.eu_consmr, test.demand.eu_consmr$Monthly.Demand)
acurracy_clasdec_demand.eu_consmr
mape_clasdec_demand.eu_consmr<-acurracy_clasdec_demand.eu_consmr[5]
mape_clasdec_demand.eu_consmr
#We Obtained the following results on the Demand forecasting accuracy of Classical Decomposition model:
#            ME        RMSE      MAE       MPE       MAPE
#Test set -19.25783  189.3226  127.333  -10.38272  21.98432
#Finally lets plot the Classical Decomposition Demand forecast along with the Actual Demand for EU_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Classical Decomposition Demand in Blue.
full_clasdec_demand.eu_consmr<- ts(c(globcomp_demand.eu_consmr, fcast_clasdec_demand.eu_consmr))
plot(ts(eu_consmr[,"Monthly.Demand"]), col="red", lwd=2)
lines(full_clasdec_demand.eu_consmr, col="blue", lwd=2)
#[2] Trying the Auto Arima Model 
atoarima_demand.eu_consmr<- auto.arima(demand.eu_consmr.timeser)
atoarima_demand.eu_consmr
#The auto arima model was of ARIMA(2,1,0) ; This implies that 1 stage differencing was performed.
#Following which it was modeled as an AR(2) model.
#The coefficients of the AR(2) model have a standard error of 16.16% and 20.2% of their respective coefficients. These values are relatively high but we will accept it with caution.
#sigma^2 estimated as 21185  |  log likelihood=-261.9  |  AIC=529.8  |  AICc=530.44  |  BIC=534.94
tsdiag(atoarima_demand.eu_consmr)
#We will now plot the Original Demand Time Series in Red and the Fitted Auto Arima Model in Blue.
plot(atoarima_demand.eu_consmr$x, col="red", lwd=2)
lines(fitted(atoarima_demand.eu_consmr), col="blue", lwd=2)
#The Demand Auto Arima Model shows a slight lag/offeset to the right in response to fluctuations in the Actual Demand
#Let us test the residual values after the removing the fitted auto arima model for Demand.
#Ideally these residual values must represent white noise.
resi_atoarima_demand.eu_consmr <- demand.eu_consmr.timeser - fitted(atoarima_demand.eu_consmr)
adf.test(resi_atoarima_demand.eu_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.04521. 
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.
kpss.test(resi_atoarima_demand.eu_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.
#Let us forecast the Demand using for atoarima_demand.eu_consmr next 6 months [Month Code: 43,44,45,46,47,48].
fcast_atoarima_demand.eu_consmr<- predict(atoarima_demand.eu_consmr, n.ahead = 6)
fcast_atoarima_demand.eu_consmr
#We will compare the aforementioned forecasted values with Actual Demand from the test.demand.eu_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Demand vs Actual Monthly Demand of [Month Code: 43,44,45,46,47,48] using MAPE
acurcy_atoarima_demand.eu_consmr<- accuracy(fcast_atoarima_demand.eu_consmr$pred, test.demand.eu_consmr$Monthly.Demand)
acurcy_atoarima_demand.eu_consmr
mape_atoarima_demand.eu_consmr<-acurcy_atoarima_demand.eu_consmr[5]
mape_atoarima_demand.eu_consmr
#We Obtained the following results on the Demand forecasting accuracy of Auto ARIMA model:
#            ME       RMSE     MAE       MPE       MAPE
#Test set 242.746  316.7626  253.8108  27.53891  30.13319
#Finally lets plot the Auto Arima forecast along with the Actual Demand for EU_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Auto Arima Model in Blue.
full_atoarima_demand.eu_consmr<- ts(c(fitted(atoarima_demand.eu_consmr), fcast_atoarima_demand.eu_consmr$pred))
plot(ts(eu_consmr[,"Monthly.Demand"]), col="red", lwd=2)
lines(full_atoarima_demand.eu_consmr, col="blue", lwd=2)
#***** EU_CONSUMER Sales Modeling Section *****#
#We will build two sales forecasting models using the below techniques:
#[1] Classical Decomposition
#[2] Auto ARIMA
#We will then evaluate the two models based on MAPE metric for Forecasts on 6 month out of bag values.
#Therefore, we need to divide the sales.eu_consmr dataframe into 42 rows for model building and 6 rows for testing.
test.sales.eu_consmr<- sales.eu_consmr[(row_count-5):row_count,]
sales.eu_consmr<- sales.eu_consmr[1:(row_count-6),]
sales.eu_consmr.timeser<- ts(sales.eu_consmr[,"Monthly.Sales"])
sales.eu_consmr.timeser
#Let us begin to understand the individual components of our Sales Time Series
#We will decompose the components and view it graphically. Since decomposition cannot happen under the frequency of 1.
#And we are viewing monthly data. It is most logical to look at it in a 12 month/1 year format. So we will set the frequency to 12. And obseve the components of the decomposed timeseries
decomposed.sales.eu_consmr<- ts(sales.eu_consmr[,"Monthly.Sales"], frequency = 12)
decomposed.sales.eu_consmr<- decompose(decomposed.sales.eu_consmr)
decomposed.sales.eu_consmr
plot(decomposed.sales.eu_consmr)
#It is clear from the decomposition that the Demand:
#[1] A linearly increasing trend with a positive slope
#[2] A high amplitude sinusoidal seasonality ranging between -15000 to +10000 units.
#[3] The Sales Time Series will be and Additive Forecasting Model
#We will now model these components.
#Plotting the Sales Time Series
plot(sales.eu_consmr.timeser, col="red", lwd=2)
#Classical Decomposition model for EU Consumer Sales
#Smoothening the monthly sales for EU_Consumer Sales using the aforementioned ts_movavg_smoother(input_timeseries, width)
smothed_sales.eu_consmr<- ts_movavg_smoother(sales.eu_consmr.timeser, 1)
lines(smothed_sales.eu_consmr, col="blue", lwd=2)
smothed_sales.eu_consmr<- cbind(sales.eu_consmr$Month.Code,smothed_sales.eu_consmr)
smothed_sales.eu_consmr<- data.frame(smothed_sales.eu_consmr)
colnames(smothed_sales.eu_consmr)<- c("Month.Code","Monthly.Sales")
#With the smoothed sales dataframe. Let us begin modeling the global trend aspect
lmfit_sales.eu_consmr <- lm(Monthly.Sales~ sin(0.4*Month.Code)*poly(Month.Code,1)+
                              cos(0.09*Month.Code)*poly(Month.Code,1), data = smothed_sales.eu_consmr)
lmfit_sales.eu_consmr
#Checking global Component
globcomp_sales.eu_consmr <- predict(lmfit_sales.eu_consmr, data=smothed_sales.eu_consmr$Month.Code)
#We will now plot the global_component globcomp_sales.eu_consmr over the smoothed Sales Time Series.
#Note the Graph Shows: Monthly Sales Time series in Red, Smoothed Monthly Sales in Blue and the Forecasted Global Component in Green
lines(globcomp_sales.eu_consmr, col="green", lwd=2)
accuracy(globcomp_sales.eu_consmr, smothed_sales.eu_consmr$Monthly.Sales)
#Now that we have modeled the Trend and Seasonality let us isolate the Local Component and analyze its ARMA components
loclcomp_sales.eu_consmr<- sales.eu_consmr.timeser - globcomp_sales.eu_consmr
#Now let's plot this local component and analyze it graphically
plot(loclcomp_sales.eu_consmr, col="red")
#Testing acf and pacf plots for this local component to check for weak stationarity
acf(loclcomp_sales.eu_consmr)
pacf(loclcomp_sales.eu_consmr)
#Let us check if this local component can be modeled as AR(p) or MA(q) series.
locfitarma_sales.eu_consmr<- auto.arima(loclcomp_sales.eu_consmr)
locfitarma_sales.eu_consmr
#ARIMA(0,0,0) with zero mean ; This implies that there is no AR(p) or MA(q) series in the local component
#We will test the residual for white noise
resi_clasdec_sales.eu_consmr<- loclcomp_sales.eu_consmr - fitted(locfitarma_sales.eu_consmr)
#Plotting the Residual
plot(resi_clasdec_sales.eu_consmr, col="red")
#Testing the residual for White Noise
adf.test(resi_clasdec_sales.eu_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.01311. 
#This Implies that the residual after extraxting the global and local components for sales is stationairy.
kpss.test(resi_clasdec_sales.eu_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for sales is stationairy.
#Let us forecast the Sales using classical decomposition model for next 6 months [Month Code: 43,44,45,46,47,48].
fcast_clasdec_sales.eu_consmr<- predict.lm(lmfit_sales.eu_consmr, data.frame(Month.Code=test.sales.eu_consmr$Month.Code))
fcast_clasdec_sales.eu_consmr
#We will compare the aforementioned forecasted values with Actual Sales from the test.sales.eu_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Sales vs Actual Monthly Sales of [Month Code: 43,44,45,46,47,48] using MAPE
acurracy_clasdec_sales.eu_consmr<- accuracy(fcast_clasdec_sales.eu_consmr, test.sales.eu_consmr$Monthly.Sales)
acurracy_clasdec_sales.eu_consmr
mape_clasdec_sales.eu_consmr<-acurracy_clasdec_sales.eu_consmr[5]
mape_clasdec_sales.eu_consmr
#We Obtained the following results on the Sales forecasting accuracy of Classical Decomposition model:
#             ME       RMSE      MAE        MPE       MAPE
#Test set 4172.431  13736.59  10622.04   2.259534   20.73958
#Finally lets plot the Classical Decomposition Sales forecast along with the Actual Sales for EU_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Demand Time Series in Red and the Fitted Classical Decomposition Demand in Blue.
full_clasdec_sales.eu_consmr<- ts(c(globcomp_sales.eu_consmr, fcast_clasdec_sales.eu_consmr))
plot(ts(eu_consmr[,"Monthly.Sales"]), col="red", lwd=2)
lines(full_clasdec_sales.eu_consmr, col="blue", lwd=2)
#[2] Trying the Auto Arima Model 
atoarima_sales.eu_consmr<- auto.arima(sales.eu_consmr.timeser)
atoarima_sales.eu_consmr
#The auto arima model was of ARIMA(2,1,0)  
#This implies that 1 stage differencing was performed and the resulting timeseries was modeled as AR(2).
#The standard error for the AR(2) coefficient is 23.2% and 26.7% of the coefficient. This is relatively high but we will accept it with caution 
#sigma^2 estimated as 168564657  |  log likelihood=-445.84  |  AIC=897.67  |  AICc=898.32  |  BIC=902.81
tsdiag(atoarima_sales.eu_consmr)
#We will now plot the Original Sales Time Series in Red and the Fitted Auto Arima Model in Blue.
plot(atoarima_sales.eu_consmr$x, col="red", lwd=2)
lines(fitted(atoarima_sales.eu_consmr), col="blue", lwd=2)
#The Sales Auto Arima Model shows a distinct lag/offset to the right when overlaid on the Actual Sales.
#This implies that the sales forecasting model has a delayed response to fluctuations in the Actual Sales on both the high and low end.
#Let us test the residual values after the removing the fitted auto arima model for Sales.
#Ideally these residual values must represent white noise.
resi_atoarima_sales.eu_consmr <- sales.eu_consmr.timeser - fitted(atoarima_sales.eu_consmr)
adf.test(resi_atoarima_sales.eu_consmr, alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the fitted auto arima for sales is stationairy.
kpss.test(resi_atoarima_sales.eu_consmr)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for sales is stationairy.
#Let us forecast the Sales using for atoarima_sales.eu_consmr next 6 months [Month Code: 43,44,45,46,47,48].
fcast_atoarima_sales.eu_consmr<- predict(atoarima_sales.eu_consmr, n.ahead = 6)
fcast_atoarima_sales.eu_consmr
#We will compare the aforementioned forecasted values with Actual sales from the test.sales.eu_consmr dataframe.
#We will evaluate MAPE for Forecasted Monthly Sales vs Actual Monthly Sales of [Month Code: 43,44,45,46,47,48] using MAPE
acurracy_atoarima_sales.eu_consmr<- accuracy(fcast_atoarima_sales.eu_consmr$pred, test.sales.eu_consmr$Monthly.Sales)
acurracy_atoarima_sales.eu_consmr
mape_atoarima_sales.eu_consmr<-acurracy_atoarima_sales.eu_consmr[5]
mape_atoarima_sales.eu_consmr
#We Obtained the following results on the Sales forecasting accuracy of Auto ARIMA model:
#             ME      RMSE      MAE       MPE       MAPE
#Test set  12935.2  19499.13   16687.6   17.678   28.9226
#Finally lets plot the Auto Arima forecast along with the Actual Sales for EU_Consumer Bucket to get an understanding of the entire model fit.
#We will now plot the Original Sales Time Series in Red and the Fitted Auto Arima Model in Blue.
full_atoarima_sales.eu_consmr<- ts(c(fitted(atoarima_sales.eu_consmr), fcast_atoarima_sales.eu_consmr$pred))
plot(ts(eu_consmr[,"Monthly.Sales"]), col="red", lwd=2)
lines(full_atoarima_sales.eu_consmr, col="blue", lwd=2)
#***************** [VI] Model Evaluation[MAPE], Model Selection, Forecasting Demand and Sales for the future 6 month Period. **********************#
#We have created the Classical Decomposition and Auto ARIMA models to forecast Demand and Sales of our most Profitable Market Buckets
#We will compare the model evaluations and select the best model based on the Accuracy and MAPE parameters of out of bag test values
#The testdata set contained month codes 43-48. Individual Model Performance is as Shown Below.
future_period<- c(49:54)
future_month<- c("Jan2015", "Feb2015", "Mar2015", "Apr2015", "May2015", "June2015")
#**************** APAC - Asia Pacific/Asia Central-Consumer[Market Buckets] ****************#
#[1]. APAC-CONSUMER MARKET BUCKET [DEMAND]
#A. Classical Decomposition Model Evaluation Result
acurcy_clasdec_demand.apac_consmr
mape_clasdec_demand.apac_consmr
#             ME       RMSE      MAE        MPE       MAPE
#Test set -23.16084  127.4945  107.1754  -8.815864   18.79196
#MAPE=18.79%
#B. Auto ARIMA Model Evaluation Results
acurcy_atoarima_demand.apac_consmr
mape_atoarima_demand.apac_consmr
#         ME   RMSE     MAE       MPE     MAPE
#Test set 12 174.3722 147.6667 -7.362467 26.24458
#MAPE=26.24%
#APAC-CONSUMER MARKET BUCKET [DEMAND] Forecasting Model Evaluation Report
#From the above results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 7.45% on the Auto Arima model. 
#Therefore the Classical Decomposition Model will be used to forecast the demand for the future 6 month Period.
futur_fcast_demand.apac_consmr<- predict.lm(lmfit_demand.apac_consmr, data.frame(Month.Code=future_period))
futur_fcast_demand.apac_consmr<- as.vector(futur_fcast_demand.apac_consmr)
futur_fcast_demand.apac_consmr<- data.frame(future_month, futur_fcast_demand.apac_consmr)
colnames(futur_fcast_demand.apac_consmr)<- c("Future Month", "Demand Forecast")
#APAC Consumer Forecasted Demand is As follows
futur_fcast_demand.apac_consmr
#[2]. APAC-CONSUMER MARKET BUCKET [SALES]
#A. Classical Decomposition Model Evaluation Result
acurcy_clasdec_sales.apac_consmr
mape_clasdec_sales.apac_consmr
#             ME       RMSE      MAE        MPE       MAPE
#Test set  6515.587  14997.64  12937.14   5.024684  20.83351
#MAPE=20.83%
#B. Auto ARIMA Model Evaluation Results
acurracy_atoarima_sales.apac_consmr
mape_atoarima_sales.apac_consmr
#             ME        RMSE      MAE       MPE       MAPE
#Test set   15848.24  22755.75  18780.19  19.73091  27.68952
#MAPE=27.68%
#APAC-CONSUMER MARKET BUCKET [SALES] Forecasting Model Evaluation Report
#From the above results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 6.86% on the Auto Arima model. 
#Therefore the Classical Decomposition Model will be used to forecast the demand for the future 6 month Period.
futur_fcast_sales.apac_consmr<- predict.lm(lmfit_sales.apac_consmr, data.frame(Month.Code=future_period))
futur_fcast_sales.apac_consmr<- as.vector(futur_fcast_sales.apac_consmr)
futur_fcast_sales.apac_consmr<- data.frame(future_month, futur_fcast_sales.apac_consmr)
colnames(futur_fcast_sales.apac_consmr)<- c("Future Month", "Sales Forecast")
#APAC Consumer Forecasted Sales is As follows
futur_fcast_sales.apac_consmr
#**************** EU - European Union-Consumer [Market Bucket] ****************#
#[3]. EU-CONSUMER MARKET BUCKET [DEMAND]
#A. Classical Decomposition Model Evaluation Result
acurracy_clasdec_demand.eu_consmr
mape_clasdec_demand.eu_consmr
#            ME        RMSE      MAE       MPE       MAPE
#Test set -19.25783  189.3226  127.333  -10.38272  21.98432
#MAPE=21.98%
#B. Auto ARIMA Model Evaluation Results
acurcy_atoarima_demand.eu_consmr
mape_atoarima_demand.eu_consmr
#            ME       RMSE     MAE       MPE       MAPE
#Test set 242.746  316.7626  253.8108  27.53891  30.13319
#MAPE=30.13%
#EU-CONSUMER MARKET BUCKET [DEMAND] Forecasting Model Evaluation Report
#From the above results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 8.15% on the Auto Arima model. 
#Therefore the Classical Decomposition Model will be used to forecast the demand for the future 6 month Period
futur_fcast_demand.eu_consmr<- predict.lm(lmfit_demand.eu_consmr, data.frame(Month.Code=future_period))
futur_fcast_demand.eu_consmr<- as.vector(futur_fcast_demand.eu_consmr)
futur_fcast_demand.eu_consmr<- data.frame(future_month, futur_fcast_demand.eu_consmr)
colnames(futur_fcast_demand.eu_consmr)<- c("Future Month", "Demand Forecast")
#EU Consumer Forecasted Demand is As follows
futur_fcast_demand.eu_consmr
#[2]. EU-CONSUMER MARKET BUCKET [SALES]
#A. Classical Decomposition Model Evaluation Result
acurracy_clasdec_sales.eu_consmr
mape_clasdec_sales.eu_consmr
#             ME       RMSE      MAE        MPE       MAPE
#Test set 4172.431  13736.59  10622.04   2.259534   20.73958
#MAPE=20.73%
#B. Auto ARIMA Model Evaluation Results
acurracy_atoarima_sales.eu_consmr
mape_atoarima_sales.eu_consmr
#             ME      RMSE      MAE       MPE       MAPE
#Test set  12935.2  19499.13   16687.6   17.678   28.9226
#MAPE=28.92%
#EU-CONSUMER MARKET BUCKET [SALES] Forecasting Model Evaluation Report
#From the above results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 8.19% on the Auto Arima model. 
#Therefore the Classical Decomposition Model will be used to forecast the demand for the future 6 month Period.
futur_fcast_sales.eu_consmr<- predict.lm(lmfit_sales.eu_consmr, data.frame(Month.Code=future_period))
futur_fcast_sales.eu_consmr<- as.vector(futur_fcast_sales.eu_consmr)
futur_fcast_sales.eu_consmr<- data.frame(future_month, futur_fcast_sales.eu_consmr)
colnames(futur_fcast_sales.eu_consmr)<- c("Future Month", "Sales Forecast")
#APAC Consumer Forecasted Sales is As follows
futur_fcast_sales.eu_consmr
#End Notes **********************************#
#Thus We have sucessfully forecasted the Demand and Sales Forecasting Model for the target market buckets:
#[1] APAC-CONSUMER
#[2] EU-CONSUMER
