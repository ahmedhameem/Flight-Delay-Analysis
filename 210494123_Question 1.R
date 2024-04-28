 
#installing required packages
install.packages('dplyr')
install.packages('ggplot2')
install.packages('RColorBrewer')
#upload the required library
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
# we will be using the cleaned data 'df' for analysis
df<-read.csv('C://Users//Ibrahim Hameem//Pictures//Ahmed Coursework//data_clean.csv')





# What is the best time of day , day of week and time of year to fly in
# in order minimise delays?

#creating a new dataframe with the required columns for analysis
df1<-select(df,Year,Month,DayOfWeek,DepDelay,DepTime,ArrDelay)
head(df1)# exploring a few rows of df1
#filtering the ArrDelay and DepDelay 
# only flight delays > 0 are considered delays otherwise the flight would be early or on time
df1<-df1[df1$ArrDelay>0,]
df1<-df1[df1$DepDelay>0,]


# what is the best day if the week to fly , in order to minimise delays? #

agg_df_dep <- aggregate(df1$DepDelay, by=list(df1$DayOfWeek), FUN=mean) #creating a new dataframe by grouping the df1 by 'DayOfWeek' and getting the mean DepDelay for each day
agg_df_dep

agg_df_arr <- aggregate(df1$ArrDelay, by=list(df1$DayOfWeek), FUN=mean)#creating a new dataframe by grouping the df1 by 'DayOfWeek' and getting the mean ArrDelay for each day
agg_df_arr


agg_df<-merge(agg_df_dep,agg_df_arr,by='Group.1') # merging both datasets obtained above
agg_df

#renaming columns in agg_df
names(agg_df)[1]<-'DayOfWeek'
names(agg_df)[2]<-'DepDelay'
names(agg_df)[3]<-'ArrDelay'
agg_df #explore dataset

#creating a new column 'Average Delay' where we add the mean DepDelay and ArrDelay and divide it by two
agg_df$AverageDelay <- (agg_df$DepDelay + agg_df$ArrDelay)/2


agg_df #explore dataset 
# we can see day 6 has the lowest average delay and hence is the best day to fly


# the bar plot below will display the information found above

agg_df$DayOfWeek<-factor(agg_df$DayOfWeek)
ggplot(agg_df, aes(x = DayOfWeek, y = AverageDelay, fill = DayOfWeek)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Day of Week", y = "Average Delay (minutes)", title = "Average Flight Delays Throughout The Week") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# Therefore Saturday is the best day to fly to minimise delays


#what is the best time of the year to fly to minimise delays?#


#selecting the required columns to identify the best month of the year to fly
dt<-select(df,Year,Month,DayofMonth,DayOfWeek,DepDelay,ArrDelay)

# only flight delays > 0 are considered delays otherwise the flight would be early or on time
dt<-dt[dt$ArrDelay>0,]
dt<-dt[dt$DepDelay>0,]


agg_df1 <- aggregate(dt$DepDelay, by=list(dt$Month), FUN=mean) #creating a new dataframe by grouping the dt by 'Month' and getting the mean DepDelay for each day
agg_df1

agg_df2 <- aggregate(dt$ArrDelay, by=list(dt$Month), FUN=mean)#creating a new dataframe by grouping the dt by 'Month' and getting the mean ArrDelay for each day
agg_df2

agg_df12<-merge(agg_df1,agg_df2,by='Group.1') # # merging both datasets obtained above
agg_df12

# rename the columns of df_12
names(agg_df12)[1]<-'Month'
names(agg_df12)[2]<-'DepDelay'
names(agg_df12)[3]<-'ArrDelay'
agg_df12 #explore dataset

#creating a new column 'Total Average Delay' that is the sum of mean'DepDelay' and 'ArrDelay' divided by 2
agg_df12$TotalAverageDelay<-(agg_df12$DepDelay+agg_df12$ArrDelay)/2
# exploring dataset
agg_df12


#we can see that May has the lowest total average delay , therefore it is the best month to travel
# the barplot below will display the results above
agg_df12$Month<-factor(agg_df12$Month)
ggplot(agg_df12, aes(x = Month, y = TotalAverageDelay, fill = Month)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Month", y = "Average Delay (minutes)", title = "Flight Delay Each Month Of The Year") +
  theme_minimal() +coord_flip()+
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# analzsing the best time of year to fly quarter-wise
agg_df12$Quarter <- NA

agg_df12$Quarter[agg_df12$Month<=3]<-'Q1'
agg_df12$Quarter[agg_df12$Month>3 & agg_df12$Month<=6]<-'Q2'
agg_df12$Quarter[agg_df12$Month>6 & agg_df12$Month<=9]<-'Q3'
agg_df12$Quarter[agg_df12$Month>9 & agg_df12$Month<=12]<-'Q4'

agg_df12 # explore the data

# obtaining a new dataset by grouping agg_df12 by the 'Quarter' and obtaining the mean 'Total Average Delay' for each quarter
df_q <- aggregate(agg_df12$TotalAverageDelay, by=list(agg_df12$Quarter), FUN=mean)
df_q #explore the dataset
# rename columns in df_q
names(df_q)[1]<-'Quarter'
names(df_q)[2]<-'TotalAverageDelay'
df_q # explore dataset
# we can see Q1 has the lowest total average delay therefore is the best quarter of the year to travel
# the bar plot below will display the information found above
df_q$Quarter<-factor(df_q$Quarter)
ggplot(df_q, aes(x = Quarter, y = TotalAverageDelay, fill = Quarter)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Quarter", y = "Average Delay (minutes)", title = "Flight Delay For Each Quarter") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# What is the best time of the day to fly, in order to minimise delays?#
# filtering df for delays >0
df<-df[df$ArrDelay>0,]
df<-df[df$DepDelay>0,]
ds<-select(df,Year,Month,DayofMonth,DayOfWeek,DepTime,ArrDelay,DepDelay)
head(ds) # explore the dataset


# creating timeframes for the 24hrs
ds$Timeframe<-NA
ds$Timeframe[ds$DepTime<400]<-'12am-4am'
ds$Timeframe[ds$DepTime>=400 & ds$DepTime<800]<-'4am-8am'
ds$Timeframe[ds$DepTime>=800 & ds$DepTime<1200]<-'8am-12pm'
ds$Timeframe[ds$DepTime>=1200 & ds$DepTime<1600]<-'12pm-4pm'
ds$Timeframe[ds$DepTime>=1600 & ds$DepTime<2000]<-'4pm-8pm'
ds$Timeframe[ds$DepTime>=2000]<-'8pm-12am'
head(ds)



agg_df3 <- aggregate(ds$DepDelay, by=list(ds$Timeframe), FUN=mean) #creating a new dataframe by grouping the ds by 'Timeframe' and getting the mean DepDelay for each day
agg_df3

agg_df4 <- aggregate(ds$ArrDelay, by=list(ds$Timeframe), FUN=mean)#creating a new dataframe by grouping the ds by 'Timeframe' and getting the mean ArrDelay for each day
agg_df4

agg_df13<-merge(agg_df3,agg_df4,by='Group.1') # # merging both datasets obtained above
agg_df13

# rename the columns of df_12
names(agg_df13)[1]<-'Timeframe'
names(agg_df13)[2]<-'DepDelay'
names(agg_df13)[3]<-'ArrDelay'
agg_df13 #explore dataset


#create a new column that sums  both mean 'ArrDelay' and 'DepDelay' and divides by 2
agg_df13$TotalDelay<-(agg_df13$ArrDelay+agg_df13$DepDelay)/2
#we only consider delays >0 ,therefore
agg_df13<-agg_df13[agg_df13$TotalDelay>0,]
head(agg_df13) #explore columns of dataset


# the bar plot below will display the information found above
agg_df13$Timeframe<-factor(agg_df13$Timeframe)
ggplot(agg_df13, aes(x = Timeframe, y = TotalDelay, fill = Timeframe)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Timeframe", y = "Average Delay (minutes)", title = "Flight Delay For Each Timeframe") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

  # we can see that the 4am to 8am time frame has the lowest total average delay making it the best timeframe within a day to fly