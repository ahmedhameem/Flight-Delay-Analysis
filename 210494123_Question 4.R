
#loading the required dataset
df<-read.csv('C://Users//Ibrahim Hameem//Pictures//Ahmed Coursework//data_clean.csv')

#importing the required library
library(lubridate)
install.packages('dplyr')
library(dplyr)
library(tidyverse)

#selecting the required columns for analysis
df<-select(df,Year,Month,DayofMonth,DayOfWeek,DepTime,TailNum,ArrDelay,DepDelay,Origin,LateAircraftDelay,Dest)
head(df) #explore dataframe

#check df for null values
colSums(is.na(df))


      # Can you detect cascading failures as delays in one airport create delays in others ? #

# we start our analysis by  the Year,Month,DayofMonth,DepTime into datetime format in a new column 'datetime'


#capturing the hours from "DepTime'
hour<-floor(df$DepTime/100)
hour<as.integer(hour)

#capturing the minutes from 'DepTime' 
cap_mins<-function(y){
  substr(y,nchar(y)-1,nchar(y))
}

minutes<-cap_mins(df$DepTime)
minutes<-as.integer(minutes)

# we now use the mutate function to convert to datetime
df<-mutate(df,datetime=make_datetime(as.integer(Year),as.integer(Month),as.integer(DayofMonth),hour,minutes))
head(df) # explore dataset


# creating a new column 'Average Delay' with the sum of mean 'ArrDelay' and 'DepDelay' divided by 2 for further analysis
agg_df_dep<-aggregate(DepDelay~TailNum+datetime,data=df,FUN=mean)
agg_df_dep

agg_df_arr<-aggregate(ArrDelay~TailNum+datetime,data=df,FUN=mean)
agg_df_arr

df1<-merge(agg_df_dep,agg_df_arr,by.x = c('TailNum','datetime'),by.y = c('TailNum','datetime'))
df1
df1$Average_Delay<-(df1$ArrDelay+df1$DepDelay)/2
df1 # explore dataframe


#now we sort df1 by 'datetime' and 'TailNum'
df1<-arrange(df1,TailNum,datetime)
head(df1)


df1<-df1%>%
  mutate(upcoming_delay=lead(Average_Delay))
df1

#check df1 for null values
colSums(is.na(df1))  # we see a null value in the 'upcoming_delay' which can be removed
df1<-na.omit(df1)
colSums(is.na(df1)) # null value removed

#scatter plot to show the relationship between the delay faced at present and upcoming delay
# note that the 'Average_Delay' column is to be considered as delay faced at present

plot(df1$Average_Delay,df1$upcoming_delay,main='Cascading Delay Analysis',
     xlab='Present Delay',ylab='Upcoming Delay')
abline(lm(df1$Average_Delay~df1$upcoming_delay,data=df1),col='darkblue')
cor(df1$Average_Delay,df1$upcoming_delay)  # we see a postive linear relationship between delays faced at present and upcoming delays

# to explore whether cascading delays in one airport create delays in others, a crosstabulation would allow
#to analyse the relationship between delays faced in the previous airport that was departed and the delay faced in the current aiport on arrival
#since crosstabs work only for categorical variables ,the delay in the current and previous airport was changed
#where 1s indicate a delay and 0s indicate no delay


#encoding columns 'Average_Delay' and 'upcoming_delay' into new columns 'facing_current_delay' and 'facing_upcoming_delay'
df1$facing_current_delay<-ifelse(df1$Average_Delay>0,1,0)
df1$facing_upcoming_delay<-ifelse(df1$upcoming_delay>0,1,0)
head(df1) #explore dataframe

#crosstabulation
table<-table(df1$facing_current_delay,df1$facing_upcoming_delay,dnn=c('current delay','upcoming delay'))
table  #explore table of observed values

##calculations show that 64.1%  of flights that had a delay at its present airport had a delay at its next destination airport (4263298/4263298+2391893) and 31.4%  of flights that faced a delay at its present airport would be able to reach its destination airport with no delay.
# we can carry out a chi_squared hypothesis test to check if a linear association does exist between 'facing_current_delay' and 'facing_upcoming_delay'
#let us first set our hypotheses
print('H0:There is no association between facing_current_delay and facing_upcoming_delay')
print('H1:There is an association between facing_current_delay and facing_upcoming_delay')

#hypothesis test
chisq.test(table,correct = FALSE)
#we have chi squared test statistic = 1 520 473
# degrees of freedom = 1
#p value is almost zero
# therefore since our p value is less than siginificance level 5% and siginificance level 1% we can conclude there is strong evidence to claim there is a linear association between 'facing_current_delay' and 'facing_upcoming_delay'