
#importing the required libraries
install.packages('tidyverse')
install.packages("rlang", version = "1.1.0")
library(tidyverse)

#importing the required datasets

df_06<-read.csv('C:/Users/Ibrahim Hameem/Pictures/Ahmed Coursework/R Courswork/2006.csv')
df_07<-read.csv('C:/Users/Ibrahim Hameem/Pictures/Ahmed Coursework/R Courswork/2007.csv')

# combine both datasets by using rbind , we concatenate the datasets by combining the rows the of both datasets
df<- rbind(df_06,df_07)

# explore df
head(df)
dim(df)

# check df for null values
colSums(is.na(df))  # we can see multiple columns with null values

# checking the dataset for outliers 
max(df$DepTime,na.rm=TRUE) # the max of 2930 does not make sense in a datetime point of view as 2359 is the max for any day would be 2359, rows exceeding 2359 must be removed
min(df$DepTime,na.rm=TRUE)


max(df$ArrTime,na.rm=TRUE) # the max of 2955 does not make sense in a datetime point of view as 2359 is the max any day, rows exceeding 2359 must be removed
min(df$ArrTime,na.rm=TRUE)

max(df$Month)
min(df$Month)

max(df$DayofMonth)
min(df$DayofMonth)


max(df$DayOfWeek)
min(df$DayOfWeek)

max(df$CRSArrTime,na.rm=TRUE) 
min(df$CRSArrTime,na.rm=TRUE)


max(df$CRSDepTime,na.rm=TRUE) 
min(df$CRSDepTime,na.rm=TRUE)
 # ArrTime and DepTime have outliers that have to be removed 
# the rows which have values >2359 have to be removed

df<-filter(df,df$DepTime<=2359)
df<-filter(df,df$ArrTime<=2359)
max(df$DepTime)
max(df$ArrTime)  # we can see all outliers have been removed

head(df) # explore the dataset again # we can see the column 'CancellationCode' is almost empty and therefore it is better to drop this column
df$CancellationCode<-NULL #98% of the entries in "CancellationCode" is missing therefore it is better to drop the column than removing the rows with null values as this will result in a massive loss of data
head(df) # check the dataset again # we can see 'CancellationCode' has been removed

# check df for null values again
colSums(is.na(df)) # we see no null values in the dataset as it has been cleaned
# checking the dimension of the dataset again
dim(df)

install.packages('readr')
library(readr)
write.csv(df,'C://Users//Ibrahim Hameem//Pictures//Ahmed Coursework//data_clean.csv',row.names = FALSE)
