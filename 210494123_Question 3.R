
#uploading the required datasets
df_2006<-read.csv('C:/Users/Ibrahim Hameem/Pictures/Ahmed Coursework/R Courswork/2006.csv')
df_2007<- read.csv('C:/Users/Ibrahim Hameem/Pictures/Ahmed Coursework/R Courswork/2007.csv')             

#selecting the required columns from each dataframe
df_2006<-select(df_2006,'Origin','Dest','Year')
df_2007<-select(df_2007,'Origin','Dest','Year')

#explore dataframes
df_2006
df_2007


# grouping the dataframe df_2006 by origin and dest and obtaining the count for each groupby
df_2006<-df_2006%>%
  group_by(Origin,Dest)%>%
  summarise(year2006=n())
  

df_2006 # explore dataset
df_2006<-df_2006%>%arrange(desc(year2006))# arranging the column year2006 by descending order
head(df_2006) # explore dataframe

# grouping the dataframe df_2007 by origin and dest and obtaining the count for each groupby
df_2007<-df_2007%>%
  group_by(Origin,Dest)%>%
  summarise(year2007=n())
df_2007 # explore dataset
df_2007<-df_2007%>%arrange(desc(year2007))  # arranging the column year2007 by descending order
head(df_2007)  #explore dataframe





# now let us merge df_2006 and df_2007 by 'Origin' and 'Dest'
df_travel<-merge(df_2006,df_2007,by=c('Origin','Dest'))
df_travel # explore dataframe
  # arrange columns in descending order
df_travel<-df_travel%>%arrange(desc(year2006),desc(year2007))
df_travel # explore column


# check df_travel for null values
is.na(df_travel)
df_travel[is.na(df_travel)]=0 #replacing any Nan values with 0

#creating a new column called 'Total FLight Count'
df_travel$Total_Flight_Count<-df_travel$year2006 + df_travel$year2007
df_travel<-df_travel%>%arrange(desc(Total_Flight_Count))  # arranging dataframe by desceding order of flights

df_travel # explore column
# creating a new dataframe with the top 10 locations according to total flight count
df_10<-df_travel[1:10,]
df_10 #explore dataset
  
# now we set out criteria for 'Origin' and 'Dest' to classify them into a new column called 'Route'
df_10$Route<-NULL

df_10$Route[df_10$Origin=='OGG' & df_10$Dest=='HNL']<-'OGG & HNL'
df_10$Route[df_10$Origin=='HNL' & df_10$Dest=='OGG']<-'HNL & OGG'
df_10$Route[df_10$Origin=='LAX' & df_10$Dest=='LAS']<-'LAX & LAS'
df_10$Route[df_10$Origin=='SAN' & df_10$Dest=='LAX']<-'SAN & LAX'
df_10$Route[df_10$Origin=='LAX' & df_10$Dest=='SAN']<-'LAX & SAN'
df_10$Route[df_10$Origin=='LAS' & df_10$Dest=='LAX']<-'LAS & LAX'
df_10$Route[df_10$Origin=='BOS' & df_10$Dest=='LGA']<-'BOS & LGA'
df_10$Route[df_10$Origin=='LIH' & df_10$Dest=='HNL']<-'LIH & HNL'
df_10$Route[df_10$Origin=='LGA' & df_10$Dest=='BOS']<-'LGA & BOS'
df_10$Route[df_10$Origin=='HNL' & df_10$Dest=='LIH']<-'HNL & LIH'

df_10

#creating a two dataframes using df_10 and stacking them on top of each other in order to create a barplot
d06<-select(df_10,Origin,Dest,year2006,Route)
d07<-select(df_10,Origin,Dest,year2007,Route)
#explore dataframes
d06
d07

#defining new column 'year' and filling each dataset with the respective year times
d06$year<-c(rep('2006',10))
d07$year<-c(rep('2007',10))
names(d06)[3]<-'Frequency'
names(d07)[3]<-'Frequency'
d06
d07
#combining both datasets by row
df_T<-rbind(d06,d07)
df_T

ggplot(df_T, aes(fill=year, y=Frequency, x=Route)) + 
  geom_bar(position="dodge", stat="identity",width =0.7)+
  theme(axis.text.x = element_text(color = "black", angle = 90, hjust = 1))
#grouped bar chart

  # analysing the change in flight counts over the years 2006 and 2007#
#uploading the required dataset
df<-read.csv('C:/Users/Ibrahim Hameem/Pictures/Ahmed Coursework/data_clean.csv')
airports_df<-read.csv('C:/Users/Ibrahim Hameem/Pictures/Ahmed Coursework/R Courswork/airports.csv')
 #explore datasets
head(df)
head(airports_df)


#iata in airports_df dataset refers to origin iata airport code and therefore we can rename that column as 'Origin'
names(airports_df)[1]<-'Origin'
#creating a new datafra,e with the required columns from airports_df and calling it airports_df
airports_df<-select(airports_df,Origin,state)


#creating a new dataframe with the required columns for analysis from df and calling it df
df<-select(df,Year,Month,Origin,Dest)

#checking the info in the dataframes
summary(df)
summary(airports_df)
  # we now merge boths airports_df and df on 'Origin'
df_merge<-merge(airports_df,df,by='Origin')
head(df_merge) # explore dataframe

# look for null values in the dataframe
colSums(is.na(df_merge))

# we can see that 'state' has null values and these need to be dropped
df_merge<-na.omit(df_merge)
colSums(is.na(df_merge))  #null values removed
  # grouping df_merge by 'Year' & 'Month'and getting the count of rows and creating a new dataframe called df_group
df_group<-df_merge%>%
  group_by(Year,Month)%>%
  summarise(Flight_Count=n())
head(df_group)  # explore dataframe

class(df_group$Month) # ensuring elements in the column 'Month' are integers for further analysis
#identifying month's of the year into the specific quarter of the year
df_group$Quarter<-NULL
df_group$Quarter[df_group$Month<4]<-'Q1'
df_group$Quarter[df_group$Month>=4  & df_group$Month<7]<-'Q2'
df_group$Quarter[df_group$Month>=7  & df_group$Month<10]<-'Q3'
df_group$Quarter[df_group$Month>=10 ]<-'Q4'

df_group# explore dataframe


df006<-df_group[1:12,] # calling of rows by index and defining a variable 
df007<-df_group[13:24,]
df006
df007
df_together<-rbind(df006,df007)  # binding both dataframes by rows


# grouping Flight_Count by Quarter and Year and getting an aggregate sum
df_together<-aggregate(Flight_Count~Quarter+Year,data=df_together,FUN=sum)
df_together# exploring the dataframe

#lineplot to show the changes in total flight for each quarter of the year for 2006-2007
ggplot(df_together,aes(x=Quarter,y=Flight_Count,group=Year,color=Year))+
  geom_line()+
  ggtitle('Changes in flight count from 06-07')+ylab('Flight Count')



  