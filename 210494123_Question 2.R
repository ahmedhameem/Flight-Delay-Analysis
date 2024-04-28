

# importing the required  data
df<-read.csv('C://Users//Ibrahim Hameem//Pictures//Ahmed Coursework//data_clean.csv')
df_airplane<-read.csv('C:/Users/Ibrahim Hameem/Pictures/Ahmed Coursework/plane-data.csv')
head(df) # exploring dataset
# filtering dataset df to remove delays < 0 , as these do not indicate delays but flighs that are early or on time
df<-df[df$ArrDelay>0,]
df<-df[df$DepDelay>0,]

# include all required columns in a new dataframe
dt<-select(df,Year,Month,DayofMonth,DayOfWeek,DepDelay,ArrDelay,DepTime,TailNum)
df_p<-select(df_airplane,tailnum,year)

#renaming columns in df_airplane , this will make it easier to merge both dt and df_p
names(df_p)[1]<-'TailNum'
names(df_p)[2]<-'Manufactured_year'

df_p  # explore dataset
df_merge<-merge(dt,df_p,by='TailNum')
df_merge # explore dataset

#create a dataframe with average delays grouped with respect to year of manufacture

Agg<-aggregate(df_merge$DepDelay,by=list(df_merge$Manufactured_year),FUN=mean)
Agg
Agg1<-aggregate(df_merge$ArrDelay,by=list(df_merge$Manufactured_year),FUN=mean)
Agg1

Agg_merge<-merge(Agg,Agg1,by='Group.1')
Agg_merge # explore dataset
# rename columns in Agg_merge
names(Agg_merge)[1]<-'Manufactured_year'
names(Agg_merge)[2]<-'DepDelay'
names(Agg_merge)[3]<-'ArrDelay'
 # creating a new column 'Total_delay' that adds both 'DepDelay' and 'ArrDelay'
Agg_merge$Total_delay<-(Agg_merge$DepDelay + Agg_merge$ArrDelay)/2
Agg_merge # explore dataset
 # we can see elements like 0000 and None under 'Manufactured_year' which does not make sense and must therefore be removed
# we also need to drop any null values in the dataset 'Agg_merge'

Agg_merge<-subset(Agg_merge,Manufactured_year!='0000' & Manufactured_year!='None' & Manufactured_year!='')
colSums(is.na(Agg_merge)) # we see no null values in Agg_merge



# Do Older Planes Suffer More Delays? #



#first we set out criteria for planes to be considered old or new
# we consider planes manufactured 25 years previous to 2007 as old , that is planes manufactured before 1982 are considered older planes

Agg_merge$Manufactured_year<-as.integer(Agg_merge$Manufactured_year)  # converting datatype to integer
Agg_merge$Plane_type <- NA

Agg_merge$Plane_type[Agg_merge$Manufactured_year<1982]<-'Old'
Agg_merge$Plane_type[Agg_merge$Manufactured_year>=1982]<-'New'
Agg_merge # explore dataset

# grouping the Agg_merge by 'Plane_type' and  getting an overall average delay for each plane type
Agg_merge %>%
  group_by(Plane_type)%>%
  summarize(TotalAverageDelay=mean(Total_delay))%>%
  arrange(TotalAverageDelay)
# We can see older planes suffer more delays than new planes
# display the results found above on a barplot
Agg_merge1<-Agg_merge %>%
  group_by(Plane_type)%>%
  summarize(TotalAverageDelay=mean(Total_delay))%>%
  arrange(TotalAverageDelay)
Agg_merge1
Agg_merge1$Plane_type<-factor(Agg_merge1$Plane_type)
ggplot(Agg_merge1, aes(x = Plane_type, y = TotalAverageDelay, fill = Plane_type)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Day of Week", y = "Average Delay (minutes)", title = "Flight Delay With Regard To Plane Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))



# let us also look at the relationship between the age of the plane and delay caused.
# we will be requiring the dataset df_merge
df_merge #explore dataset


# remove elements like '0000', None and '' from Manufactured_year as these do not make sense
df_merge<-subset(df_merge,Manufactured_year!='0000'& Manufactured_year!='None'& Manufactured_year!='')

#create a new column called 'Age'
df_merge<-mutate(df_merge,Age=Year-as.integer(Manufactured_year))
 # check elements under Age column
count(df_merge,Age)
# we can see -1 under Age which does not make sense and must therefore be removed
df_merge<-subset(df_merge,Age!=-1)
count(df_merge,Age) # check values of age again

  # create a new dataframe for further analysis
df_new<-select(df_merge,Age,ArrDelay,DepDelay)
head(df_new) # explore new dataset
# check dataset for null values
colSums(is.na(df_new))

# creating a new column called 'Total_Average_Delay' by summing mean 'ArrDelay' and 'DepDelay' once grouped by Age
agg_new<-aggregate(df_new$ArrDelay,by=list(df_new$Age),FUN=mean)
agg_new
agg_new1<-aggregate(df_new$DepDelay,by=list(df_new$Age),FUN=mean)
agg_new1

agg<-merge(agg_new,agg_new1,by='Group.1')
agg # explore dataset
# rename columns
names(agg)[1]<-'Age'
names(agg)[2]<-'ArrDelay'
names(agg)[3]<-'DepDelay'

agg$TotalAverageDelay<-agg$ArrDelay+agg$DepDelay
agg # explore dataset
 
# displaying the relationship between Age and Delay in a graph below
plot(agg$Age,agg$TotalAverageDelay,main='Relationship Between Age Of A Plane And Flight Delays',
     xlab='Age',ylab='TotalAverageDelay',)
abline(lm(agg$TotalAverageDelay~agg$Age,data=agg),col='blue')

cor(agg$Age,agg$TotalAverageDelay)
# we can see a positive correlation between Age of the plane and delay caused.
