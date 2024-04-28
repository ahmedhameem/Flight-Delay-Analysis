

#load the required data
df<-read.csv('C://Users//Ibrahim Hameem//Pictures//Ahmed Coursework//data_clean.csv')

#installing required packages
install.packages('dplyr')
#upload the required library
library(dplyr)


head(df)  # explore dataframe


#explore column of df and drop and the columns which cannot be encoded and and included in the model
colnames(df)
str(df)
df$FlightNum<-NULL
df$Cancelled<-NULL
df$Diverted<-NULL
df$Cancelled<-NULL
df$UniqueCarrier<-NULL
df$TailNum<-NULL
df$Origin<-NULL
df$Dest<-NULL
colnames(df)


# now we create a binary column by using ArrDelay and set out criteria to identify delays
# we consider an ArrDelay > 0 a delay and ArrDelay<=0 as No delay
#delays will be represented with '1' and no delays will be represented with '0'

Delay<-ifelse(df$ArrDelay>0,1,0)
df$Delay<-Delay


# plot a correlation heatmap with all the variables in df
install.packages('corrplot')
library(corrplot)
corrplot(abs(cor(df)), method = "color", 
         title = "Correlation Heatmap", 
         tl.col="black", tl.srt=360,
         addCoef.col = "grey", 
         number.cex = 0.5,
         tl.cex = 0.5, 
         type = "lower",
         is.corr = FALSE,
         col=colorRampPalette(c("lightyellow","lightblue1","dodgerblue3"))(100))


# lets now check the class balance of 'Delay'
table(df$Delay)

#check df for null values
colSums(is.na(df))  #we see no null values


#plotting a correlation heatmap with just our feature variables and target variable
#install required package
install.packages("ggcorrplot")
library(ggcorrplot)

#selecting our feature variables and target variable
df<-select(df,CRSDepTime,DayOfWeek,Month,DepDelay,TaxiIn,TaxiOut,LateAircraftDelay,Delay)
head(df) 
table(df$Delay)
colSums(is.na(df))


#select the columns for correlation heatmap
column<-c('CRSDepTime','DayOfWeek','Month','DepDelay','TaxiIn','TaxiOut','LateAircraftDelay','Delay')

corr_matrix<-cor(df[,column])

ggcorrplot(corr_matrix,hc.order=TRUE, colors =c('#6D9EC1','blue','dodgerblue3'),
           lab = TRUE,lab_size=4,title='Correlation Heatmap',ggtheme=ggplot2::theme_gray)

#install required library
install.packages('caret')
library(caret)
install.packages('caTools')
library(caTools)
install.packages("glmnet")
library(glmnet)

#split dataset into features and target variable
features<-c('CRSDepTime','DayOfWeek','Month','DepDelay','TaxiIn','TaxiOut','LateAircraftDelay','Delay')
X<-df[,features]
Y<-df$Delay

#split X and Y into training and testing datasets
set.seed(123)
split<-sample.split(Y,SplitRatio = 0.75)
training_set<-subset(X,split=TRUE)
test_set<-subset(X,split=FALSE)

#feature scaling , # we scale only the features and not the dependent variable
scaler_agent <-scale(training_set[-8])
training_set[-8] <- scale(training_set[-8],center = attr(scaler_agent,"scaled:center"), 
                         scale = attr(scaler_agent,"scaled:scale"))
test_set[-8] <-scale(test_set[-8],center = attr(scaler_agent,"scaled:center"), 
                     scale = attr(scaler_agent,"scaled:scale"))

#fitting the logistic regression to the training set
classifier<-glm(formula = Delay~.,
                family = binomial,
                data=training_set)

glm

# Predicting the Test set results
prob_pred <-predict(classifier, type = 'response', newdata = test_set[-8])
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm <- table(test_set[, 8], y_pred > 0.5,dnn = c("Actual", "Predicted"))
colnames(cm) <- rownames(cm)
cm
install.packages('pheatmap')
library(pheatmap)
pheatmap(cm,
         number_color = 'black',
         display_numbers = TRUE, 
         fontsize = 25,
         rownames = rownames(cm),
         colnames = colnames(cm))

# we use the confusion matrix to calculate the performance metrics of our logistic regression model
# 0 denotes no delay and 1 denotes delay
# Test Accuracy
accuracy <- round(sum(cm[c(1,4)])/sum(cm)*100,2)/100
accuracy

#calculate precision of model
precision_delay<-(cm[2,2]/sum(cm[2,]))
precision_delay
precision_nodelay<-(cm[1,1])/sum(cm[1,])
precision_nodelay

#calculate recall of model
recall_delay<-(cm[2,2]/sum(cm[,2]))
recall_delay

recall_nodelay<-(cm[1,1]/sum(cm[,1]))
recall_nodelay

# calculate the f1 score of model
f1_delay<-2*(precision_delay*recall_delay)/(precision_delay+recall_delay)
f1_delay

f1_nodelay<-2*(precision_nodelay*recall_nodelay) / (precision_nodelay+recall_nodelay)
f1_nodelay

# let us plot a receiver operating characteristic curve (ROC)
install.packages('ROCR')
library(ROCR)
install.packages('pROC')
library(pROC)


curve<-roc(test_set$Delay~prob_pred,plot=TRUE,print.auc=TRUE,main='Reciever Operating Characteristic Curve',xlab='False_Positive_Rate',ylab='True_Positive_Rate')
curve$specificities<-rev(curve$specificities)
plot(curve,col='blue',lwd=2,legacy.axes=TRUE,print.auc=TRUE,main='Reciever Operating Characteristic Curve',xlab='False_Positive_Rate',ylab='True_Positive_Rate')


#performance metric summary
Performance_Summary<-data.frame(accuracy,precision_delay,precision_nodelay,recall_delay,recall_nodelay,f1_delay,f1_nodelay)
Performance_Summary

