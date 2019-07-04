rm(list=ls(all=T))
getwd()
#setwd("/Users/apple/Desktop/edwisory/project1/employee_absenteeism/")
#install libraries 

#x=c("forecast","xlsx","outliers","tseries","lmtest")
install.packages("gdata")
#Libraries imported
#library(xlsx)
library(ggplot2)
library(gridExtra)
library(DMwR)
library(corrgram)
library(forecast)
library(outliers)
library(plyr)
library(tseries) #for stationarity
library(lmtest) #For testing autocorrelation
library(gdata)



#importing employee absenteeism data set
ea_df = read.xls("Absenteeism_at_work_Project.xls", sheet = 1, header = T)

"As it's 3 years of data from July 2007 to July 2010(37 months) and we don't have a 'Year' variable in the dataset, 
including that variable will help in our seasonal analysis"

ea_df[1:113, 'Year'] = 2007
ea_df[114:358, 'Year'] = 2008
ea_df[359:570, 'Year'] = 2009
ea_df[571:740, 'Year'] = 2010

#Changing the position of the Year variable
ea_df = ea_df[,c(1,2,3,22,4:21)]

#Checking data type and structure
head(ea_df, 10); 

summary(ea_df)

names(ea_df);
str(ea_df);


"
We have 3 data points which are at bottom where monthofabsence is 0(which doesn't mean anything) we will remove these rows. The rows have reason of 
abscene 0 and absenteeism time 0 as well and uneven season pattern.

"
ea_df = ea_df[1:737,]

"We have a month of 1 in 2007 which is not possible as the data is given from July 2007, seems to be a typo error as
it is between month 10, missplaced as 1. We will replace it to 10
"

ea_df[67, 'Month.of.absence'] = 10

"
As we can see our data has all numeric variables, but we can see that few variables are category time,
so converting those variables into factors for our analysis
"

factor_variables = c('ID',"Reason.for.absence", "Month.of.absence","Day.of.the.week", "Seasons", "Disciplinary.failure",
                     "Education", "Son", "Social.drinker", "Social.smoker", "Pet")
for(i in factor_variables)
{
  ea_df[,i] = as.factor(ea_df[,i])
}

#Confirming the changes type
str(ea_df)






#*****************Completeness of data******************************

#Missing value Analysis
#Creating a data frame with missing value count
missing_val = data.frame(apply(ea_df,2,function(x){sum(is.na(x))}))
#Adding a columns with features and traget variable name
missing_val$Columns = row.names(missing_val)
#Renaming the missing value columns with 'missing_percentage' 
names(missing_val)[1] =  "missing_percentage"
#Calculating the percentage
missing_val$missing_percentage = (missing_val$missing_percentage/nrow(ea_df)) * 100
#Changing the position of 'missing_percentage'
missing_val = missing_val[order(-missing_val$missing_percentage),]
#Removing the first redundant column
row.names(missing_val) = NULL
#Changing the position of columns
missing_val = missing_val[,c(2,1)]
#Saving the missing percentages in a csv
write.csv(missing_val, "Miising_perc.csv", row.names = F)


ggplot(data = missing_val[1:10,], aes(x=reorder(Columns, -missing_percentage),y = missing_percentage))+
  geom_bar(stat = "identity",fill = "grey",size=25)+ xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()



"As we can see we don't have much missing values per column, the highest we have is 4.18% for Body.mass.index feature
*So we will be imputing the missing values with the appropriate technique
*Try trial and error method, to check when technique from Mean, median and Knn imputation works fine in our dataset
*First imputing NA's for the values we already know and we will se which technique works best.

Imputing: row no BMI for row no 139 which is 23 with NA
row no 194 and 707 for Absenteeism time which is 3 and 8 with NA
Mean Method:194:6.981844 , 707:6.981844, 139: 26.68, 139:25

Median Method: 194:3 , 707:3 ,139:25
kNN Imputation
194:1.58, 707: 7.274897
139:23.71572
"
"We are going with Knn imputation as the results of the manully imputated variables to NA seems to be closer to 
the actual values.
Mean gives 6.98 for all the values of Absenteeism.time.in.hours, which can increase our overall mean of the target variable
And median decreases.
The most appropriate seems to be Knn imputation in this case so freezing this.
"

#kNN Imputation
ea_df = knnImputation(ea_df, k = 3)


#Confirming there are no na's left
sum(is.na(ea_df))

#Structure of data after imputing
str(ea_df)

#Summary of data
summary(ea_df)

"Just having a glance at the data it looks like there are many outliers in the data set.
Reasons: The abseenteeism.time.in.hour cannot be greater than 24 hours as it's a daily data, in some cases it is more
than 24 even 120 which is not possible.
Reasons: Service.Time: Service time is the daily service hours of the persons, which cannot be more than 12 hours(keeping it as 24) but
I can see data points with 29 hours which is not possible.

"

#So lets perform outlier analysis first as visualizing the data will be better after this





#********************Outlier Analysis**********************

numeric_index = sapply(ea_df,is.numeric) #selecting only numeric

numeric_data = ea_df[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(ea_df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of Absenteeism.time.in.hours for",cnames[i])))
}

# ## Plotting together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)



#As our data is less we will impute the outliers treating them as missing value with the help of KnnImputation
for(i in cnames)
{
  cnames
  val = ea_df[,i][ea_df[,i] %in% boxplot.stats(ea_df[,i])$out]
  print(length(val))
  ea_df[,i][ea_df[,i] %in% val] = NA
}

"
ID , Transportation.expense, Distance.from.Residence.to.Work
Service.time, Age, Work.load.Average.day., Hit.target, Weight, Height,Body.mass.index, Absenteeism.time.in.hours
[1] 3
[1] 0
[1] 5
[1] 8
[1] 31
[1] 19
[1] 0
[1] 119
[1] 0
[1] 44 
"


#As knnimputation takes only numeric variables into consideration, converting all the 
ea_df = knnImputation(ea_df, k = 3)

#Confirming that there is no missing value left
sum(is.na(ea_df))

#Checking the structure again
str(ea_df)

write.csv(ea_df, "CleanedData.csv", row.names = T)

##*********************Correlation Plot************************

corrgram(ea_df, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#Checking the correlation with churn using anova to finding the corrleation between the target continous and categorical features
#From the correlated variable

avo1 = aov(ea_df$Absenteeism.time.in.hours ~ ea_df$Day.of.the.week)
avo2 = aov(ea_df$Absenteeism.time.in.hours ~ ea_df$Seasons)
avo3 = aov(ea_df$Absenteeism.time.in.hours ~ ea_df$Disciplinary.failure)
avo4 = aov(ea_df$Absenteeism.time.in.hours ~ ea_df$Education)
avo5 = aov(ea_df$Absenteeism.time.in.hours ~ ea_df$Son)
avo6 = aov(ea_df$Absenteeism.time.in.hours ~ ea_df$Social.drinker)
avo7 = aov(ea_df$Absenteeism.time.in.hours ~ ea_df$Social.smoker)
avo8 = aov(ea_df$Absenteeism.time.in.hours ~ ea_df$Pet)

#***************Exploring some important variables***********************

#Visualizing data
#Univariate analysis of important continuous variable
#Making the density plot
g1 = ggplot(ea_df, aes(x = Transportation.expense)) + geom_density()
g2 = ggplot(ea_df, aes(x = Distance.from.Residence.to.Work)) + geom_density()
g3 = ggplot(ea_df, aes(x = Age)) + geom_density()
g4 = ggplot(ea_df, aes(x = Service.time)) + geom_density()
g5 = ggplot(ea_df, aes(x = Work.load.Average.day.)) + geom_density()
g6 = ggplot(ea_df, aes(x = Hit.target)) + geom_density()
g7 = ggplot(ea_df, aes(x = Weight)) + geom_density()
g8 = ggplot(ea_df, aes(x = Height)) + geom_density()
g9 = ggplot(ea_df, aes(x = Body.mass.index)) + geom_density()
g10 = ggplot(ea_df, aes(x = Absenteeism.time.in.hours)) + geom_density()

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, ncol=2)




#Exploring top 5 important features:
#Disciplinary failure, Reason of Absence, Social Drinker, Social Smoker and Son. 

aggre.absent.desciplinary_failure =  ddply(ea_df, c( "Disciplinary.failure"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

#Count for Reason of absence
g11 = ggplot(ea_df, aes(x = Reason.for.absence, fill = Reason.for.absence)) + geom_bar(stat = 'count')  +
  geom_label(stat='count',aes(label=..count..), size=5) + labs(ggtitle("Count for Reason of Absence")) +
  theme_grey(base_size = 18)
g11
#Reasons for absence according to day or week, seasons and month of absence
g12 = ggplot(ea_df, aes(x = Reason.for.absence, fill = Day.of.the.week)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) + labs(ggtitle("Reason of absence based day of week"))+
  theme_grey(base_size = 18)
g12

g13 = ggplot(ea_df, aes(x = Reason.for.absence, fill = Seasons)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) + labs(ggtitle("Reason of absence based on seasons of all the years"))+
  facet_grid(Year~.)
theme_grey()
g13

g14 = ggplot(ea_df, aes(x = Reason.for.absence, fill = Month.of.absence)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..))+ labs(ggtitle("Count of reason for absence per year")) +
  facet_grid(Year~.) +
  theme_grey()
g14




#Aggregating the absentense hours based on ID
aggre.absent.ID =  ddply(ea_df, c( "ID"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))
#Aggregating the absendtense hours based on Reason of Absence and ID 
aggre.absent.id.reasons=  ddply(ea_df, c("ID", "Reason.for.absence"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))



#Aggregating based on sons, smoker, drinker
aggre.absent.son=  ddply(ea_df, c("Son"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

aggre.absent.drinker=  ddply(ea_df, c("Social.drinker"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

aggre.absent.smoker =  ddply(ea_df, c("Social.smoker"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))


#Modelling

#Aggregating the data based on Absenteeism time in hour per month for an year
aggre.absent.hours.months =  ddply(ea_df, c("Year", "Month.of.absence"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))


"
Adding the time stamp to the aggre.absent.hours.months, as we need to forecast monthly absenteeism and we have clubbed the data 
based on months, adding frequency = 12, start = c(2007,7) as we have data from July 2007.

"
ts_ea_df = ts(aggre.absent.hours.months$Absenteeism.time.in.hours, frequency = 12,start = c(2007, 7))
autoplot(ts_ea_df)
write.csv(aggre.absent.hours.months, "Absentese_per_month11.csv", row.names = T)

#Diving into train and test; 80:20 split
train = window(ts_ea_df, start = c(2007,7), end = c(2009,12))
test = window(ts_ea_df, start = c(2010))

#Decomposition
plot(decompose(ts_ea_df))


#Check for stationarity
#There is non - stationarity in the dataset
adf.test(ts_ea_df, k = 1)#Stationarity in the dataset  
# null hypothesis = time series is non- stationary
#Alternative = time series is stationary
#P value less than 0.05 reject the null hypothesis

"
Augmented Dickey-Fuller Test

data:  ts_ea_df
Dickey-Fuller = -5.5232, Lag order = 1, p-value = 0.01
alternative hypothesis: stationary

"

#Test for autocorrelation
dwtest(ts_ea_df[-37] ~ ts_ea_df[-1])

"
Durbin-Watson test

data:  train[-37] ~ train[-1]
DW = 1.9684, p-value =  0.4584
alternative hypothesis: true autocorrelation is greater than 0

"
#There seems to be no autocorrelation in the series
#Null hypothesis: There is no autocorrelation


set.seed(123)

#***********************Applying time series linear model with trend*********************

#Training the model
fit =  tslm(train~ season + trend)

#Summary of the fit
summary(fit)

#AIC score of tslm
AIC(fit)

#Residuals
checkresiduals(fit)

#Lets forcast for next 18 months
forecast_tslm = forecast(fit, h = 18)
autoplot(forecast_tslm)

#Lets check the accuracy
accuracy(forecast_tslm, test)
forecast_tslm

#**************************Applyting auto.arima****************************

#Checkingthe ACF and PACF PLOT
tsdisplay(train)
#*Autocorrelation: The correlation coefficient between different time points(lags) in a time series
#*Partial autocorrelation: The correlation coefficient adjusted for all shorter lags in a time series
#*the acf() is used to identify the moving average(MA) part of the ARIMA model, while pacf()
#identifies the values for the autoregressive part(AR)

#Training the model
arima_fit = auto.arima(train, trace = T)

#Forecasting
arimafore = forecast(arima_fit, h =18)
autoplot(arimafore)
arimafore
#Accuracy
accuracy(arimafore, test)

#Residuals
checkresiduals(arima_fit)



#*********************ETS Model**********************


#Training the model
etsmodel = ets(train)
#Model appears to be "ANN" as expected as there is no trend and seasonality

#Forecasting
ets_forecast = forecast(etsmodel, h = 18)
autoplot(ets_forecast)
ets_forecast
#Accuracy
accuracy(ets_forecast, test)

#Residuals
checkresiduals(etsmodel)



#*******Important Note********
"Increasing the test data is leading to RMSE: 29 and MAPE: 26 i.e a high error score.
Reason: There has been a lot of variation in the recent months for example May which has absentees's hours of 62.17 and 75
in 2008 and 2009 has a absentees hours 131 in 2010, taking this in test data is not correct as model is not able to predict
this drastic variation. I've tried all the model tuning operations but it doesn't work out as the model is not trained with
such a drastic change in trend. We can also not select random train and test since since this would prevent the model from catching autocorrelation which relies on the c
correct succession of the time data.
"

accuracy(forecast_tslm, test)
accuracy(arimafore, test)
accuracy(ets_forecast, test)
