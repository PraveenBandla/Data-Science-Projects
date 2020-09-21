#Project: Retail Analysis-Walmart
#Importing data in csv file into R
wm<-read.csv("E:/Education/PGP Simplilearn-Purdue/PGP in Data Science/Data Science with R/Projects/Retail Analysis with Walmart data/Walmart_Store_sales.csv",stringsAsFactors = F,na.strings = c("","NA","<NA>"))
head(wm)
View(wm)
str(wm)#6435rows and 8columns

#Data Wrangling--Cleaning and Preprocessing
#Checking for 'NA' Values
colSums(is.na(wm))

#Checking for duplicates
n=data.frame(table(wm$Store,wm$Date))
n[n$Freq>1,]

#Converting date column from character type to date
as.Date(wm$Date,format = "%d-%m-%Y")->wm$Date
head(wm)

#Converting Holiday flag to factor variable
wm$Holiday_Flag=as.factor(wm$Holiday_Flag)

#Analysis Tasks
#Basic Statistics Tasks
#1.Store with maximumm sales---------------------------------------------------------------------------------------------------
library(dplyr)
wm%>%group_by(Store)%>%summarise(Sales_sum=sum(Weekly_Sales))%>%arrange(desc(Sales_sum))%>%filter(row_number()==1)
#Insights:
#Store 20 has maximum sales with 301397792 units

#2.Store with maximum standard deviation in sales. Aslo, Calculate co-efficient of Mean to Standard Deviation------------------
wm%>%group_by(Store)%>%summarise(stddev_sales=sd(Weekly_Sales),coeff=mean(Weekly_Sales)/stddev_sales,cv=stddev_sales*100/mean(Weekly_Sales))%>%arrange(desc(stddev_sales))%>%filter(row_number()==1)
#Insights:
#Store 14 has maximum standard deviation with 317570 units, co-efficient of mean to standard deviation is 6.36 and
#Coefficient of Variation is 15.7%

#3.Which store/s has good quarterly growth rate in Q3'2012----------------------------------------------------------------------
#Creating a new column called 'Quarter' and 'Year'
quarters(wm$Date)->wm$Quarter
ifelse(wm$Quarter=="Q1",1,ifelse(wm$Quarter=="Q2",2,ifelse(wm$Quarter=="Q3",3,4)))->wm$Quarter
library(lubridate)
year(wm$Date)->wm$Year
month(wm$Date)->wm$Month
semester(wm$Date)->wm$Semester
head(wm)

#Successive quarterly growth rate
wm%>%group_by(Store,Quarter,Year)%>%summarise(Sales_sum1=sum(Weekly_Sales))%>%
  filter((Quarter=="Q2"|Quarter=="Q3")&Year=="2012")->Quarterly_Sales

Quarterly_Sales%>%group_by(Store)%>%mutate(growth_rate=((Sales_sum1/lag(Sales_sum1,1))-1)*100)%>%arrange(desc(growth_rate))
#Insights:
#Store 7 has high successive quarterly growth rate of 13.3% in Q3'2012 compared to Q2'2012

#Year-on-Year quarterly growth rate
wm%>%group_by(Store,Quarter,Year)%>%summarise(Sales_sum2=sum(Weekly_Sales))%>%
  filter((Quarter=="Q3")&(Year=="2011"|Year=="2012"))->Quarterly_Sales1

Quarterly_Sales1%>%group_by(Store)%>%mutate(growth_rate=((Sales_sum2/lag(Sales_sum2,1))-1)*100)%>%arrange(desc(growth_rate))
#Insights:
#Store 44 has high Year-on-Year quarterly growth rate of 4.96% in Q3'2012 compared to Q3'2011

#4.Some holidays have a negative impact on sales---------------------------------------------------------------------------------------
#Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
#Appending Holiday name to the data frame
wm$Holiday_Name=ifelse(wm$Holiday_Flag==1,ifelse(wm$Month==2,"Super Bowl",ifelse(wm$Month==9,"Labour Day",ifelse(wm$Month==11,"Thanksgiving",ifelse(wm$Month==12,"Christmas","")))),"Non-Holiday")
head(wm)

#Arranging mean sales by holiday name
wm%>%group_by(Holiday_Name)%>%summarise(Mean_Sales=mean(Weekly_Sales))%>%arrange(desc(Mean_Sales))
#Insights:
#Thanksgiving, Super bowl and Labour day have higher sales than the mean sales of non-holiday season
#Christmas have lower sales than the mean sales of non-holiday season

#5.Provide a monthly and semester view of sales in units and give insights-------------------------------------------------------------
#Appending month name column
month.name[wm$Month]->wm$Month_Name

wm%>%group_by(Month_Name)%>%summarise(MSales_sum=mean(Weekly_Sales))%>%arrange(desc(MSales_sum))
#Insights:
#1. December, November have the highest monthly mean sales
#2. January, September have the lowest monthly mean sales 

wm%>%group_by(Year,Semester)%>%summarise(MSales_sum=mean(Weekly_Sales))%>%arrange(desc(MSales_sum))
#Insights:
#1. Send semester has higher mean sales than first semester in 2010 and 2011
#2. For 2012, First and second semester mean sales are almost the same
#3. However, Since, November and December data is missing for 2012, which have highest monthly sales on average,
#We can state that, Second semester seems to record higher mean sales than first semester

head(wm)
str(wm)

#Statistical Model
#For Store 1 - Build  prediction models to forecast demand
#Linear Regression - Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order).
#Hypothesize if CPI, unemployment, and fuel price have any impact on sales.

wm[wm$Store==1,]->wm_store1
head(wm_store1)
str(wm_store1)
wm_store1$Store=as.numeric(wm_store1$Store)

#Checking importance of independent variables through Visualizations and statistical tests
#Mean Weekly sales vs Holiday flag
tapply(wm_store1$Weekly_Sales,list(wm_store1$Holiday_Flag),mean)->tab
barplot(tab,col = c("lightblue","lightgreen"),xlab = "Holiday Flag",ylab = "Mean Weekly Sales",main = "Mean Weekly Sales vs Holiday Flag",ylim = c(0,2000000))
#Mean Holiday sales for store 1 are slighly higher than that of non-holiday sales=>Holiday flag should be considered in our model

t.test(Weekly_Sales~Holiday_Flag,data=wm_store1)
#Statistical tests show that mean difference between holiday and non-holiday sales are not statistically significant 

#Weekly sales vs Temperature
plot(wm_store1$Temperature,wm_store1$Weekly_Sales)
cor.test(wm_store1$Temperature,wm_store1$Weekly_Sales)
#Weekly sales are slightly negatively correlated with temperature and is statistically significant

#Weekly sales vs fuel price
plot(wm_store1$Fuel_Price,wm_store1$Weekly_Sales)
cor.test(wm_store1$Fuel_Price,wm_store1$Weekly_Sales)
#Weekly sales are slightly positively correlated with fuel price and is not statistically significant

#Weekly sales vs CPI
plot(wm_store1$CPI,wm_store1$Weekly_Sales)
cor.test(wm$CPI,wm$Weekly_Sales)
#Weekly sales are not correlated with CPI and the result is statistically significant

#Weekly sales vs Unemployment
plot(wm_store1$Unemployment,wm_store1$Weekly_Sales)
cor.test(wm$Unemployment,wm$Weekly_Sales)
#Weekly sales are slightly negatively correlated with unemployment and is statistically significant

head(wm_store1)

#Since, we are predicting weekly sales data, It is better to consider week number rather than month or quarter or year
library(lubridate)
week(wm_store1$Date)->wm_store1$Week_num
head(wm_store1)
str(wm_store1)

#Linear Regression-Model 1------------------------------------------------------------------------------------------------
#Converting all the variables into numeric type and removing redundant variables
library(dplyr)
wm_store1_num=wm_store1%>%select(-Date,-Year,-Month_Name,-Holiday_Name)
#wm_store1_num=wm_store1%>%select(-Date,-(Quarter:Month_Name))

head(wm_store1_num)
str(wm_store1_num)

#Linear Regression Model
#Splitting the data into training and testing sets(70:30 ratio)
library(caTools)
set.seed(100)
split=sample.split(wm_store1_num,SplitRatio = 0.7)

wm_train=wm_store1_num[split,]
wm_test=wm_store1_num[!split,]

str(wm_train)
str(wm_test)

#Perform linear regression on training data
lm(formula = Weekly_Sales~.-Store,data = wm_train)->model
summary(model)

step(object = model,direction = "both")->mod
summary(mod)

#Insights(Model1):
#Unemployment, Month and Week number variables are coming out to be significant variables (for 5% significance level) that impact weekly sales
#formula = Weekly_Sales ~ Holiday_Flag + Temperature + Unemployment + Quarter + Month + Semester + Week_num
#Multiple R-squared:  0.4202,	Adjusted R-squared:  0.3713 => Accuracy on training data=37.13%

predict(object = mod,newdata=wm_test)->Sales_pred
wm_test$Weekly_Sales->Sales_actual

error=Sales_pred-Sales_actual 

sum(error^2)->sse
ybar=mean(wm_train$Weekly_Sales)
sst=sum((Sales_actual-ybar)^2)

rsquared=1-sse/sst
rsquared
#Accuracy on test data=14.39%

#Linear Regression-Model 2---------------------------------------------------------------------------------------
#Converting all the variables into numeric type and removing redundant variables
head(wm_store1)
str(wm_store1)

wm_store1_num=wm_store1%>%select(-Date,-Year,-Month_Name,-Holiday_Flag)
head(wm_store1_num)

#Converting factor variables to numeric
model.matrix(object = Weekly_Sales~.,data = wm_store1_num)[,-1]->mat
head(mat)

as.data.frame(mat)->df
head(df)

#Splitting the data into training and testing sets(70:30 ratio)
library(caTools)
set.seed(100)
split=sample.split(df,SplitRatio = 0.7)

wm_train=wm_store1_num[split,]
wm_test=wm_store1_num[!split,]

#Perform linear regression on training data
lm(formula = Weekly_Sales~.-Store,data = wm_train)->model
summary(model)

step(object = model,direction = "both")->mod
summary(mod)
#Insights(Model2):
#CPI, Temperature, Holiday name, Month, Semester and Week number variables are coming out to be significant variables (for 5% significance level) that impact weekly sales
#formula = Weekly_Sales ~ Temperature + CPI + Month + Semester + Holiday_Name + Week_num
#Multiple R-squared:  0.4495,	Adjusted R-squared:  0.3938 => Accuracy on training data=39.38%

predict(object = mod,newdata=wm_test)->Sales_pred
wm_test$Weekly_Sales->Sales_actual

error=Sales_pred-Sales_actual 

sum(error^2)->sse
ybar=mean(wm_train$Weekly_Sales)
sst=sum((Sales_actual-ybar)^2)

rsquared=1-sse/sst
rsquared
#Accuracy on test data=36.93%

#Note:
#Accuracy on training and test sets is better for Model2 and hence Model2 is better at predicting weekly sales
#Also, Accuracy is low even in Model2. This is because of lack of additional data such as Markdowns, Store size and Department.  

#Linear Regression-Model 3------------------------------------------------------------------------------------------------
#Converting all the variables into numeric type and removing redundant variables
library(dplyr)
str(wm)

wm_num=wm%>%select(-Date,-Year,-Month_Name,-Holiday_Name)
#wm_store1_num=wm_store1%>%select(-Date,-(Quarter:Month_Name))

head(wm_num)
str(wm_num)

#Converting factor variables to numeric
model.matrix(object = Weekly_Sales~.,data = wm_num)[,-1]->mat
head(mat)

as.data.frame(mat)->df
head(df)

#Combine Weekly Sales column with numeric data
cbind(wm_num$Weekly_Sales,df)->wm_num
head(wm_num)

names(wm_num)[1]="Weekly_Sales"

#Linear Regression Model
#Splitting the data into training and testing sets(70:30 ratio)
library(caTools)
set.seed(100)
split=sample.split(wm_num,SplitRatio = 0.7)

wm_train=wm_num[split,]
wm_test=wm_num[!split,]

str(wm_train)
str(wm_test)

#Perform linear regression on training data
lm(formula = Weekly_Sales~.,data = wm_train)->model
summary(model)

step(object = model,direction = "both")->mod
summary(mod)

#Insights(Model1):
#1. Unemployment, CPI, Holiday flag, Temperature, Fuel Price, QUarter, Semester, Month, Week number and specific store numbers variables 
#are coming out to be significant variables (for 5% significance level) that impact weekly sales

#2. formula = Weekly_Sales ~ Store2 + Store3 + Store4 + Store5 + 
#  Store7 + Store8 + Store9 + Store10 + Store11 + Store12 + 
#  Store13 + Store14 + Store15 + Store16 + Store17 + Store18 + 
#  Store19 + Store20 + Store21 + Store22 + Store23 + Store25 + 
#  Store26 + Store27 + Store28 + Store29 + Store30 + Store31 + 
#  Store32 + Store33 + Store34 + Store35 + Store36 + Store37 + 
#  Store38 + Store39 + Store40 + Store41 + Store42 + Store43 + 
#  Store44 + Store45 + Holiday_Flag1 + Temperature + Fuel_Price + 
#  CPI + Unemployment + Quarter + Month + Semester + Week_num

#3. Multiple R-squared:  0.9323,	Adjusted R-squared:  0.9315 => Accuracy on training data=93.15%

predict(object = mod,newdata=wm_test)->Sales_pred
wm_test$Weekly_Sales->Sales_actual

error=Sales_pred-Sales_actual 

sum(error^2)->sse
ybar=mean(wm_train$Weekly_Sales)
sst=sum((Sales_actual-ybar)^2)

rsquared=1-sse/sst
rsquared
#Accuracy on test data=92.22%










