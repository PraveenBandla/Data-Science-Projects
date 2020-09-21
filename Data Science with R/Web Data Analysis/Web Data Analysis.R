# DESCRIPTION:
# 
# Background and Objective:
#   The web analytics team of www.datadb.com is interested to understand the web activities of the site, 
#   which are the sources used to access the website. They have a database that states the keywords of time in the page,
#   source group, bounces, exits, unique page views, and visits.
# 
# Domain: Web

#Data Loading
library(readxl)
wd=read_excel("E:/Education/PGP Simplilearn-Purdue/PGP in Data Science/Data Science with R/Projects/Practice Projects/Web Data Analysis/1555058318_internet_dataset.xlsx")
head(wd)
str(wd) #32,109 rows and 8 columns
as.factor(wd$Continent)->wd$Continent
as.factor(wd$Sourcegroup)->wd$Sourcegroup

#Data Wrangling--Data Cleaning and Pre-processing
#Checking for NA Values
colSums(is.na(wd))

# Analysis Tasks:
# 1. The team wants to analyze each variable of the data collected through data summarization
# to get a basic understanding of the dataset and to prepare for further analysis.

#Data Summarization of each variable
summary(wd)

#2. As mentioned earlier, a unique page view represents the number of sessions during which that page was viewed one or more times. 
# A visit counts all instances, no matter how many times the same visitor may have been to your site. 
# So the team needs to know whether the unique page view value depends on visits.

plot(wd$Visits,wd$Uniquepageviews)
#Scatterplot shows strong positive correlation between vists and unique page views

cor.test(wd$Visits,wd$Uniquepageviews)
#Corelation test also shows strong positive correlation between vists and unique page views with correlation coefficient of 81.44%
#The result is also statistically significant


#3. Find out the probable factors from the dataset, which could affect the exits. 
# Exit Page Analysis is usually required to get an idea about why a user leaves the website for a session and moves on to another one. 
# Please keep in mind that exits should not be confused with bounces.
#Anova test to find out factors impacting exits
aov(Exits~.,data = wd)->model1
summary(model1)

#From the Anova model, we can see that exits are more impacted by bounces,continent,source group,time in page and unique page views.
#Exits also depends on number of visits but have less significance


#4. Every site wants to increase the time on page for a visitor. 
# This increases the chances of the visitor understanding the site content better and 
# hence there are more chances of a transaction taking place. Find the variables which possibly have an effect on the time on page.
aov(Timeinpage~.,data = wd)->model2
summary(model2)

#From the Anova model, we can see that time spent on page is more impacted by bounces,continent,exits,visits and unique page views.
#Timespent on page does not depend on source group 


#5. A high bounce rate is a cause of alarm for websites which depend on visitor engagement.
# Help the team in determining the factors that are impacting the bounce.
aov(Bounces~.,data = wd)->model3
summary(model3)

#From the Anova model, we can see that Bounce rate is more impacted by exits,continent,source group,time in page,visits and unique page views.

#Logistic Regression Model
glm(formula = BouncesNew~.-Bounces,data = wd,family = "binomial")->model4
summary(model4)

#From the logistic regression model, Exits, Unique page views and visits seems to be significantly impacting bounce rate

#Linear Regression Model
model.matrix(object = Bounces~.,data = wd)[,-1]->mat
View(mat)

as.data.frame(mat)->df
head(df)

cbind(wd$Bounces,df)->wd_num
head(wd_num)

colnames(wd_num)[1]="Bounces"

lm(formula = Bounces~.-BouncesNew,data = wd)->model5
summary(model5)

#From linear regression model, Exits, Source groups such as Google, Others, public.tableausoftware.com, tableausoftware.com and visualisingdata.com,
#time spent on page, unique page views and visits are significant impacting bounce rate

#We will consider this model as the result seems to be more intuitive compared to other models

#Multiple R-squared:  0.7855,	Adjusted R-squared:  0.7854 => Accuracy of the model=78.54%






