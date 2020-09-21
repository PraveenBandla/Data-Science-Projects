#IBM HR Analytics-Employee Modelling----------------------------------------------------------------------------------------------------------

#DESCRIPTION:
# IBM is an American MNC operating in around 170 countries with major business vertical as computing, software, and hardware. 
# Attrition is a major risk to service-providing organizations where trained and experienced people are the assets of the company. 
# The organization would like to identify the factors which influence the attrition of employees.

#Importing the dataset
ibm<-read.csv("E:/Education/PGP Simplilearn-Purdue/PGP in Data Science/Data Science with R/Projects/Practice Projects/IBM HR Analytics/WA_Fn-UseC_-HR-Employee-Attrition.csv")
str(ibm) #1470 rows and 35 columns
View(ibm)

#Changing column name
colnames(ibm)[1]="Age"

# Exploratory data analysis:
# Find the age distribution of employees in IBM
table(ibm$Age)->age_dist
barplot(age_dist)
hist(ibm$Age)
summary(ibm$Age)
#Age of the employee is slightly positively skewed with majority of the employees between 25-40 years of age

# Find outliers--explore attrition by Age
table(ibm$Attrition,ibm$Age)->age_attr_dist
barplot(age_attr_dist,legend=c("No","Yes"))

# Explore data for Left employees
head(ibm)

#Attrition vs Business Travel
library(dplyr)
table(ibm$BusinessTravel,ibm$Attrition)->table1
df1=as.data.frame(table1)

colnames(df1)
df1=reshape(df1,idvar = "Var1",timevar = "Var2",direction = "wide")

df1%>%group_by(Var1)%>%mutate(attr_rate=Freq.Yes*100/(Freq.Yes+Freq.No))%>%arrange(desc(attr_rate))
#People who travel frequently are more likely to attrite


#Attrition vs Department
library(dplyr)
table(ibm$Department,ibm$Attrition)->table1
df1=as.data.frame(table1)

df1=reshape(df1,idvar = "Var1",timevar = "Var2",direction = "wide")

df1%>%group_by(Var1)%>%mutate(attr_rate=Freq.Yes*100/(Freq.Yes+Freq.No))%>%arrange(desc(attr_rate))
#Sales and HR department has more attrition and R&D has lower attrition


#Attrition vs Education Field
library(dplyr)
table(ibm$EducationField,ibm$Attrition)->table1
df1=as.data.frame(table1)

df1=reshape(df1,idvar = "Var1",timevar = "Var2",direction = "wide")

df1%>%group_by(Var1)%>%mutate(attr_rate=Freq.Yes*100/(Freq.Yes+Freq.No))%>%arrange(desc(attr_rate))
#People from education fields such as HR, technical degree and Marketing have high attrition
#People from education fields such as life sciences, medical and other have low attrition


#Attrition vs Gender
library(dplyr)
table(ibm$Gender,ibm$Attrition)->table1
df1=as.data.frame(table1)

df1=reshape(df1,idvar = "Var1",timevar = "Var2",direction = "wide")

df1%>%group_by(Var1)%>%mutate(attr_rate=Freq.Yes*100/(Freq.Yes+Freq.No))%>%arrange(desc(attr_rate))
#Attrition is slightly higer for male compared to female

#Attrition vs Job role
library(dplyr)
table(ibm$JobRole,ibm$Attrition)->table1
df1=as.data.frame(table1)

df1=reshape(df1,idvar = "Var1",timevar = "Var2",direction = "wide")

df1%>%group_by(Var1)%>%mutate(attr_rate=Freq.Yes*100/(Freq.Yes+Freq.No))%>%arrange(desc(attr_rate))
#Sales representative, Laboratory technician and HR job roles have high attrition rate

#Attrition vs Job satisfaction
library(dplyr)
table(ibm$JobSatisfaction,ibm$Attrition)->table1
df1=as.data.frame(table1)

df1=reshape(df1,idvar = "Var1",timevar = "Var2",direction = "wide")

df1%>%group_by(Var1)%>%mutate(attr_rate=Freq.Yes*100/(Freq.Yes+Freq.No))%>%arrange(desc(attr_rate))
#Attrition rate is inversely proportional to job satisfaction


#Attrition vs Marital status
library(dplyr)
table(ibm$MaritalStatus,ibm$Attrition)->table1
df1=as.data.frame(table1)

df1=reshape(df1,idvar = "Var1",timevar = "Var2",direction = "wide")

df1%>%group_by(Var1)%>%mutate(attr_rate=Freq.Yes*100/(Freq.Yes+Freq.No))%>%arrange(desc(attr_rate))
#Single employees have higher attrition rate compared to married and divorced

 
# Find out the distribution of employees by the education field
table(ibm$EducationField)->edu_dist
barplot(edu_dist)
#There are more employees with Life Sciences and Medical compared to other education fields

# Give a bar chart for the number of married and unmarried employees
table(ibm$MaritalStatus)->mar_dist
barplot(mar_dist)

#Build up a logistic regression model to predict which employees are likely to attrite and find the significant variables
#Checking for NA Values
colSums(is.na(ibm))

library(caTools)
set.seed(100)
split=sample.split(Y = ibm,SplitRatio = 0.7)

ibm_train=ibm[split,]
ibm_test=ibm[!split,]

str(ibm)
table(ibm$EmployeeCount)

#Checking for columns with <2 levels
which(sapply(ibm, function(x) (is.character(x) | is.factor(x) |is.integer(x)) & length(unique(x))<2))

model=glm(formula = Attrition~Age+BusinessTravel+DistanceFromHome+EducationField+EnvironmentSatisfaction+
            HourlyRate+JobInvolvement+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+
            WorkLifeBalance,data = ibm_train,family = "binomial")
summary(model)

step(object = model,direction = "both")->mod
summary(mod)

#Predicting attrition
predict(mod,newdata=ibm_test)->p
ifelse(p>=0.5,1,0)->ypred

yactual=ibm_test$Attrition

table(yactual,ypred)->tab
tab
#TP-20; TN-377; FP-5; FN-60

TP=tab[2,2]
TN=tab[1,1]
FP=tab[1,2]
FN=tab[2,1]

Accuracy=(TP+TN)/(TP+TN+FP+FN)
Accuracy
#Accuracy on test data-85.93%

#Decision Tree--Non-Linear Classification Model------------------------------------------------------------------------------------------
str(ibm_train)
library(rpart)
model=rpart(formula = Attrition~.,data = ibm_train)
library(rpart.plot)
prp(model)
plotcp(model)

#Pruning the tree to avoid overfitting
mod=prune(tree = model,cp=0.01)
prp(mod)


#Accuracy on test data
predict(mod,newdata=ibm_test)->p
ifelse(p>=0.5,1,0)->ypred

ypred=as.data.frame(ypred)
colnames(ypred)

ifelse(ypred$No==1,"No","Yes")->ypred1

yactual=ibm_test$Attrition

table(yactual,ypred1)->tab
tab
#TP-22; TN-359; FP-23; FN-58

TP=tab[2,2]
TN=tab[1,1]
FP=tab[1,2]
FN=tab[2,1]

Accuracy=(TP+TN)/(TP+TN+FP+FN)
Accuracy
#Accuracy on test data-82.46%

#Random Forest--Non-Linear Classification-----------------------------------------------------------------------------------------------
install.packages("randomForest")
library(randomForest)
set.seed(100)
model=randomForest(Attrition~.,data = ibm_train,ntree=500,mtry=5)

predict(object = model,newdata=ibm_test,type="class")->ypred

yactual=ibm_test$Attrition

table(yactual,ypred)->tab
tab

#TP-11; TN-377; FP-5; FN-69

TP=tab[2,2]
TN=tab[1,1]
FP=tab[1,2]
FN=tab[2,1]

Accuracy=(TP+TN)/(TP+TN+FP+FN)
Accuracy
#Accuracy on test data-83.98%

importance(model)->imp
imp
#Mean decrease in Gini index is higher => Purity is higher
#Mean increase in information gain => Purity is higher







