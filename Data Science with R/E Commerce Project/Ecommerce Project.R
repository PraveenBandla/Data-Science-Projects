#E-Commerce Project-------------------------------------------------------------------------------------------------------
# Analysis tasks to be performed: 
# . Use the clustering methodology to segment customers into groups:
# . Use the following clustering algorithms:
#       1.K means
#       2.Hierarchical
# . Identify the right number of customer segments.
# . Provide the number of customers who are highly valued.
# . Identify the clustering algorithm that gives maximum accuracy and explains robust clusters.
# . If the number of observations is loaded in one of the clusters, break down that cluster further using the clustering algorithm.
# [ hint: Here loaded means if any cluster has more number of data points as compared to other clusters then split that clusters by 
#   increasing the number of clusters and observe, compare the results with previous results.]

ec<-read.csv("E:/Education/PGP Simplilearn-Purdue/PGP in Data Science/Data Science with R/Projects/E Commerce Project/Ecommerce.csv")
head(ec)
str(ec) #541909 rows and 9 columns

library(dplyr)
ec<-as_tibble(ec)
ec=ec[,-9]
head(ec)
View(ec)

#Data Wrangling--Cleaning and Preprocessing
#Checking for 'NA' Values
colSums(is.na(ec))

#Deleting 'NA' Vales
na.omit(ec)->ec

#Calculating total amount spent for a transaction
ec$Total_Amount=ec$Quantity*ec$UnitPrice

ec%>%group_by(CustomerID)%>%summarise(Revenue=sum(Total_Amount),Num_Transactions=n())%>%arrange(desc(Revenue))->ec1
head(ec1)

##Checking for 'NA' Values
colSums(is.na(ec1))

#Deleting 'NA' Vales
na.omit(ec1)->ec1

#Assining customer ID as row names
row.names(ec1)=ec1$CustomerID
ec1$CustomerID=NULL

#Scaling the dataset
scale(ec1)->ec1
head(ec1)

#1. Kmeans clustering--Model1(Taking both Revenue and transaction count for clustering)-------------------------------------------------------
kmeans(x = ec1,centers = 6,iter.max = 10000,nstart = 20)->eccluster1
eccluster1

#Identify the right number of customer segments.
#Insights:
#1.The right number of customer segments can be identified by varying the number of centers and checking for accuracy percentage
#2.I have varied thenumber of centers from 3 to 7 and found that the accuracy % increase drops as i move above 6 clusters
#3.Hence, 6 clusters seems to be appropriate for the given dataset with an accuracy of 86.1%

# Provide the number of customers who are highly valued.
for(i in 1:length(unique(eccluster1$cluster)))
{
  cat("Cluster",i,"\n",length(unique(ec$CustomerID[eccluster1$cluster==i])),"\t")
  cat(mean(ec$Total_Amount[eccluster1$cluster==i]))
  cat("\n")
}

#Customer IDs of High Value Customers
unique(ec$CustomerID[eccluster1$cluster==2])
unique(ec$CustomerID[eccluster1$cluster==5])->hvc1

#Insights:
#1.High value customers can be found by looking at average revenue generated per cluster
#2.CLuster 5 has a highest average revenue of $26.65 per customer with 97 customers and hence, can be considered high value customers
#3.Cluster 2 has second highest average revenue of $22.85 per customer with 161 customers

#1.K-means clustering--Model2(Taking only revenue into consideration)-----------------------------------------------------------------------

ec%>%group_by(CustomerID)%>%summarise(Revenue=sum(Total_Amount))%>%arrange(desc(Revenue))->ec2
head(ec2)

##Checking for 'NA' Values
colSums(is.na(ec2))

#Deleting 'NA' Vales
na.omit(ec2)->ec2

#Assining customer ID as row names
row.names(ec2)=ec2$CustomerID
ec2$CustomerID=NULL

#Not Scaling the dataset in this case as clustering is being done based on only one variable(Revenue)

kmeans(x = ec2,centers = 4,iter.max = 10000,nstart = 20)->eccluster2
eccluster2

#Identify the right number of customer segments.
#Insights:
#1.The right number of customer segments can be identified by varying the number of centers and checking for accuracy percentage
#2.I have varied the number of centers from 3 to 6 and found that the accuracy % increase drops as i move above 4 clusters
#3.Hence, 4 clusters seems to be appropriate for the given dataset with an accuracy of 89.8%

# Provide the number of customers who are highly valued.

for(i in 1:length(unique(eccluster2$cluster)))
{
  cat("Cluster",i,"\n")
  cat(length(unique(ec$CustomerID[eccluster2$cluster==i])),"\t")
  cat(mean(ec$Total_Amount[eccluster2$cluster==i]))
  cat("\n")
}
#Insights:
#1.High value customers can be found by looking at average revenue generated per cluster
#2.Cluster 2 has a highest average revenue of $26.65 per customer with 97 customers and hence, can be considered high value customers
#3.This result validates the output obtained from Model-1
#4.Hence, 97 customers can be considered as high value customers
#5.Since, Model2 has higher accuracy than Model1, We will consider Model2 for clustering this dataset

#Customer IDs of High Value Customers
unique(ec$CustomerID[eccluster2$cluster==2])->hvc2

#2.Hierarchical clustering--Model3(Taking only revenue into consideration)-----------------------------------------------------------------------

ec%>%group_by(CustomerID)%>%summarise(Revenue=sum(Total_Amount))%>%arrange(desc(Revenue))->ec2
head(ec2)

##Checking for 'NA' Values
colSums(is.na(ec2))

#Deleting 'NA' Vales
na.omit(ec2)->ec2

#Assining customer ID as row names
row.names(ec2)=ec2$CustomerID
ec2$CustomerID=NULL

#Calculating dist
dist(ec2)->d

#Not Scaling the dataset in this case as clustering is being done based on only one variable(Revenue)

hclust(d = d,method = "ward.D")->eccluster3
eccluster3

plot(as.dendrogram(eccluster3))
rect.hclust(eccluster3,k = 9)

k=cutree(tree = eccluster3,k=9)

#Identify the right number of customer segments.
#The right number of customer segments can be identified by varying the number of centers and comparing the results with K means clustering

# Provide the number of customers who are highly valued.

for(i in 1:length(unique(k)))
{
  cat("Cluster",i,"\n")
  cat(length(unique(ec$CustomerID[k==i])),"\t")
  cat(mean(ec$Total_Amount[k==i]))
  cat("\n")
}

#Customer IDs of High Value Customers
unique(ec$CustomerID[k==1])->hvc3


#Insights:
#1.High value customers can be found by looking at average revenue generated per cluster
#2.Cluster 1 has a highest average revenue of $26.65 per customer with 97 customers and hence, can be considered high value customers
#This result validates the output obtained from Model-1 and Model-2

#Comparing results from Models 1,2 and 3
sum(hvc1!=hvc2)
sum(hvc2!=hvc3)
sum(hvc1!=hvc3)

#All the three models are fetching same set of cutomer IDs as high value customers.
#hvc1,hvc2 and hvc3 contain same customer IDs who are high value customers 
























