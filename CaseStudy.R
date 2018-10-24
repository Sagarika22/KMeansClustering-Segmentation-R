# 12-16 , 19, 31

library(readxl)
DM_Sheet <- read_excel("~/UIC/Courses/DataMining/Assignments/Assignment6/Datasets/DM_Sheet.xlsm")
View(DM_Sheet)

BathSoap_Data <- DM_Sheet
View(BathSoap_Data)
dim(BathSoap_Data)
str(BathSoap_Data)
sum(is.na(BathSoap_Data))

## Part (a)
BrandLoyalty<-BathSoap_Data[,12:31]
dim(BrandLoyalty)
View(BrandLoyalty)
str(BrandLoyalty)

vol <- function(x){
  return(x*BrandLoyalty$`Total Volume`)
}

vol_Data<-as.data.frame(lapply(BrandLoyalty[9:20],vol))
View(vol_Data)

Brand_final <- BrandLoyalty[,1:8]
Brand_final <- cbind(Brand_final,vol_Data)
View(Brand_final)
Brand_final$max <- apply(Brand_final[,12:19], 1, max)
View(Brand_final)

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
Brand_normData <- as.data.frame(lapply(Brand_final[c(1:5,8,20,21)], normalize))

View(Brand_normData)

set.seed(7)
km_Brand = kmeans(Brand_normData, 2, nstart =100)
km_Brand

km_Brand$cluster
km_Brand$centers
km_Brand$withinss
km_Brand$betweenss
km_Brand$size
km_Brand$iter

# Plot results
plot(Brand_normData, col =(km_Brand$cluster) , 
     main="K-Means with 2 clusters", 
     pch=30, cex=2)

#Scree Plot - Check for the optimal number of clusters given the data
Brand_normData_1 <- Brand_normData
wss <- (nrow(Brand_normData_1)-1)*sum(apply(Brand_normData_1,2,var))
wss

for (i in 2:15) 
  wss[i] <- sum(kmeans(Brand_normData_1, 
                       centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Assessing the above Scree Plot we found that no. of cluster = 7 is the optimum point
# Thus, we will run the K-means clustering algorithm again using k=
set.seed(7)
km_brand_1 = kmeans(Brand_normData_1, 4, nstart=100)
km_brand_1

km_brand_1 = kmeans(Brand_normData_1, 5, nstart=100)
km_brand_1

km_brand_1 = kmeans(Brand_normData_1, 6, nstart=100)
km_brand_1

km_brand_1 = kmeans(Brand_normData_1, 7, nstart=100)
km_brand_1

km_brand_1$tot.withinss/km_brand_1$totss

# Plot results
col =(km_brand_1$cluster +1)
plot(Brand_normData_1, col = col, main="K-Means result with 7 clusters",
     pch=20, cex=2)
points(km_brand_1$centers, col=col, pch=19, cex=2)

# To get all the data instances that belong to cluster one you can use the following code
Cluster1_Instances = Brand_normData_1[km_brand_1$cluster == 1, ]
Cluster2_Instances = Brand_normData_1[km_brand_1$cluster == 2, ]
Cluster3_Instances = Brand_normData_1[km_brand_1$cluster == 3, ]
Cluster4_Instances = Brand_normData_1[km_brand_1$cluster == 4, ]
Cluster5_Instances = Brand_normData_1[km_brand_1$cluster == 5, ]
Cluster6_Instances = Brand_normData_1[km_brand_1$cluster == 6, ]
Cluster7_Instances = Brand_normData_1[km_brand_1$cluster == 7, ]

View(Cluster1_Instances)
View(Cluster2_Instances)


newData<-as.data.frame(Brand_normData_1$No..of.Brands)
newData<-cbind(newData,km_brand_1$cluster)

View(newData)

# Plotting box plot to see the distribution of each variable in different clusters
boxplot(Brand_normData_1$No..of.Brands~km_brand_1$cluster, ylab="No of Brands", xlab="Clusters")
boxplot(Brand_normData_1$Brand.Runs~km_brand_1$cluster, ylab="Brand Runs", xlab="Clusters")
boxplot(Brand_normData_1$Total.Volume~km_brand_1$cluster, ylab="Total Volume", xlab="Clusters")
boxplot(Brand_normData_1$No..of..Trans~km_brand_1$cluster, ylab="No of Transactions", xlab="Clusters")
boxplot(Brand_normData_1$Value~km_brand_1$cluster, ylab="Value", xlab="Clusters")
boxplot(Brand_normData_1$Avg..Price~km_brand_1$cluster, ylab="Average Price", xlab="Clusters")
boxplot(Brand_normData_1$Others.999~km_brand_1$cluster, ylab="Others99", xlab="Clusters")
boxplot(Brand_normData_1$max~km_brand_1$cluster, ylab="Max to one brand", xlab="Clusters")

############# Removing unimportant & using only required important variable ###############
View(Brand_normData_1)
Brand_normData_2<-Brand_normData_1[,c(1,2,3,5,7,8)]
View(Brand_normData_2)

wss_2 <- (nrow(Brand_normData_2)-1)*sum(apply(Brand_normData_2,2,var))
wss_2


for (i in 2:15) wss_2[i] <- sum(kmeans(Brand_normData_2, centers=i)$withinss)
plot(1:15, wss_2, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", 
     pch=20, cex=2)

#Trying k-means clustering for different values of k
set.seed(7)
km_brand_1 = kmeans(Brand_normData_2, 4, nstart=100)
km_brand_1

km_brand_1 = kmeans(Brand_normData_2, 5, nstart=100)
km_brand_1

km_brand_1 = kmeans(Brand_normData_2, 6, nstart=100)
km_brand_1

km_brand_1 = kmeans(Brand_normData_2, 7, nstart=100)
km_brand_1

km_brand_1$tot.withinss/km_brand_1$totss

#########################1.b##########################################

View(BathSoap_Data)
sellProp_data<-BathSoap_Data[,c(14,20,21,22,32:46)]
View(sellProp_data)
dim(sellProp_data)

# Calculating volume(by multiplying by total volume)
vol <- function(x){
  return(x*sellProp_data$`Total Volume`)
}

sellProp_vol_Data<-as.data.frame(lapply(sellProp_data[2:19],vol))
View(sellProp_vol_Data)
dim(sellProp_vol_Data)

#Normalising the above data
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
sellProp_normData <- as.data.frame(lapply(sellProp_vol_Data, normalize))
View(sellProp_normData)
dim(sellProp_normData)

#Scree Plot - Check for the optimal number of clusters given the data
sellProp_normData_1 <- sellProp_normData
wss <- (nrow(sellProp_normData_1)-1)*sum(apply(sellProp_normData_1,2,var))
wss

for (i in 2:15) { set.seed(7)
  wss[i] <- sum(kmeans(sellProp_normData_1, 
                       centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Running clustering model k=
set.seed(7)
km_4 = kmeans(sellProp_normData, 4, nstart =100)
km_4

km_4 = kmeans(sellProp_normData, 5, nstart =100)
km_4

km_4 = kmeans(sellProp_normData, 6, nstart =100)
km_4

km_4 = kmeans(sellProp_normData, 7, nstart =100)
km_4

km_4 = kmeans(sellProp_normData, 8, nstart =100)
km_4

Ratio <- km_4$withinss/km_4$betweenss
Ratio
###############################################
count1=0
counting <- function(x){
  
  
  which(x>0.25)
  
  return(count1) 
  
}

lapply(sp_data[12],counting)

count1
#9 10 11 12 13 15
#################################remove 9 10 11 12 13 15#######################

set.seed(7)
sp_data_final<-sellProp_data[,c(1:11,17)]

View(sp_data_final)

wss <- (nrow(sp_data_final)-1)*sum(apply(sp_data_final,2,var))
wss

for (i in 2:15) 
{set.seed(7)
  wss[i] <- sum(kmeans(sp_data_final, centers=i)$withinss)}
plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", 
     pch=20, cex=2)


set.seed(7)
km_sp_2 = kmeans(sp_data_final, , nstart =100)
km_sp_2

km_sp_1


######################### Part c #########################
Both_Data <- Brand_final[,c(1:5,8,20,21)] #Brand_normData_1[,c(1,2,3,5,7,8)] # Both_Data <- Brand_final[,c(1:3,5,20,21)]
View(Both_Data)

Both_Data <- cbind(Both_Data, sp_data_final)
View(Both_Data)
dim(Both_Data)

# Calculating volume(by multiplying by total volume)
vol <- function(x){
  return(x*Both_Data$`Total Volume`)
}

Both_vol_Data<-as.data.frame(lapply(Both_Data[10:20],vol))
View(Both_vol_Data)

Both_final <- Both_Data[,1:8]
Both_final <- cbind(Both_final,Both_vol_Data)
View(Both_final)

#Normalising the above data
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
Both_normData <- as.data.frame(lapply(Both_final, normalize))
View(Both_normData)

View(Brand_normData)


km_Brand_4 = kmeans(Brand_normData, 2, nstart =100)
km_Brand_4

Both_Data$cluster
Both_Data$centers
Both_Data$withinss
Both_Data$betweenss
Both_Data$size
Both_Data$iter

# Plot results
plot(Both_Data, col =(km_Brand_4$cluster) , 
     main="K-Means with 2 clusters", 
     pch=30, cex=2)

#Scree Plot - Check for the optimal number of clusters given the data
Both_Data_1 <- Both_Data
wss <- (nrow(Both_Data_1)-1)*sum(apply(Both_Data_1,2,var))
wss

for (i in 2:15) 
  wss[i] <- sum(kmeans(Both_Data_1, 
                       centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Assessing the above Scree Plot we found that no. of cluster = 7 is the optimum point
# Thus, we will run the K-means clustering algorithm again using k=7
set.seed(7)
km_Brand_5 = kmeans(Both_Data_1, 7, nstart=100)

# Examine the result of the clustering algorithm
km_Brand_5

# Plot results
col =(km_Brand_5$cluster +1)
plot(Both_Data_1, col = col, main="K-Means result with 7 clusters",
     pch=20, cex=2)
points(km_Brand_5$centers, col=col, pch=19, cex=2)