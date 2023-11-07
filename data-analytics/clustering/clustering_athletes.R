# this code is largely based on https://github.com/bkrai/Top-10-Machine-Learning-Methods-With-R/blob/master/ClusterAnalysis
# run preprocessing_athlete_data first to perform cluster analysis on data from https://www.kaggle.com/datasets/shashwatwork/injury-prediction-for-competitive-runners
library(cluster) 
library(viridis)
library(factoextra)
library(ggpubr)

# Cluster Analysis
mydata <- athelete_data_means.cluster_subset
#mydata <- athelete_data_means.cluster_subset[,c(3,5,6,13)]
str(mydata)
head(mydata)
summary(mydata)
pairs(mydata)

# Scatter plot 
plot(mydata$`total kms`~ mydata$`max recovery`, data = mydata)
with(mydata,text(mydata$`total kms` ~ mydata$`max recovery`, labels=athelete_data_means$`Athlete ID`,pos=4))

# Normalize 
z <- mydata
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)

# Calculate distance matrix  
distance = dist(nor)

# Hierarchical agglomerative clustering  
mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Company,main='Default from hclust')
plot(mydata.hclust,hang=-1)

# Hierarchical agglomerative clustering using "average" linkage 
#mydata.hclust<-hclust(distance,method="average")
#plot(mydata.hclust,hang=-1)

# Cluster membership, cut by height
member = cutree(mydata.hclust,h=8)
table(member)
# Cluster membership, cut by number of clusters
member = cutree(mydata.hclust,k=3)
table(member)

# Characterizing clusters 
aggregate(nor,list(member),mean)
aggregate(mydata,list(member),mean)

# Silhouette Plot

plot(silhouette(cutree(mydata.hclust,k=3), distance)) 

# Scree Plot
#wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
#for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
#plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 


fviz_nbclust(nor, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(nor, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


fviz_nbclust(nor, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# K-means clustering
set.seed(123)
kc<-kmeans(nor,3)

# average total kms and average max recovery
plot(mydata$`total kms`~ mydata$`max recovery`, data = mydata, col=kc$cluster)
with(mydata,text(mydata$`total kms` ~ mydata$`max recovery`, labels=athelete_data_means$`Athlete ID`,pos=4))

plot(mydata$`total kms`~ mydata$`nr. rest days`, data = mydata, col=kc$cluster)
with(mydata,text(mydata$`total kms` ~ mydata$`nr. rest days`, labels=athelete_data_means$`Athlete ID`,pos=4))


plot(mydata$`total kms`~ mydata$`nr. rest days`, data = mydata, col=athelete_data_means$injury)
with(mydata,text(mydata$`total kms` ~ mydata$`nr. rest days`, labels=athelete_data_means$`Athlete ID`,pos=4))

# average total kms and average hours of alternative training, less alternative training means more injuries
plot(mydata$`total kms`~ mydata$`total hours alternative training`, data = mydata, col=athelete_data_means$injury)
with(mydata,text(mydata$`total kms` ~ mydata$`total hours alternative training`, labels=athelete_data_means$`Athlete ID`,pos=4))

# average total kms and average hours of alternative training, less alternative training means more injuries
plot(mydata$`avg exertion`~ mydata$`avg recovery`, data = mydata, col=athelete_data_means$injury)
with(mydata,text(mydata$`avg exertion`~ mydata$`avg recovery`, labels=athelete_data_means$`Athlete ID`,pos=4))

plot(mydata$`avg exertion`~ mydata$`avg recovery`, data = mydata, col=kc$cluster)
with(mydata,text(mydata$`avg exertion`~ mydata$`avg recovery`, labels=athelete_data_means$`Athlete ID`,pos=4))

# visualise cluster means
cluster_means <- aggregate(mydata,list(member),mean)
cluster_means.long <- gather(cluster_means,var,val,`total kms`:num_injuries)
ggplot(cluster_means.long, aes(fill=var, y=val, x=Group.1)) + 
  geom_bar(position='dodge', stat='identity') +
  theme_minimal() +
  labs(x = "Cluster", y = "Variable mean") +
  scale_fill_viridis_d()


mydata.boxplot <- mydata
mydata.boxplot$cluster <- member


my_comparisons <- list( c("1", "2"), c("1", "3"), c("2", "3") )
ggboxplot(mydata.boxplot, x = "cluster", y = "num_injuries",
          color = "cluster")+ 
  stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
  stat_compare_means(label.y = 45)

my_comparisons <- list( c("1", "2"), c("1", "3"), c("2", "3") )
ggboxplot(mydata.boxplot, x = "cluster", y = "total hours alternative training",
          color = "cluster")+ 
  stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
  stat_compare_means(label.y = 45)

my_comparisons <- list( c("1", "2"), c("1", "3"), c("2", "3") )
ggboxplot(mydata.boxplot, x = "cluster", y = "avg exertion",
          color = "cluster")+ 
  stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
  stat_compare_means(label.y = 45)


# MDS
mds.coor <- cmdscale(distance)
plot(mds.coor[,1], mds.coor[,2], type="n", xlab="", ylab="")
text(jitter(mds.coor[,1]), jitter(mds.coor[,2]),
     athelete_data_means$`Athlete ID`, cex=0.8)
abline(h=0,v=0,col="gray75")
