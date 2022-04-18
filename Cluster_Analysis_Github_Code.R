
# required packages

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(ggplot2)
library(ggExtra)
library(ggsci)

# import the data
data <- read.csv("data_filename_here.csv", row.names = 1)

#create a subset for cluster analysis
#containing only the d13C and d15N values
cluster.data <- subset(data, select = c(d15N, d13C))

# scale the data so the clustering algorithm doesn't depend on an arbitrary variable unit

cluster.data<- scale(cluster.data)
head(cluster.data)

#Agglomerative Hierarchical Clustering

#four different methods: complete, average, single and ward.D

#Complete Linkage Method
# Dissimilarity matrix
d.matrix <- dist(cluster.data, method="euclidean")

data.hc1 <- hclust(d.matrix, method="complete")

#plot the obtained dendrogram
plot(data.hc1, cex=0.6, hang=-1)

# compute with agnes, to get the agglomerative coefficient (ac)
# ac measures the amount of clustering structure found, with values closer to 1 = stronger clustering structure

data.hc2 <- agnes(cluster.data, method="complete")

#agglomerative coefficient
data.hc2$ac

#assess the different methods
m <- c("average","single","complete","ward")
names(m) <- c("average","single","complete","ward")

ac <- function(x){
  agnes(cluster.data, method=x)$ac
}

map_dbl(m,ac)

# choose the method with the agglomerative coefficient which is closest to 1
# this method has the strongest clustering structures
# in this case, Ward's method had the greatest agglomerative coefficient

data.hc3 <- agnes(cluster.data, method="ward")
pltree(data.hc3, cex=0.6, hang=-1, main = "Dendrogram of agnes, ward method")

# cutting the dendrogram

data.hc4 <- hclust(d.matrix, method="ward.D2")
sub_grp <- cutree(datahc4, k = 2)

#number of members in each cluster
table(sub_grp)

#draw the dendrogram with a border around the two clusters

png("Filename_Output_Here.png", units = "in", width = 7, height = 5.5, res = 300 )

plot(data.hc4, cex=0.3)
rect.hclust(data.hc4, k = 2, border = 2:5)

dev.off()

# visualise in a scatterplot

library(factoextra)

png("Scatterplot.png", units = "in", width = 7, height = 5.5, res = 300 )

fviz_cluster(list(data=cluster.data, cluster=sub_grp))+
  theme_classic()

dev.off()

#add a column to the dataframe for the cluster
data$cluster <- cutree(data.hc4, k=2)

#create a plot showing the cluster by colour

data$cluster <- as.factor(data$cluster)
data$Year_Group <- as.factor(data$Year_Group)

# Kruskal-Wallis test -----------------------------------------------------

# check the clusters are significantly different 

kruskal.test(d13C_Suess ~ cluster, data = maui.data)
kruskal.test(d15N ~ cluster, data = maui.data)

#create a dataframe with cluster membership added to each row

write.csv(data, "filepath_here", row.names = TRUE)








