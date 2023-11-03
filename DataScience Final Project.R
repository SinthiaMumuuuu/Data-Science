
hdp <- read.csv("C:/Users/HP/Downloads/heart_disease_patients.csv", header = TRUE)
# Viewing the first few rows of the dataset
head(hdp)
# Checking the summary of the dataset
summary(hdp)
# Checking the dimensions of the dataset
dim(hdp)
# Checking the structure of dataset
str(hdp)


# Removing missing values
heart_data <- na.omit(hdp)
# removing ID column
heart_data <- hdp[, -1]
heart_data
# Selecting relevant columns for clustering
hdp_subset <- hdp[,c(2,5,6,9)]
hdp_subset

# scaling the variables
hdp_scaled<- scale(hdp_subset)
hdp_scaled


library(factoextra)

# Estimating optimal number of clusters
fviz_nbclust(hdp_scaled, kmeans, method = "wss")

# K-means clustering with 3 clusters
kmeans_model <- kmeans(hdp_scaled, centers = 3, nstart = 25)
kmeans_model

#Visualizing the output of K-means Clustering Algorithm
fviz_cluster(kmeans_model, data = hdp_scaled )

aggregate(hdp_scaled, by = list(cluster = kmeans_result$cluster), mean)

# Loading the required packages
library(ggplot2)
library(gridExtra)

# Creating a scatterplot matrix of the clusters
p1 <- ggplot(heart_clusters, aes_string(x = "age", y = "trestbps", color = "cluster")) +
  geom_point() +
  labs(title = "Age vs. Resting Blood Pressure")

p2 <- ggplot(heart_clusters, aes_string(x = "chol", y = "thalach", color = "cluster")) +
  geom_point() +
  labs(title = "Cholesterol vs. Max Heart Rate")

grid.arrange(p1, p2, ncol = 2)
