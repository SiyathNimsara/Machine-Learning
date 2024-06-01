# Load necessary libraries
library(readxl)

# Read data
data <- read_excel("C:/Users/DELL/Desktop/ML_CW/Whitewine_v6.xlsx")


# Scale data
data_scaled <- scale(data[, 1:11])

# Detect and remove outliers
outlier_threshold <- 3
outliers <- apply(data_scaled, 1, function(x) any(abs(x) > outlier_threshold))
data_clean <- data_scaled[!outliers, ]

# Now data_clean is ready for clustering

boxplot(data_clean)
boxplot(data_scaled)
dim(data_clean)
dim(data_scaled)

# Install and load necessary libraries
if (!require("factoextra")) install.packages("factoextra")
if (!require("NbClust")) install.packages("NbClust")
if (!require("cluster")) install.packages("cluster")
if (!require("ggplot2")) install.packages("ggplot2")
library(factoextra)
library(NbClust)
library(cluster)
library(ggplot2)

# Elbow Method
set.seed(123)
wss <- sapply(1:15, function(k) { kmeans(data_clean, k, nstart = 20)$tot.withinss })
plot(1:15, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K", ylab = "Total within-cluster sum of squares")

# Silhouette Method
sil_width <- sapply(2:15, function(k) {
  model <- kmeans(data_clean, k, nstart = 20)
  silhouette <- silhouette(model$cluster, dist(data_clean))
  mean(silhouette[, 3])
})
plot(2:15, sil_width, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters", ylab = "Average silhouette width")

# Gap Statistic
set.seed(123)
gap_stat <- clusGap(data_clean, FUN = kmeans, K.max = 15, B = 50)
fviz_gap_stat(gap_stat)

# NbClust
nb <- NbClust(data_clean, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")
barplot(table(nb$Best.nc[1, ]),
        xlab = "Number of clusters", ylab = "Number of criteria",
        main = "Number of clusters chosen by different criteria")




# Assuming the optimal number of clusters k is 2
optimal_k <- 2
set.seed(123)  # for reproducibility

# Performing k-means clustering
kmeans_result <- kmeans(data_clean, centers = optimal_k, nstart = 25)

# Viewing the results
print(kmeans_result)

# The cluster centers
cat("Cluster Centers:\n")
print(kmeans_result$centers)

# The size of each cluster
cat("Cluster Sizes:\n")
print(kmeans_result$size)

# Total within-cluster sum of squares (WSS)
cat("Total Within-cluster Sum of Squares (WSS):\n")
print(kmeans_result$tot.withinss)

# Calculating total sum of squares (TSS)
total_mean <- colMeans(data_clean)
TSS <- sum(sapply(1:nrow(data_clean), function(i) sum((data_clean[i, ] - total_mean)^2)))

# Between-cluster sum of squares (BSS)
BSS <- TSS - kmeans_result$tot.withinss

# Ratio of BSS over TSS
ratio_BSS_TSS <- BSS / TSS
cat("Ratio of Between-cluster Sum of Squares (BSS) over Total Sum of Squares (TSS):\n")
print(ratio_BSS_TSS)

# Plotting the clusters
fviz_cluster(kmeans_result, data = data_clean)

# Load necessary library if not already loaded
if (!require("cluster")) install.packages("cluster")
library(cluster)

# Compute silhouette widths
silhouette_widths <- silhouette(kmeans_result$cluster, dist(data_clean))

# Plot the silhouette
plot(silhouette_widths, col = 1:optimal_k, border = NA, main = "Silhouette Plot")

# Calculate the average silhouette score
average_silhouette_score <- mean(silhouette_widths[, "sil_width"])
cat("Average Silhouette Width: ", average_silhouette_score, "\n")


# Perform PCA
pca_result <- prcomp(data_clean, scale. = TRUE)

# Visualize the PCs 
fviz_eig(pca_result, addlabels = TRUE)
fviz_pca_var(pca_result, col.var = "black")

# View summary of PCA results
summary(pca_result)


# Eigenvalues
cat("Eigenvalues:\n")
print(pca_result$sdev^2)  # Printing the variance explained by each principal component

# Eigenvectors (loadings)
cat("Eigenvectors (Loadings):\n")
print(pca_result$rotation)

# Calculate and plot cumulative variance
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
plot(cumulative_variance, xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = 'b', main = "Cumulative Variance Explained by PCs")

# Find number of components to explain at least 85% of variance
num_components <- which(cumulative_variance >= 0.85)[1]
cat("Number of components to explain at least 85% variance: ", num_components, "\n")

# New dataset with selected principal components
transformed_data <- data.frame(pca_result$x[, 1:5])

print(transformed_data)


# Compute total within-cluster sum of squares for a range of cluster numbers
wss <- sapply(1:15, function(k) {
  set.seed(123)
  kmeans(transformed_data, centers = k, nstart = 25)$tot.withinss
})

# Plot the elbow curve
plot(1:15, wss, type = "b", pch = 19, col = "blue", xlab = "Number of Clusters", ylab = "Total within-cluster sum of squares", main = "Elbow Method")


# Calculate average silhouette width for a range of clusters
sil_width <- sapply(2:15, function(k) {
  model <- kmeans(transformed_data, centers = k, nstart = 25)
  silhouette(model$cluster, dist(transformed_data))[, "sil_width"] |> mean()
})

# Plot silhouette scores
plot(2:15, sil_width, type = "b", pch = 19, col = "red", xlab = "Number of Clusters", ylab = "Average Silhouette Width", main = "Silhouette Method")


# Load and use the cluster library
library(cluster)
set.seed(123)
gap_stat <- clusGap(transformed_data, FUN = kmeans, K.max = 15, B = 50)
plot(gap_stat, main = "Gap Statistic")


# Load and use the NbClust library
if (!require("NbClust")) install.packages("NbClust")
library(NbClust)
nb <- NbClust(transformed_data, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")
barplot(table(nb$Best.nc[1, ]), xlab = "Number of clusters", ylab = "Number of criteria met", main = "NbClust Results")

# Set a seed for reproducibility
set.seed(123)

# Perform k-means clustering
kmeans_result <- kmeans(transformed_data, centers = 4, nstart = 25)

# Print cluster assignments
print(kmeans_result$cluster)

library(ggplot2)
transformed_data <- as.data.frame(transformed_data)
transformed_data$cluster <- as.factor(kmeans_result$cluster)

ggplot(transformed_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.5) +
  labs(title = "Cluster Plot of PCA-transformed Data",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# Assuming the optimal number of clusters
optimal_k <- 2
set.seed(123)  # for reproducibility

# Perform k-means clustering
kmeans_result <- kmeans(transformed_data, centers = optimal_k, nstart = 25)

# Viewing the results
print(kmeans_result)

# The cluster centers
cat("Cluster Centers:\n")
print(kmeans_result$centers)

# Total within-cluster sum of squares (WSS)
cat("Total Within-cluster Sum of Squares (WSS):\n")
print(kmeans_result$tot.withinss)

# Ensure all data in transformed_data is numeric for calculation
numeric_transformed_data <- transformed_data[, sapply(transformed_data, is.numeric)]

# Calculate total mean for numeric data only
total_mean <- colMeans(numeric_transformed_data)

# Calculate total sum of squares (TSS)
TSS <- sum(sapply(1:nrow(numeric_transformed_data), function(i) sum((numeric_transformed_data[i, ] - total_mean)^2)))

# Between-cluster sum of squares (BSS)
BSS <- TSS - kmeans_result$tot.withinss

# Ratio of BSS over TSS
ratio_BSS_TSS <- BSS / TSS
cat("Ratio of Between-cluster Sum of Squares (BSS) over Total Sum of Squares (TSS):\n")
print(ratio_BSS_TSS)

# If you want to visualize clusters, ensure that the data passed to fviz_cluster is numeric
library(factoextra)
fviz_cluster(kmeans_result, data = numeric_transformed_data, geom = "point", stand = FALSE, ellipse.type = "norm")


# Compute silhouette widths
silhouette_widths <- silhouette(kmeans_result$cluster, dist(transformed_data))

# Plot the silhouette
plot(silhouette_widths, col = 1:optimal_k, border = NA, main = "Silhouette Plot for PCA-based Clusters")

# Calculate the average silhouette score
average_silhouette_score <- mean(silhouette_widths[, "sil_width"])
cat("Average Silhouette Width: ", average_silhouette_score, "\n")


if (!require("fpc")) install.packages("fpc")
library(fpc)

# Distance matrix of the PCA-transformed data
dist_matrix <- dist(transformed_data)

# Cluster assignments from k-means result
cluster_assignment <- kmeans_result$cluster

# Calculate the Calinski-Harabasz Index
ch_index <- cluster.stats(dist_matrix, cluster_assignment)$ch
cat("Calinski-Harabasz Index: ", ch_index, "\n")


