# Homework exercises

library(data.table)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(magrittr)
library(gridExtra)
library(fossil)
library(pheatmap)
library(mclust)
library(factoextra)

# section 04
# 1
# Cluster: - X1: A, B, D
#          - X2: C, E
# New centroids: - 1: (7.66, 6.66)
#                - 2: (5, 5)

# create dt
dt = data.table(
  letter = c('A', 'B', 'C', 'D', 'E', 'X1', 'X2'),
  x = c(8, 10, 4, 5, 6, 6, 3),
  y = c(5, 7, 6, 8, 4, 9, 3)
)

# distances to current centroids
dt[, deltax1:=(x-dt[letter=='X1', x])**2 + (y-dt[letter=='X1', y])**2]
dt[, deltax2:=(x-dt[letter=='X2', x])**2 + (y-dt[letter=='X2', y])**2]

# get closest centroids
dt[, cluster:=ifelse(deltax1<deltax2, 'X1', 'X2')]

#compute new centroids
dt[(letter != 'X1') & (letter != 'X2'), .(mean(x), mean(y)), by=cluster]

# 2
iris_dt <- as.data.table(iris)[, 1:4]
iris_dt_scaled <- scale(iris_dt)

clust_iris <- kmeans(iris_dt, 3)
clust_iris_scaled <- kmeans(iris_dt_scaled, 3)

clusters_iris <- clust_iris$cluster
centers_iris <- clust_iris$center
clusters_iris_scaled <- clust_iris_scaled$cluster
centers_iris_scaled <- clust_iris_scaled$center

# 3
iris_matrix <- as.matrix(iris_dt)
iris_heatmap_cluster <- pheatmap(iris_matrix,
                                 cluster_rows = TRUE,
                                 cluster_cols = FALSE,
                                 clustering_method = "complete",
                                 scale = "column")
iris_clusters_3 <- cutree(iris_heatmap_cluster$tree_row, k = 3)

comparison <- table(clusters_iris_scaled, iris_clusters_3)

# 4
iris_dt <- as.data.table(iris)
rownames(iris_matrix) <- 1:nrow(iris_dt)
iris_dt[, clustered := as.factor(iris_clusters_3)]
iris_dt[, k_means := as.factor(clusters_iris_scaled)]
species_labels <- iris_dt[, .(Species, clustered, k_means)]
iris_heatmap_cluster_annot <- pheatmap(iris_matrix,
                                       cluster_rows = T,
                                       cluster_cols = F,
                                       clustering_method = "complete",
                                       scale = "column",
                                       annotation_row = species_labels,
                                       show_rownames = F)

# ---

# section 05
# 1
iris_dt <- as.data.table(iris)
iris_dt[, clustered := as.factor(iris_clusters_3)]
iris_dt[, k_means := as.factor(clusters_iris_scaled)]
all_clusters <- iris_dt[, .(as.numeric(Species), clustered, k_means)]
rand_ind_scaled_all <- apply(all_clusters, 2, function(column_i) apply(all_clusters, 2, function(column_j) rand.index(as.numeric(column_i), as.numeric(column_j))))

# with for loop
for (i in 1:3) {
  for (j in 1:3) {
    rand.index(all_clusters[,..i], all_clusters[,..j])
  }
}

# 2 TODO


# ---

# section 06
# 1
iris_prep <- as.data.table(iris)[, 1:4]
iris_pca <- prcomp(iris_prep, center = TRUE, scale. = TRUE)

# 2
summary_pca <- summary(iris_pca)
# PC1: 51,46%
# PC2: 25,55%
# PC3: 16,70%
# PC4: 6,287%

# 3
prediction <- predict(iris_pca)
plot_pred <- biplot(iris_pca)
# plot_pred_pretty <- fviz_pca_biplot(iris_pca)

# 4 TODO
pca_data <- cbind(iris_prep, as.data.table(predict(iris_pca)))
pca_data_melted <- melt(pca_data,
                        id.vars=c("PC1", "PC2", "PC3", "PC4"))

all_together <- ggplot(pca_data_melted, aes(x=PC1, y=value)) +
                  geom_point() +
                  facet_wrap(~variable, scales='free')
# scales='free': different scales for axis possible

# 5
pca_data <- cbind(as.data.table(iris), as.data.table(predict(iris_pca)))
final_plot <- ggplot(pca_data, aes(x=PC1, y=PC2, color=Species)) +
                geom_point()

# data <- replicate(100, rnorm(100))
# pca <- prcomp(data)
# raw <- pca$x[,1:2]
# plot(raw[,1], raw[,2], col=rainbow(nrow(raw)), pch=20)
