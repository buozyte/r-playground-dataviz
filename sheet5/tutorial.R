# Tutorial exercises

# HINT: silent=TRUE supresses plots :)

# section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(GGally)
library(pheatmap)
library(mclust)

# ---

# section 01
expr <- readRDS("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/cancer_data.rds") %>% as.data.table(keep.rownames = "tumor_type")
head(expr[, 1:6])
# 1
correlation <- ggcorr(expr[, -"tumor_type"])
# correlation between FUK and UPG2 is way too high ...

# 2
expr_matrix <- as.matrix(expr[, -"tumor_type"])
rownames(expr_matrix) <- expr[, tumor_type]     # add rownames, not neccessary, but nice :)
heatmap <- pheatmap(expr_matrix,
                    cluster_rows = FALSE,
                    cluster_cols = FALSE)
heatmap_scaled <- pheatmap(expr_matrix,
                           cluster_rows = FALSE,
                           cluster_cols = FALSE,
                           scale = 'column')
# the values for DOHH2 in FUK and UGP2 seem to be kind of outliers
#   they are at least really high compared to the rest
# see 3 :)

# 3
# from bioinformatics: values should never be above 5 or 6, so 100 is impossible
melted <- melt(expr,
               id.vars = 'tumor_type')
largest_values <- melted[order(-value)] %>% head()
# conclusion: largest value should be 5.876502 and all values have decimals behind comma!

histogram_all <- ggplot(melted, aes(x=value)) +
                  geom_histogram() +
                  facet_wrap(~variable)
histogram <- ggplot(melted[variable %in% c("FUK", "UGP2")], aes(x=value)) +
              geom_histogram() +
              facet_wrap(~variable)
scatter_outliers <- ggplot(expr, aes(x=FUK, y=UGP2)) +
                      geom_point()

# remove outliers
expr[tumor_type == "DOHH2", `:=` (FUK=NA, UGP2=NA)]

expr_matrix <- as.matrix(expr[, -"tumor_type"])
rownames(expr_matrix) <- expr[, tumor_type]
new_heatmap <- pheatmap(expr_matrix,
                        cluster_rows = FALSE,
                        cluster_cols = FALSE)

# ---

# section 02
# 1
iris_dt <- as.data.table(iris)
iris_matrix <- as.matrix(iris_dt[, -"Species"])
iris_heatmap <- pheatmap(iris_matrix,
                         cluster_rows = FALSE,
                         cluster_cols = FALSE,
                         scale = "column")

# 2
iris_heatmap_cluster <- pheatmap(iris_matrix,
                                 cluster_rows = TRUE,
                                 cluster_cols = FALSE,
                                 clustering_method = "complete",
                                 scale = "column")

# 3
# alternative:
# cutree(hclust(dist(iris_matrix)), k=3)
# official solution:
iris_clusters_3 <- cutree(iris_heatmap_cluster$tree_row, k = 3)

# 4
rownames(iris_matrix) <- 1:nrow(iris_dt)
iris_dt[, clustered := as.factor(iris_clusters_3)]
species_labels <- iris_dt[, .(Species, clustered)]
iris_heatmap_cluster_annot <- pheatmap(iris_matrix,
                                       cluster_rows = TRUE,
                                       cluster_cols = FALSE,
                                       clustering_method = "complete",
                                       scale = "column",
                                       annotation_row = species_labels,
                                       show_rownames = FALSE)
# clustering is ok, but could be better :)

# ---

# section 03
# 1
# (3+8)/(30/2)