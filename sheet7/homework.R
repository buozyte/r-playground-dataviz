# Homework exercises

library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork)
library(gridExtra)

genotype <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/genotype.txt")
genotype <- melt(genotype, id.vars = "strain", variable.name = "marker",
                 value.name = "genotype")
growth <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/growth.txt")
growth <- melt(growth, id.vars = "strain", variable.name = "media",
               value.name = "growth_rate")
marker <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/marker.txt")

# Plotting the growth rate difference
getMaltoseDt = function(mrk){
  growth_mrk <- merge(growth, genotype[marker %in% mrk, .(strain, genotype, marker)],
                      by = 'strain', allow.cartesian = TRUE)
  growth_mrk[media == "YPMalt"]
}

# Boxplot
plot_growth_one_mk <- function(mk){
  ggplot(getMaltoseDt(mk), aes(genotype, growth_rate)) +
    geom_boxplot() +
    labs(title = mk) + theme_bw(base_size = 16)
}
box_5211 <- plot_growth_one_mk("mrk_5211")

# Function to calculate the difference of the medians of two genotypes
median_diff <- function(dt){
  dt[genotype == 'Wild isolate', median(growth_rate, na.rm=T)] -
    dt[genotype == 'Lab strain', median(growth_rate, na.rm=T)]
}

# Function to permute the table, plot the resulting histogram # and compute a p-value
p_val_medians <- function(dt, N_permu = 1000){
  # It will return both a pvalue and plot a histogram of T_star
  T_ref <- median_diff(dt)
  T_star <- sapply(1:N_permu,
                   function(x){
                     median_diff(dt[, genotype := sample(genotype)])
                   }
  )
  # Plot
  g <- ggplot(data = data.table(T_star = T_star), aes(T_star)) +
    geom_histogram() +
    geom_vline(aes(xintercept=T_ref, color="T_ref")) +
    xlim(-3,3)
  
  # Compute and return the p value
  # First compute each tail seperately
  p_val_right <- (sum(T_star >= T_ref) + 1) / (N_permu + 1)
  p_val_left <- (sum(T_star <= T_ref) + 1) / (N_permu + 1)
  # Then combine the above to obtain the double sided p-value.
  p_val <- 2 * min(p_val_right, p_val_left)
  return(list(p_val, g))
}

# section 03
# correct idea, but I could have just used the mean
#   instead of plotting both values heh

# 1
# H0: marker 5091 is not associated with the growth if conditioned on
#     marker 5211.
mks_geno <- genotype[marker %in% c("mrk_5091", "mrk_5211")] %>% spread(marker, genotype)
by_5211 <- left_join(getMaltoseDt("mrk_5091"),
                     mks_geno,
                     c("strain" = "strain", "genotype" = "mrk_5091"))

median_diff_by_5211 <- function(dt){
  t1 <- dt[genotype == 'Wild isolate', median(growth_rate, na.rm=T), by="mrk_5211"]
  t2 <- dt[genotype == 'Lab strain', median(growth_rate, na.rm=T), by="mrk_5211"]
  t1[order(mrk_5211)][,V1] - t2[order(mrk_5211)][,V1]
}
p_val_medians_by_5211 <- function(dt, N_permu = 1000){
  # It will return both a pvalue and plot a histogram of T_star
  T_ref <- median_diff_by_5211(dt)
  T_star <- sapply(1:N_permu,
                   function(x){
                     median_diff_by_5211(dt[, genotype:=sample(genotype)])
                   }
  )
  # Plot
  g1 <- ggplot(data = data.table((T_star[1,] = T_star[1,])), aes(V1)) +
    geom_histogram() +
    geom_vline(aes(xintercept=T_ref[1], color="T_ref")) +
    xlim(-3,3)
  g2 <- ggplot(data = data.table((T_star[2,] = T_star[2,])), aes(V1)) +
    geom_histogram() +
    geom_vline(aes(xintercept=T_ref[2], color="T_ref")) +
    xlim(-3,3)
  
  # Compute and return the p value
  # First compute each tail seperately
  p_val_right <- (sum(T_star >= T_ref) + 1) / (N_permu + 1)
  p_val_left <- (sum(T_star <= T_ref) + 1) / (N_permu + 1)
  # Then combine the above to obtain the double sided p-value.
  p_val <- 2 * min(p_val_right, p_val_left)
  return(list(p_val, g1, g2))
}

# 2
by_5091 <- left_join(getMaltoseDt("mrk_5211"),
                     mks_geno,
                     c("strain" = "strain", "genotype" = "mrk_5211"))

median_diff_by_5091 <- function(dt){
  t1 <- dt[genotype == 'Wild isolate', median(growth_rate, na.rm=T), by="mrk_5091"]
  t2 <- dt[genotype == 'Lab strain', median(growth_rate, na.rm=T), by="mrk_5091"]
  t1[order(mrk_5091)][,V1] - t2[order(mrk_5091)][,V1]
}
p_val_medians_by_5091 <- function(dt, N_permu = 1000){
  # It will return both a pvalue and plot a histogram of T_star
  T_ref <- median_diff_by_5091(dt)
  T_star <- sapply(1:N_permu,
                   function(x){
                     median_diff_by_5091(dt[, genotype:=sample(genotype)])
                   }
  )
  # Plot
  g1 <- ggplot(data = data.table((T_star[1,] = T_star[1,])), aes(V1)) +
    geom_histogram() +
    geom_vline(aes(xintercept=T_ref[1], color="T_ref")) +
    xlim(-3,3)
  g2 <- ggplot(data = data.table((T_star[2,] = T_star[2,])), aes(V1)) +
    geom_histogram() +
    geom_vline(aes(xintercept=T_ref[2], color="T_ref")) +
    xlim(-3,3)
  
  # Compute and return the p value
  # First compute each tail seperately
  p_val_right <- (sum(T_star >= T_ref) + 1) / (N_permu + 1)
  p_val_left <- (sum(T_star <= T_ref) + 1) / (N_permu + 1)
  # Then combine the above to obtain the double sided p-value.
  p_val <- 2 * min(p_val_right, p_val_left)
  return(list(p_val, g1, g2))
}

p_5211 <- p_val_medians_by_5211(by_5211)
p_5091 <- p_val_medians_by_5091(by_5091)
res_3 <- grid.arrange(p_5211[[2]], p_5211[[3]],
                      p_5091[[2]], p_5091[[3]],
                      ncol=2, nrow=2)

# ---

# section 04
# 1
# function from tutorials but a bit switched up :)
p_conf_int <- function(dt, N_permu = 1000){
  # sampled kinda wrong :(
  T_star <- sapply(1:N_permu,
                   function(x){
                     median_diff(dt[, genotype := sample(genotype)])
                   }
  )
  boot <- lapply(1:N_permu,
                 function(i){
                   sample(T_star, N_permu, replace = TRUE)
                   }
  )
  sample_means <- sapply(boot, mean)
  conf_int = quantile(sample_means, c(0.025, 0.975))
  # Plot
  g <- ggplot(data = data.table(means=sample_means), aes(x=means)) +
    geom_histogram() +
    geom_vline(aes(xintercept=mean(T_star), color="observed")) +
    geom_vline(aes(xintercept=conf_int[1], color="CI"), linetype="dashed") +
    geom_vline(aes(xintercept=conf_int[2], color="CI"), linetype="dashed")

  return(g)
}

conf_int_5211 <- p_conf_int(getMaltoseDt("mrk_5211"))
conf_int_5091 <- p_conf_int(getMaltoseDt("mrk_5091"))
conf_int_1653 <- p_conf_int(getMaltoseDt("mrk_1653"))
res_4 <- grid.arrange(conf_int_5211,
                      conf_int_5091,
                      conf_int_1653,
                      ncol=3, nrow=1)
