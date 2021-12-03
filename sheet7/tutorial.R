# Tutorial exercises

# section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(patchwork)

genotype <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/genotype.txt")
genotype <- melt(genotype, id.vars = "strain", variable.name = "marker",
                 value.name = "genotype")
growth <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/growth.txt")
growth <- melt(growth, id.vars = "strain", variable.name = "media",
               value.name = "growth_rate")
marker <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/marker.txt")

# ---

# section 01
# 1
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

# Calling the function:
medians_5211 <- p_val_medians(getMaltoseDt("mrk_5211"))

# 2
medians_1653 <- p_val_medians(getMaltoseDt("mrk_1653"))
medians_5091 <- p_val_medians(getMaltoseDt("mrk_5091"))
cat("Marker 5211:", medians_5211[[1]], "> 5% ==", medians_5211[[1]] > 0.05)
cat("Marker 1653:", medians_1653[[1]], "> 5% ==", medians_1653[[1]] > 0.05)
cat("Marker 5091:", medians_5091[[1]], "> 5% ==", medians_5091[[1]] > 0.05)

grid.arrange(box_5211, medians_5211[[2]], 
             medians_1653[[2]], medians_5091[[2]],
             ncol=2, nrow=2)

# Interpretation:
# The second null hypothesis is not rejected, the others are rejected.
#  Hence marker 1653 does not associate with growth, while marker 5211
#   and 5091 are significantly associated with growth.

# ---

# section 02
# 1
mks_geno <- genotype[marker %in% c("mrk_5091", "mrk_5211")] %>%
  spread(marker, genotype)

# H0:
# marker 5091 is not significantly associated with marker 5211

# Test statistic T_ref
equal_genotypes <- function(dt){
  dt[mrk_5091 == mrk_5211, .N]
}
# alternative:
equal_genotypes_alt <- function(dt){
  dt[mrk_5091 == mrk_5211, .N] / nrow(dt)
}
percentage_equal <- equal_genotypes_alt(mks_geno)
# if associated, then percentage will rpobably be over 50%

# Permutation test
p_val_equal_gen <- function(dt, N_permu = 1000){
  # It will return both a pvalue and plot a histogram of T_star
  T_ref <- equal_genotypes(dt)
  T_star <- sapply(1:N_permu,
                   function(x){
                     equal_genotypes(dt[, mrk_5211 := sample(mrk_5211)])
                   }
  )
  # Plot
  g <- ggplot(data = data.table(T_star = T_star), aes(T_star)) +
        geom_histogram() +
        geom_vline(aes(xintercept=T_ref, color="T_ref")) +
        xlim(0,nrow(dt))
  
  # Compute and return the p value 
  # Note: only one sided p-value (here: right)
  # Reason: only want to know if there are "bigger associations"
  p_val<- (sum(T_star >= T_ref) + 1) / (N_permu + 1)
  return(list(p_val, g))
}

res <- p_val_equal_gen(mks_geno)
cat(res[[1]] / nrow(mks_geno), "> 5% ==",(res[[1]] / nrow(mks_geno)) > 0.05)

# Conclusion:
# H0 is rejected, hence the two markers are significantly associated