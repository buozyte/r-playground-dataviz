# Homework exercises (central exercise session)

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
# 1
conditioned_on_5211 <- ggplot(conditioned_dt, aes(mrk_5091, growth_rate)) +
                        geom_boxplot() +
                        facet_wrap(~mrk_5211)

# H0: Marker 5091 is not significantly associated with the growth
#     if conditioned on marker 5211

growth_medians <- function(dt, by_val=c("test", "const")){
  dt[, median(growth_rate, na.rm=TRUE), by=by_val]
}

p_val_med <- function(test_val, const_val, N_permu = 1000){
  dt <- getMaltoseDt(c("mrk_5091", "mrk_5211")) %>% spread(marker, genotype)
  setnames(dt, test_val, "test")
  setnames(dt, const_val, "const")
  medians <- growth_medians(dt) %>% spread(test, V1)
  
  T_ref <- mean(medians[, `Wild isolate`-`Lab strain`])
  T_star <- numeric(N_permu)
  for (i in 1:N_permu){
    dt[, test:=sample(test), by=const]
    medians <- growth_medians(dt) %>% spread(test, V1)
    T_star[i] <- mean(medians[, `Wild isolate`-`Lab strain`])
  }
  
  g <- ggplot(data = data.table(T_star = T_star), aes(T_star)) +
    geom_histogram() +
    geom_vline(aes(xintercept=T_ref, color="T_ref"))
  
  p_val <- (sum(T_star >= T_ref) + 1) / (N_permu + 1)
  return(list(p_val, g))
}

condition_5091_on_5211 <- p_val_med("mrk_5091", "mrk_5211")

# 2
conditioned_on_5091 <- ggplot(conditioned_dt, aes(mrk_5211, growth_rate)) +
                        geom_boxplot() +
                        facet_wrap(~mrk_5091)
condition_5211_on_5091 <- p_val_med("mrk_5211", "mrk_5091")

summary_3 <- grid.arrange(condition_5091_on_5211[[2]],
                          condition_5211_on_5091[[2]],
                          ncol=3, nrow=1)

# ---

# section 04
# 1
p_conf_int <- function(marker){
  dt_resampled <- getMaltoseDt(marker)
  ref_diff <- median_diff(dt_resampled)
  
  resample <- function(x){
    median_diff(dt_resampled[sample(.N, .N, replace=TRUE)])
  }
  boot_diff <- sapply(1:1000, resample)
  
  conf_int = quantile(boot_diff, c(0.025, 0.975))
  
  plot_conf_int <- ggplot(data = data.table(boot_diff = boot_diff), aes(boot_diff)) +
    geom_histogram() +
    geom_vline(aes(xintercept=ref_diff, color="ref_diff")) +
    geom_vline(aes(xintercept=conf_int[1], color="CI"), linetype="dashed") +
    geom_vline(aes(xintercept=conf_int[2], color="CI"), linetype="dashed")
  return(plot_conf_int)
}

conf_int_5211 <- p_conf_int("mrk_5211")
conf_int_5091 <- p_conf_int("mrk_5091")
conf_int_1653 <- p_conf_int("mrk_1653")
summary_4 <- grid.arrange(conf_int_5211,
                          conf_int_5091,
                          conf_int_1653,
                          ncol=3, nrow=1)
