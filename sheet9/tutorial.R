library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork)
library(gridExtra)

gene <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/gene.txt")
genotype <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/genotype.txt")
genotype <- melt(genotype,
                 id.vars = "strain",
                 variable.name = "marker",
                 value.name = "genotype")
growth <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/growth.txt")
growth <- melt(growth,
               id.vars = "strain",
               variable.name = "media",
               value.name = "growth_rate")
marker <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/marker.txt")
setnames(marker, "id", "marker")

# ---

# section 01
# 1
set.seed(10)
# rnorm allows you to draw random values from a normal distribution
# rnorm(10) # 10 random draws
# pnorm gives the cumulative probability from a normal
# i.e. pnorm(x) = p(X < x) where X is the random variable
# pnorm(0)
# qnorm returns the quantiles of a normal distribution
# it is the inverse of pnorm
# i.e. given a probability p,
# it finds x so that pnorm(x) = p
# qnorm(0.5)
# qnorm can be used to find different types of quantiles
# qnorm(seq(0.25,0.75,0.25)) # quartiles of the normal
# qnorm(seq(0.1,0.9,0.1)) # deciles of the normal

sim_normal_dist <- data.table(x = rnorm(100))

plot_qq <- function(dt, observed_quantile){
  plot_hist <- ggplot(dt, aes(get(observed_quantile))) +
                geom_histogram(bins=20) +
                xlim(-6, 6) +
                xlab("Observed quantiles")
  plot_qq <- ggplot(dt, aes(sample = sort(get(observed_quantile)))) +
              geom_qq(distribution=stats::qnorm) +
              # geom_qq_line(distribution=stats::qnorm) +
              xlim(-6, 6) +
              ylim(-6.5, 6.5) +
              xlab("sample") +
              ylab("theoretical") +
              geom_abline(intercept=0,slope=1)
  return(list(plot_hist, plot_qq))
}
hist_and_qq <- plot_qq(sim_normal_dist, 'x')

# 2
mean_4_normal_dist <- data.table(x = rnorm(100, mean=4))
# actually shifted up, althought it looks like a left-shift
hist_and_qq_mean_4 <- plot_qq(mean_4_normal_dist, 'x')

# 3
# 2.5 as sample is about 2 time theoratical value
dev_x_normal_dist <- data.table(x = rnorm(100, sd=2.5))
hist_and_qq_dev_x <- plot_qq(dev_x_normal_dist, 'x')

all_plots <- grid.arrange(hist_and_qq[[1]], hist_and_qq[[2]],
                          hist_and_qq_mean_4[[1]], hist_and_qq_mean_4[[2]],
                          hist_and_qq_dev_x[[1]], hist_and_qq_dev_x[[2]],
                          ncol=2, nrow=3)

# ---

# section 02
# TODO: compare with official solution?
#         something does not work quiet right here ...

# 1 (not really exam level, a bit too high)
merged_dt <- merge(growth, # [media == "YPMalt"],
                   genotype,
                   by = 'strain',
                   allow.cartesian = TRUE)
# YPMalt instead of growth_rate?
merged_dt[, p_val := wilcox.test(growth_rate ~ genotype)$p.value, by=marker]
plot_hist_yeast <- ggplot(merged_dt, aes(p_val)) +
                    geom_histogram(boundary=TRUE, bins=50)
plot_qq_yeast <- ggplot(merged_dt[order(p_val)], aes(-log10(ppoints(p_val)), -log10(p_val))) +
                  geom_point() +
                  geom_abline()
final_yeast <- plot_hist_yeast + plot_qq_yeast

merged_dt[, p_adj := p.adjust(p_val, method="BH")]
merged_dt[p_adj < 0.05][order(p_adj)]

# do need to correct for multiple testing

# 2
d_merged_dt <-  merge(merged_dt, # [media == "YPMalt"],
                      marker,
                      by = 'marker',
                      allow.cartesian = TRUE)
plot_qq_per_chrom <- ggplot(d_merged_dt, aes(start, -log10(p_val))) +
                      geom_point() +
                      geom_abline(intercept=-log10(0.05), slope=0) +
                      facet_wrap(~chrom, scales="free_x", nrow=2) +
                      theme(axis.text.x=element_blank())

# 3
below_sig_val_normal <- d_merged_dt[p_val<0.05, .N]
below_sig_val_adjusted <- d_merged_dt[p_adj<0.05, .N]
# only really associated with growth is marker 5211
#   but although we adjusted, we get significant p-values for other
#   markers
