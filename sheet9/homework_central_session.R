# Homework exercises (central exercise session)

library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork)
library(gridExtra)

# section 03
# former exam question but a bit harder :)
# 1
# quantitative variable: probably just numeric, but not
#                        non-categorial, non-binary            
# use e.g. summary(cars)
cars <- as.data.table(mtcars)
quant_cols <- c("mpg", "disp", "hp", "drat", "wt", "qsec")
# like a n-dimensional cross product of all the columns, but
#    - every combinations appears only once
#    - one variable appears at most once in an element/a combination
combinations <- as.data.table(t(combn(quant_cols, 2))) # t() = transpose

# sapply: loop over list
# apply: loop over 2-dim. object
# margin=1: given X it will go through each row and pipe it into the
#            funtions
# margin=2: given X it will go through each column and pipe it into the
#            funtions
# get: 'I'm giving you a string, but it is the thing that you
#       (data table) expect'
combinations[, pval := apply(combinations, 1, 
                             function(p) {
                               cars[, cor.test(get(p[[1]]), get(p[[2]]),
                                               method="spearman")$p.value]
                             })]
significant_pval <- combinations[pval < 0.05]

# 2
# in exam: find out which correction should be used based on the
#           wanted result or the rephrased definition of the correction
#           in the question
combinations[, pval_BH := p.adjust(pval, method="BH")]
significant_pval_BH <- combinations[pval_BH < 0.05]
# same result as in 1 :)
# now: at most 5% are false positive, controls ratio of positives

# 3
combinations[, pval_BF := p.adjust(pval, method="bonferroni")]
significant_pval_BF <- combinations[pval_BF < 0.05]
# now: probability(#(false positives) > 0) < 5%, controls probability

# false positive: null hypothesis true, but we reject it
# false negative: null hypothesis false, but we fail reject it
# true positive : null hypothesis false and we reject it
# true negative : null hypothesis true and we fail to reject it

# ---

# section 04
# 1
simulate_norm_groups <- function(sample_size=50, N_experiments=10000, mu_x=0, mu_y=0){
  sapply(seq(N_experiments),
         function(i){
           x <- rnorm(sample_size, mu_x)
           y <- rnorm(sample_size, mu_y)
           t.test(x, y, alternative="two.sided")$p.value
         })
}
plot_pval <- function(pvals, title="p-val distribution", ...){
  pval_dt <- data.table(pvals=pvals)
  histo <- ggplot(pval_dt, aes(pvals)) +
            geom_histogram(boundary = TRUE) +
            labs(title = title)
  qq <- ggplot(data = pval_dt, aes(sample = pvals)) +
          geom_qq(distribution = stats::qunif,
                  dparams = list(min = 0, max = 1)) +
          geom_abline(a=0,b=1) +
          ylim(c(0,1)) +
          labs(title = title)
  histo + qq
}

set.seed(10)
pvals0 <- simulate_norm_groups(sample_size=50)

# 2
pvals0_plot <- plot_pval(pvals=pvals0)
# expect: uniform distribution

# 3
pvals0_B <- p.adjust(pvals0, method="bonferroni")
pvals0_B_plot <- plot_pval(pvals0_B, main="\Bonferroni adj. p-values")
# result: perfect distribution, p(more than 0 false positive) is low
# if null hypothesis always true, then all p-values are moved up to 1

pvals0_BH <- p.adjust(pvals0, method="BH")
pvals0_BH_plot <- plot_pval(pvals0_BH, main="\BH adj. p-values")
# effects less extreme

# summary: take diagonal and shift it upwards

# ---

# section 05
# 1
samples_10 <- plot_pval(simulate_norm_groups(sample_size=10,
                                             mu_y=0.5),
                        main="H1: sample size 10")
# result: fail to reject in most cases although null hypothesis is
#          wrong due to wrong sample size (i.e. too much noise compared
#          to sample size)
samples_100 <- plot_pval(simulate_norm_groups(sample_size=100,
                                              mu_y=0.5),
                         main="H1: sample size 100")
# result: "plot goes down", correctly reject null hypothesis more often
samples_1000 <- plot_pval(simulate_norm_groups(sample_size=1000,
                                               mu_y=0.5),
                          main="H1: sample size 1000")
# result: correctly always reject null hypothesis

# 2
plot_pval_log10 <- function(pvals, title="p val distribution", ...){
  n <- length(pvals)
  
  # trafo on both axes: -log10()
  dt <- data.table(
    observed = -log10(sort(pvals)),
    expected = -log10(ppoints(n))
    )
  
  ggplot(dt) +
    geom_point(aes(expected, observed)) +
    geom_abline(intercept = 0, slope = 1)
}
pvals <- c(simulate_norm_groups(sample_size=50,
                                N_experiments=10000),
           simulate_norm_groups(sample_size=50,
                                N_experiments=1000,
                                mu_y=0.5))
plot_normal <- plot_pval(pvals, title="10000 H0 with 1000 H1")
plot_log_10 <- plot_pval_log10(pvals, main="10000 H0 with 1000 H1")
# big deviation close to 0
# better visible in -log10() plot

# 3
error_analysis <- function(method='BH', sample_size=50, cut=0.05){
  pvals <- c(simulate_norm_groups(sample_size=sample_size,
                                  N_experiments = 10000),
             simulate_norm_groups(sample_size=sample_size,
                                  N_experiments = 1000,
                                  mu_y=0.5))
  names(pvals) <- rep(c("H0", "H1"),
                      c(10000, 1000))
  pvals_adj <- p.adjust(pvals, method=method)
  # contingency table:
  table(ifelse(pvals_adj < cut, "significant", "not significant"),
        names(pvals))
}

error_10 <- error_analysis(sample_size=10)
# result: never or rarely reject due to sample size
error_10_b <- error_analysis(sample_size=10, method="bonferroni")
# result: also never or rarely reject

error_30 <- error_analysis(sample_size=30)
# result: a lot of false negatives, some false positives
error_30_b <- error_analysis(sample_size=30, method="bonferroni")
# result: more false negatives than BH, most likely no false positives

error_50 <- error_analysis(sample_size=50)
error_50_b <- error_analysis(sample_size=50, method="bonferroni")

error_100 <- error_analysis(sample_size=100)
error_100_b <- error_analysis(sample_size=100, method="bonferroni")

error_1000 <- error_analysis(sample_size=1000)
# on average: (significantH0)/(significantH0 + significantH1) ~ 5%
error_1000_b <- error_analysis(sample_size=1000, method="bonferroni")
# with probability 5%: significantH0 > 0

# summary:
#  bonferroni has less false positives,
#   but considerably more false negatives unless sample size is huge
