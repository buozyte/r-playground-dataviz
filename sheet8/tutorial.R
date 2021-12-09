# Tutorial exercises

# section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(datasets)
library(gridExtra)
library(patchwork)

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

# ---

# section 01
# 1
# 1.
# H0: no significant association between height and weight
# test: Spearman correlation test
# Note: due to central limit theorem it is not unlikely that the
#       distribution will at least tend to normal distribution
# 2.
# H0: no significant difference in average weight,
#     i.e. equal means
# test: t-test
# 3.
# H0: tests are always properly administered,
#     i.e. test is correct 99% of the time
# test: Binomial test
# 4.
# H0: no significant association between infected and positive
# test: Fisher exact test

# ---

# section 02
# 1
getMaltoseDt <- function(mrk){
  growth_mrk <- merge(growth,
                      genotype[marker == mrk, .(strain, genotype)],
                      by = 'strain')
  growth_mrk[media == "YPMalt"]
}

plot_growth_one_mk <- function(mk){
  ggplot(getMaltoseDt(mk), aes(genotype, growth_rate)) +
    geom_boxplot() +
    labs(title = mk) +
    theme_bw(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5))
}
boxplot_5211 <- plot_growth_one_mk("mrk_5211")

#2
# tests: Wilcoxon, Welch
# alternative hypothesis: double sided

# H0: no difference in means
# H1: there is some differnce in means
wilcox_test <- wilcox.test(growth_rate ~ genotype,
                           data=getMaltoseDt("mrk_5211"),
                           alternative="two.sided")
welch_test <- t.test(growth_rate ~ genotype,
                     data=getMaltoseDt("mrk_5211"),
                     alternative="two.sided")
welch_test_equal_var <- t.test(growth_rate ~ genotype,
                               data=getMaltoseDt("mrk_5211"),
                               alternative="two.sided",
                               var.equal=TRUE)
# results: P-values are really low, hence significant statistical
#           association as H0 is rejected

#3
test_growth <- function(mk, test="t"){
  m_dt <- getMaltoseDt(mk)
  if(test == 'wilcoxon') {
      return((wilcox.test(growth_rate ~ genotype,
                          m_dt,
                          alternative="two.sided"))$p.value)
    } else {
      return((t.test(growth_rate ~ genotype,
                     m_dt,
                     alternative="two.sided"))$p.value)
  }
  return(-1)
}

mk_1653_w <- test_growth("mrk_1653", "wilcoxon")
mk_1653_t <- test_growth("mrk_1653")
# result: fail to reject null hypothesis
mk_5091_w <- test_growth("mrk_5091", "wilcoxon")
mk_5091_t <- test_growth("mrk_5091")
# result: reject null hypothesis, hence significant statistical
#           association

# ---

# section 03
# 1
iris_dt <- as.data.table(iris)
iris_plot <- ggplot(iris_dt, aes(Sepal.Length,Sepal.Width)) +
              geom_point(aes(color=Species)) +
              geom_smooth(aes(color=Species), method='lm') +
              geom_smooth(method='lm')
# geom smooth can "visualize" correlation
# but this line does not make sense, as it implies that
#   with higher length, the width gets smaller

spear_cor <- cor(iris_dt$Sepal.Length,
                 iris_dt$Sepal.Width,
                 method="spearman")
spear_test <- cor.test(iris_dt$Sepal.Length,
                       iris_dt$Sepal.Width,
                       method="spearman")
# no results due to too ties

pears_cor <- cor(iris_dt$Sepal.Length,
                 iris_dt$Sepal.Width,
                 method="pearson")
pears_test <- cor.test(iris_dt$Sepal.Length,
                       iris_dt$Sepal.Width,
                       method="pearson")
# no results due to too ties
#2
setosa_plot <- ggplot(iris_dt[Species=="setosa",],
                      aes(Sepal.Length,Sepal.Width)) +
                geom_point() +
                geom_smooth(method='lm')
setosa_spear <- cor.test(iris_dt[Species=="setosa",]$Sepal.Length,
                         iris_dt[Species=="setosa",]$Sepal.Width,
                         method="spearman")
setosa_cor <- cor(iris_dt[Species=="setosa",]$Sepal.Length,
                  iris_dt[Species=="setosa",]$Sepal.Width,
                  method="spearman")
versicolor_plot <- ggplot(iris_dt[Species=="versicolor",],
                          aes(Sepal.Length,Sepal.Width)) +
                    geom_point() +
                    geom_smooth(method='lm')
versicolor_spear <- cor.test(iris_dt[Species=="versicolor",]$Sepal.Length,
                             iris_dt[Species=="versicolor",]$Sepal.Width,
                             method="spearman")
virginica_plot <- ggplot(iris_dt[Species=="virginica",],
                         aes(Sepal.Length,Sepal.Width)) +
                    geom_point() +
                    geom_smooth(method='lm')
virginica_spear <- cor.test(iris_dt[Species=="virginica",]$Sepal.Length,
                            iris_dt[Species=="virginica",]$Sepal.Width,
                            method="spearman")

faster_cor <- iris_dt[, cor.test(Sepal.Length, Sepal.Width), by=Species]

summary_spec <- ggplot(iris_dt, aes(Sepal.Length,Sepal.Width)) +
                  geom_point() +
                  geom_smooth(method='lm') +
                  facet_wrap(~Species)

plot_summary <- grid.arrange(
                  iris_plot, setosa_plot,
                  versicolor_plot, virginica_plot,
                  ncol=2, nrow=2)
# buzzword: Simpson's Paradox