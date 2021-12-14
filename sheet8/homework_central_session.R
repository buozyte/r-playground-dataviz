# Homework exercises (central exercise session)

library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork)
library(gridExtra)

# section 04
# 1
dt_pitfalls <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/stats-pitfalls.csv")
summary(dt_pitfalls)
# problem: same means -> t-test will fails!

boxplot_pitfalls <- ggplot(melt(dt_pitfalls), aes(variable, value)) +
                      geom_boxplot()
violin_pitfalls <- ggplot(melt(dt_pitfalls), aes(variable, value)) +
                    geom_violin()
hist_pitfalls <- ggplot(melt(dt_pitfalls), aes(value)) +
                  geom_histogram() +
                  facet_wrap(~variable, scales="free")
# group2 has objects with significantly higher number
#   hence probably different distributions.

summary_pitfalls <- grid.arrange(
                      boxplot_pitfalls, violin_pitfalls,
                      hist_pitfalls,
                      ncol=2, nrow=2)
t_test_pitfalls <- t.test(dt_pitfalls$group1, dt_pitfalls$group2)
# fails as mean not informative
# "if distribution not normal, then mean is not that informative and not
#   the best summary anymore"

w_test_pitfalls <- wilcox.test(dt_pitfalls$group1, dt_pitfalls$group2)

# 2
dt_exams <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/exam_correlation.tsv")
summary(dt_exams)

per_cor_exams <- dt_exams[, cor.test(attendance,
                                     achieved_points,
                                     method="pearson")]
# pearson rejects H0
spr_cor_exams <- dt_exams[, cor.test(attendance,
                                     achieved_points,
                                     method="spearman")]
# spearman rejects H0, but is higher (as it looks at the ranks!)
# note: spearman robust to outliers

scatter_exams <- ggplot(dt_exams, aes(attendance, achieved_points)) +
                  geom_point() +
                  geom_smooth(method='lm')
# there are outliers on the bottom right
# "line represents (spearman) correlation"


# ---

# section 05 (similar to former exams)
# 1
dt_cars <- as.data.table(mtcars)
dt_cars[, cyl_4:=cyl>4]
dt_cars[, gear_3:=gear>3]
dt_cars_red <- dt_cars[, .(cyl_4, gear_3)]

# contingency table, i.e. sums over number of elements, see lecture.
cont_tbl <- table(dt_cars_red)
f_test_cars <- fisher.test(cont_tbl)
# reject null hypothesis

signif(f_test_cars$p.value, digits=2)

# 2
# p-value is lower than alpha, hence reject H0 (??)

# 3
f_test_cars_alt <- fisher.test(cont_tbl, alternative="greater")
# conclusion: fail to reject H0

# 4
# fisher test does not make assumptions on distribution,
#   hence nothing has to be changed.

# ---

# section 06
gene <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/gene.txt")
genotype <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/genotype.txt")
genotype <- melt(genotype, id.vars = "strain", variable.name = "marker",
                 value.name = "genotype")
growth <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/growth.txt")
growth <- melt(growth, id.vars = "strain", variable.name = "media",
               value.name = "growth_rate")
marker <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/marker.txt")
# 1
marker_test <- function(marker1, marker2){
  mks_geno <- genotype[marker %in% c(marker1, marker2)] %>%
                spread(marker, genotype)
  table_markers <- table(mks_geno[, 2:3])
  return(fisher.test(table_markers)$p.value)
}

marker_test_1_13314 <- marker_test("mrk_1", "mrk_13314")
# fail to reject H0

# 2
# apply function to marker 1 + all other markers 
other_markers <- marker[id !=  "mrk_1"]
other_markers[, pval:=sapply(id, marker_test, "mrk_1")]

# 3
# take negative log as all values between 0 and 1, hence streched
plot_markers_pval <- ggplot(other_markers, aes(start, -log10(pval))) +
                      geom_point() +
                      facet_wrap(~chrom, scales="free_x")
# conclusion: in chromosome 1 the association is higher if things are
#               closer to the start (i.e. closer to marker 1, hence
#               on the same or neighbouring blocks).
# P-value: if assume that H0 is true, what is probability of getting more
#           extreme values
# Hence: small P-value -> extreme cases still appear

# 4
other_markers[, on_chr_1:=.(chrom=="chr01")]

pval_on_chr_1 <- ggplot(other_markers, aes(pval)) +
                  geom_histogram(bins=20) +
                  facet_wrap(~on_chr_1, scales="free_y")

# 5
frac_on_1 <- nrow(other_markers[pval<0.05 & (on_chr_1),])/nrow(other_markers[(on_chr_1),])
frac_not_on_1 <- nrow(other_markers[pval<0.05 & (!on_chr_1),])/nrow(other_markers[(!on_chr_1),])
# conclusion: H0 seems to be often right