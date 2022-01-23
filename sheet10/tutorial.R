library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork) # optional, makes plots nicer
library(cowplot)

# ---

# section 01
path <- "/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/height.csv"

heights <- fread(path) %>% na.omit() %>% .[, sex:=as.factor(toupper(sex))]
# 1
m <- heights[, lm(height ~ sex + mother + father)]
summary(m)

# m_only_m <- heights[sex %in% c("M, F"), lm(height ~ sex + mother + father)]
# summary(m_only_m)

# 2
y_hat <- predict(m)
resi <- residuals(m)

ggplot(data.table(y_hat = y_hat, res = resi), aes(x=y_hat, y=res)) +
  geom_point() +
  geom_hline(yintercept = 0)
# variances very similar everywhere

qqnorm(resi)
qqline(resi)
# ggplot(data.table(y_hat = y_hat, res = resi), aes(sample=res)) +
#   geom_pp() +
#   geom_line()

# conclusion: linear regression correct yey :)

# 3 + 4
male_heights <- heights[sex == "M"]
male_heights[, son_father := predict(lm(height ~ father))]
male_heights[, father_son := predict(lm(father ~ height))]

ggplot(male_heights) +
  geom_point(aes(x=height, y=father)) +
  geom_line(aes(x=height, y=father_son, color="blue")) +
  geom_line(aes(x=son_father, y=father, color="red"))

# 5
pca_comp <- princomp(male_heights[, .(height, father)])$loadings
pca_centr <- princomp(male_heights[, .(height, father)])$center

slope <- pca_comp["height", "Comp.1"] / pca_comp["father", "Comp.1"]
intercept <- pca_centr["father"] - slope * pca_centr["height"]

ggplot(male_heights) +
  geom_point(aes(x=height, y=father)) +
  geom_line(aes(x=height, y=father_son, color="blue")) +
  geom_line(aes(x=son_father, y=father, color="red")) +
  geom_abline(aes(intercept=intercept, slope=slope, color="green"))

# 6
# PCA 1: minimize distance between points and "middle line", i.e.
#   minimization along both axes

# ---

# section 02
growth <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/growth.txt")
growth <- growth %>% melt(id.vars="strain", variable.name='media', value.name='growth_rate')
growth <- growth[media=="YPMalt"]
genotype <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/eqtl/genotype.txt")
genotype <- genotype[, .(strain, mrk_5211, mrk_5091)]
# 1
gg <- merge(growth, genotype, by = c("strain"))
full <- gg[, lm(growth_rate ~ mrk_5211 + mrk_5091)]

# 2
reduced <- gg[, lm(growth_rate ~ mrk_5211)]
anova(full, reduced)
