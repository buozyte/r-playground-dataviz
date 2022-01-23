# Homework exercises (central exercise session)

library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork) # optional, makes plots nicer
library(cowplot)
library(gridExtra)

# section 03
iris_dt <- as.data.table(iris)
base <- ggplot(iris_dt, aes(Sepal.Length, Sepal.Width, color=Species)) +
          geom_point()
# 1
base_model <- lm(Sepal.Width ~ Sepal.Length, data=iris_dt)
# different intercept, common slope model:
per_species_model <- lm(Sepal.Width ~ Sepal.Length + Species, iris_dt)
# model.matrix(per_species_model)
# different intercept and slope model:
species_model <- lm(Sepal.Width ~ Sepal.Length * Species, iris_dt)

# 2
# base model formula:
# Sepal.Width = beta_intercept
#                + beta_Sepal.Length * Sepal.Length     <- slope

# per_species_model formula:
# Sepal.Width = beta_intercept
#                + beta_versicolor * (is versicolor?)
#                + beta_virginica * (is virginica?)
#                + beta_Sepal.Length * Sepal.Length     <- slope

# species_model formula:
# Sepal.Width = beta_intercept
#                + beta_versicolor * (is versicolor?)
#                + beta_virginica * (is virginica?)
#                + beta_Sepal.Length * Sepal.Length     <- general slope
#                + beta_Sepal.Length_vercolor * (is versicolor?)
#                   * Sepal.Length                      <- spec.slope
#                + beta_Sepal.Length_virginica * (is virginica?)
#                   * Sepal.Length                      <- spec.slope

# 3
iris_dt[, base_pred := predict(base_model)]
iris_dt[, common_slope_pred := predict(per_species_model)]
iris_dt[, species_pred := predict(species_model)]
overlayed_base <- ggplot(iris_dt) +
                    geom_point(aes(Sepal.Length, Sepal.Width, color=Species)) +
                    geom_line(aes(Sepal.Length, base_pred)) +
                    facet_wrap(~Species)
overlayed_common_slope <- ggplot(iris_dt) +
                            geom_point(aes(Sepal.Length, Sepal.Width, color=Species)) +
                            geom_line(aes(Sepal.Length, common_slope_pred)) +
                            facet_wrap(~Species)
overlayed_diff <- ggplot(iris_dt) +
                    geom_point(aes(Sepal.Length, Sepal.Width, color=Species)) +
                    geom_line(aes(Sepal.Length, species_pred)) +
                    facet_wrap(~Species)

grid.arrange(overlayed_base, overlayed_common_slope,
             overlayed_diff, ncol=2)

# 4
anova(base_model, per_species_model)
anova(base_model, species_model)