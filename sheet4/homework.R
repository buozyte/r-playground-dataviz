# Homework exercises

library(data.table)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(magrittr)
library(gridExtra)

# section 05
# 1
medals_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/medals.csv")
pop_vs_total <- ggplot(data=medals_dt, aes(x=total, y=population)) +
                  geom_point()

# 2
# main problem: scaling
pop_vs_total_scaled <- pop_vs_total +
                        scale_x_log10() +
                        scale_y_log10()

# 3
pop_vs_total_scaled_labeled <- pop_vs_total_scaled +
                                geom_text(aes(label=country))

pop_vs_total_scaled_labeled_repel <- pop_vs_total_scaled +
                                      geom_text_repel(aes(label=country))
# comparison: a little bit better with repel :)

# ---

# section 06
# 1
anscombe_reshaped <- anscombe %>%
  as.data.table %>%
  .[, ID := seq(nrow(.))] %>% melt(id.var=c("ID")) %>%
  separate(variable, c('xy', "group"), sep=1) %>% dcast(... ~ xy) %>%
  .[, group := paste0("dataset_", group)]

# reshaped is tidier as the original one contains columns
#   with repetitions over all rows.
# additionally the original table contains the information about the
#   data set in the column names.
# lastly the information about the axis is devided into four columns
#   each.

# 2
mean <- anscombe_reshaped[, .(mean_x = mean(x), mean_y = mean(y)), by=group]
stand_dev <- anscombe_reshaped[, .(sd_x = sd(x), sd_y = sd(y)), by=group]

# all means and sds are very similar per axis

# 3
pearson_cor <- anscombe_reshaped[, cor(x, y), by=group]

# 4
plot_each_set <- ggplot(data=anscombe_reshaped, aes(x=x, y=y)) +
                  geom_point() +
                  facet_wrap(~group)

# 5
boxplots_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/boxplots.csv")
boxplots1 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_1)) +
              geom_boxplot()
boxplots2 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_2)) +
              geom_boxplot()
boxplots3 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_3)) +
              geom_boxplot()
boxplots4 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_4)) +
              geom_boxplot()
boxplots5 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_5)) +
              geom_boxplot()

# 6
violin1 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_1)) +
              geom_violin()
violin2 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_2)) +
              geom_violin()
violin3 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_3)) +
              geom_violin()
violin4 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_4)) +
              geom_violin()
violin5 <- ggplot(data=boxplots_dt, aes(x=1, y=dataset_5)) +
              geom_violin()

grid <- grid.arrange(boxplots1, boxplots2, boxplots3, boxplots4, boxplots5,
                      violin1, violin2, violin3, violin4, violin5,
                      ncol=5, nrow=2)
# boxplots are the same hence imply that the data is similar,
#   but the violin plots show that the datasets are indeed
#   very different.

# ---

# section 7
# 1
boxplot_mtcars <- ggplot(data=mtcars, aes(x=as.factor(cyl), y=mpg)) +
                    geom_boxplot()

# 2
# TODO
