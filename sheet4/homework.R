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
pop_vs_total <- ggplot(data=medals_dt, aes(y=total, x=population)) +
                  geom_point()

# 2
# main problem: scaling, can already be seen in the summary of the table!
pop_vs_total_scaled <- pop_vs_total +
                        scale_x_log10() +
                        scale_y_log10()

# 3
pop_vs_total_scaled_lab <- pop_vs_total_scaled +
                            geom_text(aes(label=country))

pop_vs_total_scaled_lab_rep <- pop_vs_total_scaled +
                                geom_text_repel(aes(label=country))
# comparison: a little bit better with repel :)

pop_vs_total_scaled_lab_rep_alt <- pop_vs_total_scaled +
                                    geom_label_repel(aes(label=country))

# bonus exercise: label only countries with pop>1e+8
bonus_1 <- pop_vs_total_scaled +
            geom_text_repel(data=medals_dt[population>1e+8],
                            aes(label=country))

# bonus exercise: label only specific countries
countries <- c('Mexico', 'Brazil', 'Columbia')
bonus_2 <- pop_vs_total_scaled +
            geom_text_repel(data=medals_dt[country %in% countries],
                            aes(label=country))

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
mean_stand <- anscombe_reshaped[, .(mean_x = mean(x), mean_y = mean(y),
                                    sd_x = sd(x), sd_y = sd(y)),
                                by=group]

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

boxplots_melted <- melt(boxplots_dt)
all_boxplots <- ggplot(data=boxplots_melted, aes(x=variable, y=value)) +
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

all_violins <- ggplot(data=boxplots_melted, aes(x=variable, y=value)) +
                geom_violin()

grid <- grid.arrange(boxplots1, boxplots2, boxplots3, boxplots4, boxplots5,
                      violin1, violin2, violin3, violin4, violin5,
                      ncol=5, nrow=2)
# boxplots are the same hence imply that the data is similar,
#   but the violin plots show that the datasets are indeed
#   very different.
# but boxplots provide other informations, like the mean or quantiles.
# hence a combination of e.g. boxplots and violins may be the best
#   solution.
possibly_better <- ggplot(data=boxplots_melted, aes(x=variable, y=value)) +
                    geom_violin(alpha=0.3) +
                    geom_boxplot(width=0.1)

# ---

# section 07
# 1
mtcars_dt <- as.data.table(mtcars)
boxplot_mtcars <- ggplot(data=mtcars_dt, aes(x=as.factor(cyl), y=mpg)) +
                    geom_boxplot()

# 2
mtcars_dt[, medians:=median(mpg), by=cyl]
mtcars_dt[, c("lower_quartile", "upper_quartile"):=
            .(quantile(mpg, 0.25),
               quantile(mpg, 0.75)), by=cyl]
mtcars_dt[, c("lower_IQR", "upper_IQR"):=
            .(lower_quartile-1.5*IQR(mpg),
              upper_quartile+1.5*IQR(mpg)), by=cyl]
mtcars_dt[mpg<upper_IQR, up_whisker:=max(mpg), by=cyl]
mtcars_dt[mpg>lower_IQR, down_whisker:=min(mpg), by=cyl]
mtcars_dt[, outlier:=(mpg<lower_IQR | mpg>upper_IQR), by=cyl]

boxplot_selfmade <- ggplot(data=mtcars_dt) +
                      geom_crossbar(aes(x=cyl, y=medians,
                                        ymax=upper_quartile, ymin=lower_quartile),
                                        fill='white', width=1.3) +  # box
                      geom_segment(aes(x=cyl, y=upper_quartile,
                                       xend=cyl, yend=up_whisker)) +  # whiskers
                      geom_segment(aes(x=cyl, y=down_whisker,
                                       xend=cyl, yend=lower_quartile)) +  # whiskers
                      geom_point(data=mtcars_dt[outlier==TRUE], 
                                 aes(x=cyl, y=mpg)) +  # outliers
                      labs(y='mpg')
