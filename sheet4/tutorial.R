# Tutorial exercises

# section 00
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(ggrepel)

# ---

# section 01
# 1
# boxplot

# 2
# bar chart

# 3
# line chart

# 4
# scatterplot

# ---

# section 02
# 1
mpg_dt <- as.data.table(mpg)
plot_2_dir <- ggplot(data=use, aes(x=cty, y=hwy, color=factor(year))) +
                geom_point() +
                geom_smooth()
use <- mpg_dt[, year:=as.factor(year)]
plot_2 <- ggplot(data=use, aes(x=cty, y=hwy, color=year)) +
            geom_point() +
            geom_smooth()

# ---

# section 03
# 1
iris_dt <- as.data.table(iris)
head(iris_dt, 2)
tail(iris_dt, 2)

# 2
iris_melted <- melt(iris_dt, 
                    id.vars = "Species",
                    value.name = "value",
                    variable.name = "type")
iris_hist <-  ggplot(iris_melted, aes(x=value)) +
                geom_histogram() +
                facet_wrap(~type)

# 3
iris_hist_60 <-  ggplot(iris_melted, aes(x=value)) +
                  geom_histogram(bins=60) +
                  facet_wrap(~type)
iris_hist_50 <-  ggplot(iris_melted, aes(x=value)) +
                  geom_histogram(bins=50) +
                  facet_wrap(~type)
iris_hist_40 <-  ggplot(iris_melted, aes(x=value)) +
                  geom_histogram(bins=40) +
                  facet_wrap(~type)
iris_hist_20 <-  ggplot(iris_melted, aes(x=value)) +
                  geom_histogram(bins=20) +
                  facet_wrap(~type)
iris_hist_10 <-  ggplot(iris_melted, aes(x=value)) +
                  geom_histogram(bins=10) +
                  facet_wrap(~type)
grid_iris_hist <- grid.arrange(iris_hist_10, iris_hist_20,
                                iris_hist, iris_hist_40,
                                iris_hist_50, iris_hist_60,
                                nrow=2)

# often: most lengths and widths (except fo the sepal length)
#   are the same for each observation.
# too many or not enough bins is not very good
# also: either x or y in histograms, never both! :)

# 4
iris_boxplot <- ggplot(iris_melted, aes(x=type, y=value)) +
                  geom_boxplot()
# remove outliers: geom_boxplot(outlier.shape=NA)
iris_boxplot_per_spec <- ggplot(iris_melted, aes(x=type, y=value)) +
                          geom_boxplot() +
                          facet_wrap(~Species)

# 5
iris_box_jitter <- iris_boxplot +
                      geom_jitter()
# maybe a bit easier to read with geom_dotplot() instead if geom_jitter()

# boxplot does not really represent true distribution, for example
#   in the case of the petal lengths, which are basically divided into
#   two clusters

# 6
library(gridExtra)
iris_violin <- ggplot(iris_melted, aes(x=type, y=value)) +
                geom_violin()
iris_violin_jitter <- iris_violin +
                        geom_jitter()
grid_violin_box <- grid.arrange(iris_boxplot, iris_violin,
                                iris_box_jitter, iris_violin_jitter,
                                nrow=2)
# violins represent data better :)

# geom_{} objects are stacked, i.e. what comes first will be plotted first

# 7
# bi-modal patterns and "extreme" patterns, which could be interpreted
#   as outliers
summary(iris_dt)
dot_per_spec <- ggplot(data=iris_melted, aes(x=type, y=value, color=Species)) +
                  geom_dotplot(bixaxis="y",
                                stackdir='center',
                                dotsize=0.3)
violin_jitter_per_spec <- iris_violin_jitter +
                            aes(color=Species) +
                            facet_wrap(~Species)

# ---

# section 04
library(GGally)
# 1
petals <- ggplot(data=iris_dt, aes(x=Petal.Length, y=Petal.Width, color=Species)) +
            geom_point()
corr <- ggcorr(iris_dt, geom = 'circle')
pairs <- ggpairs(iris_dt, columns = colnames(iris_dt)[1:4])
petal_pairs <- ggpairs(iris_dt, columns = c('Petal.Length', 'Petal.Width'))

# 2
corr_spec <- petals +
              facet_wrap(~Species)
# alternative: my petals plot as it includes already colors :)

alt <- petals +
        geom_smooth(method="lm")




