# Homework exercises

library(magrittr)
library(data.table)
library(tidyr)

# section 04
# 1
dict_path <- "/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata"
baby_names_list <- list.files(file.path(dict_path, "baby-names"), full.names = TRUE)

# 2
baby_names_files <- list.files(file.path(dict_path, "baby-names"), full.names = FALSE)

# 3
names(baby_names_list) <- baby_names_files
tables <- lapply(baby_names_list, fread)
combined_baby_names <- rbindlist(tables, idcol = 'filename')

# 4
# not tidy as filename contains year and gender
tidy_baby_names <- separate(combined_baby_names,
                            col = 'filename',
                            into = c('year', 'sex'),
                            # remove = FALSE,
                            sep = '\\.')

# ---

# section 05
# 1
gt <- fread(file.path(dict_path, "eqtl", "genotype.txt"))
growth <- fread(file.path(dict_path, "eqtl", "growth.txt"))

# 2 + 3
head(gt)
head(growth)
# in this case: outer merge == inner merge
merged <- merge(growth, gt, by = 'strain')
head(merged)
melted_1 <- melt(merged,
                 measure.vars = colnames(growth)[-1],     # == id.vars = colnames(growth)[1]
                 value.name = 'growth_rate',
                 variable.name =  'media')
melted_final <- melt(melted_1,
                     measure.vars = colnames(gt)[-1],
                     value.name = 'gt',
                     variable.name = 'marker')

melted_final$strain <- as.factor(melted_final$strain)
melted_final$gt <- as.factor(melted_final$gt)

head(melted_final)
summary(melted_final)

# Alternative for 2:
# gt_melt <- melt(gt, id.vars = 'strain', variable.name = 'markers', value.name = 'gt')
# growth_melt <- melt(growth, id.vars = 'strain', variable.name = 'media', value.name = 'growth_rate')
# dt <- merge(gt_melt, growth_melt, by = 'strain', all = TRUE, allow.cartesian = TRUE)

# 4
library(ggplot2)
ggplot(melted_final[marker %in% c('mrk_5211', 'mrk_1653')], aes(marker, growth_rate, color = gt)) +
         geom_boxplot() + facet_wrap(~media)
