# Homework exercises

# section 00
library(magrittr)
library(data.table)
library(tidyr)

# ---

# section 01
# 1
# d. (+1)

# 2
# b (+1)

# 3
# No, label contains multiple values (person, gender, vowels).

# 4
product_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/example_product_data.csv")

# 5
melted_product <- melt(product_dt,
                       id.vars = "name", 
                       measure.vars = c("producta", "productb"),
                       value.name = "number_products",
                       variable.name = "product_type")
# alternative: melt(product_dt, id.vars = "name", value.name = "number_products",
#                    variable.name = "product_type")
# alternative: melt(product_dt, measure.vars = c("producta", "productb"),
#                     value.name = "number_products", variable.name = "product_type")
# names can also be left out, but they get "value" and "variable" assigned :)

# 6
casted_product <- dcast(melted_product,
                        ... ~ product_type,
                        #     other columns are arranged accordingly
                        value.var = "number_products")
# casted_product == product_dt
# same information in table, but different order :)
# HINT: leave out "value.var" -> R assigns first(?) numerical column as values

# ---

# section 02
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, carname := rownames(mtcars)]
dt1 <- mtcars_dt[5:25,.(carname, mpg, cyl)]
dt2 <- mtcars_dt[1:10, .(carname, gear)]
# 1
inner <- merge(dt1, dt2, by = "carname", all = FALSE)
inner_cols <- inner[, .N]

# 2
left <- merge(dt1, dt2, by = "carname", all.x = TRUE)
left_cols <- left[, .N]

# 3
outer <- merge(dt1, dt2, by = "carname", all = TRUE)
outer_cols <- outer[, .N]

# ---

# section 03
# 1
weather <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/weather.txt")

# 2
# Too many NAs (sparse matrix -> unefficient)

# - variables as columns (days)
# - element column is not a variable
# - single entity spread over many columns

# 3
# Combine year, month and day to one variable/column and give the values a separate
#   column/variable decribing exactly what they mean -> i.e. in a column based on
#   element

# 4
per_day <- melt(weather,
                measure.vars = colnames(weather)[-1:-4],    # == id_vars = colnames(weather)[1:4]
                value.name = "value",
                variable.name = "day",
                na.rm = TRUE)
per_day[, day := gsub('d', '', day)]                        # remove d from the day
tidyer <- unite(per_day,
                col = "date",
                year,
                month,
                day,
                sep = "-")
tidy <- dcast(tidyer,
              ... ~ element,
              value.var = "value")

# alternative to removing NAs
# tidy[!(is.na(TMAX) & is.na(TMIN))]
