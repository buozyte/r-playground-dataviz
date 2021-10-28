# Quizzes from lecture

# Question 1:
library(data.table)
awesome.dt <- data.table(x = order(1:6, decreasing = T), y = rep(c(TRUE, FALSE), each = 3))
# Answer: a) (+1)

# Question 2:
library(magrittr)
flights <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/flightsLAX.csv")
last_10_to_lax <- flights[MONTH == 12 & DAY == 24 & DESTINATION_AIRPORT == "LAX"] %>% tail(n=10)
# Answer: d) (+1)

# Question 3:
iris.dt <- data.table(iris)
number_species <- iris.dt[Species != "setosa" | Sepal.Length <= 5, .N, by = Species]
# Answer: d) (+1) (get #rows for each unique `Species` for all rows where
#             `Species` is not `"setosa"` or `Sepal.Length` is less than or equal to 5)

# Question 4:
flights_per_port <- flights[MONTH %in% 6:8, .N, by = ORIGIN_AIRPORT]
# Answer: b) (+1)
