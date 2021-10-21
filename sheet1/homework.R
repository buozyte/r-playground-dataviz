# Homework exercises

# section 04
data(murders)
# 1
pop <- murders$population
sorted_pop <- sort(pop)
min_val <- sorted_pop[1]
print(min_val == min(murders$population))
print(min_val)

# 2
pop <- murders$population
ordered_pop <- order(pop)
index_min_val <- ordered_pop[1]
print(index_min_val)

# 3
ind_min_val <- which.min(murders$population)
print(index_min_val == ind_min_val)

# 4
print(murders$state[index_min_val])

# 5
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)

ranks <-rank(murders$population)
my_df <- data.frame(rank = ranks, state = murders$population)

# ---

# section 05
# 1
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
names(temp) <- city
temp_F <- (5 / 9) * (temp - 32)

# 2
data(na_example)
str(na_example)
mean(na_example)

ind <- is.na(na_example)
print(sum(ind))

# 3
print(mean(na_example[!ind]))