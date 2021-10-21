# Tutorial exercises

# section 00
# install.packages("dslabs")
library(dslabs)

# ---

# section 01
# 1
n <- 100
formula_100 <- (n * (n + 1) / 2)

# 2
n <- 1000
formula_1000 <- (n * (n + 1) / 2)

# 3
n <- 1000
x <- seq(1,n)
summed <- sum(x)            # == n * (n + 1) / 2
# Answer to question: b (+1)

# 4
v1 <- log(sqrt(100), base = 10)
v2 <- log10(sqrt(100))

# 5
# Answer: c (+1)

# 6
x <- seq(1,100)
summed_arr <- sum(x ^ (-2))
form_pi <- (pi ^ 2) / 6

# ---

# section 02
# 1
data(murders)
str(murders)
# Answer: c (+1)

# 2
name_m <- names(murders)
name_col_m <- colnames(murders)     # specification sometimes needed, i.e. is rows and cols have names

# 3
a <- murders$abb
class_a <- class(a)

# 4
num_reg <- length(levels(murders$region))

# 5
x <- c('DC', 'Alabama', 'Florida', 'Florida', 'DC', 'DC')
rep_x <- table(x)
rep_reg <- table(murders$region)

# ---

# section 03
# 1
temp <- c(35,88,42,84,81,30)

# 2
city <- c('Beijing', 'Lagos', 'Paris', 'Rio de Janeiro', 'San Juan', 'Toronto')

# 3
names(temp) <- city

# 4
first_3 <- temp[1:3]

# 5
spec_city <- temp[c('Paris', 'San Juan')]

# 6
arr <- 12:73

# 7
odd_up_to_100 <- seq(1,100,2)

# 8
vec <- seq(6,55,4/7)
numbers_in_vec <- length(vec)

# 9
class(seq(1,10,0.5))
# Answer: `numeric` (+1)

# 10
class(seq(1,10))
# Answer: `integer` (+1)

# 11
class(a<-1)
class(a<-1L)

# 12
x <- c("1", "3", "5")
casted_x <- as.integer(x)