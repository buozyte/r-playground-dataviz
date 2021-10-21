# Tutorial exercises

# section 00
# install.packages("dslabs")
library(dslabs)

# ---

# section 01
# 1
n <- 100
print(n * (n + 1) / 2)

# 2
n <- 1000
print(n * (n + 1) / 2)

# 3
n <- 1000
x <- seq(1,n)
print(sum(x))            # == n * (n + 1) / 2
# Answer to question: b

# 4
log(sqrt(100), base = 10)

# 5
# Answer: c

# 6
x <- seq(1,100)
sum(x ^ (-2))
(pi ^ 2) / 6

# ---

# section 02
# 1
data(murders)
str(murders)
# Answer: c

# 2
names(murders)

# 3
a <- murders$abb
class(a)

# 4
length(levels(murders$region))

# 5
x <- c('DC', 'Alabama', 'Florida', 'Florida', 'DC', 'DC')
table(x)
table(murders$region)

# ---

# section 03
# 1
temp <- c(35,88,42,84,81,30)

# 2
city <- c('Beijing', 'Lagos', 'Paris', 'Rio de Janeiro', 'San Juan', 'Toronto')

# 3
names(temp) <- city

# 4
temp[1:3]

# 5
temp[c('Paris', 'San Juan')]

# 6
12:73

# 7
seq(1,100,2)

# 8
vec <- seq(6,55,4/7)
length(vec)

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
as.integer(x)