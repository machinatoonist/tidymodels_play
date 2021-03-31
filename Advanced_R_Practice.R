library(rlang)

# Advanced R with Hadley Wickham
# https://adv-r.hadley.nz/
# See also https://r4ds.had.co.nz/
x <- c(2.1, 3.4, 4.2, 5.1)
y <-  x

x
x[]

x(1)
x(c(1,1))
x[c(1,1)]
y[c(1,3)]
x

# Check environment properties ----
env_parent()
global_env()
current_env()
identical(global_env(), current_env())

identical(x, y)
x == y
y
x

y[1]
y[c(1)]
y[1] * x

x[c(4,3,2,1)]
x[order(x)]
x[order(-x)]

# Remove the 1st and 4th elements of vector ----
x[-c(1,4)]

z <- c(x, y)

z

z <- data.frame(x, y)
z
?data.frame

# Detecting NAs with logicals ----
m <- c(NA, 4, NA, 9)

is.na(m)
!is.na(m)

# return values that are not NA


