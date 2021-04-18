
# Sample data -------------------------------------------------------------

library(datasets)
data("mpg")
glimpse(mpg)

# User supplies a string and function takes a quote and string -----------------------

my_func <- function(data, column_1, column_2){
  ex1 <- rlang::parse_expr(column_1)
  ex2 <- rlang::parse_expr(column_2)
  return
    (ggplot(data = data,
           aes(x = !!ex1, y = !!ex2)) + 
      geom_point()) +
    xlab(column_1) +
    ylab(column_2)
  
}

variables <- c("cty", "hwy")

var_quote <- exprs(cty, hwy)

my_func(mpg, "cty", "hwy")  # works
my_func(mpg, cty, hwy) # fails
my_func(mpg, variables) # fails
data <- mpg


# User inputs a quote and string and the function uses a quote ------------

my_select <- function(data, ...) {
  # cols <- rlang::enexprs(...)  # Creates a list
  cols <- rlang::enquos(...)
  vars <- as.list(set_names(seq_along(data), names(data)))
  cols_char <- purrr::map(cols, rlang::eval_tidy, vars)
  # browser()
  idx <- purrr::map_int(cols_char, function(x) {
    ifelse(is.character(x),vars[[x]],x)})
  # cols_char <- as.vector(cols, mode = "character")
  # idx <- which(names(data) %in% as.character(cols_char)) # Does not match order
  # idx <- match(cols_char, names(data)) # Use match to get same order of columns
  return(
    data[,idx, drop = FALSE]
  )
}
variables <- c("cty", "hwy")
my_select(mpg, variables, "year", trans)  # Works for mixed inputs
my_select(mpg, "cty") # Works
my_select(mpg, variables) # Works

my_select(mpg, !!!variables)  # works for single and multiple columns
my_select(mpg, var_quote)   # works

my_select(mpg, cty, hwy) # Works
my_select(mpg, hwy, cty) # Works
select(mpg, cty, hwy) # Compare with dplyr

mpg %>%
  my_select(c("cty", "hwy"))

mpg %>%
  select(c("cty", "hwy"))
