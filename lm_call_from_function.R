library(tidyverse)
data("mtcars")
data("mpg")

str(mtcars)

fit_lm <- lm(mpg ~.,data =  mtcars)

predictions_1 <- predict(fit_lm,newdata = NULL)

mtcars %>%
  bind_cols(predictions = predictions_1)


# Create a function to call lm with inputs supplied indirectly --------

# fit_linear <- function(data, response) {
#   q <- rlang::enexpr(response)
#   col_string <- rlang::as_string(q)
#   formula <- str_c(col_string,"~ .")
#   lm(formula, data = data) 
# }


fit_all <- fit_linear(mtcars, "mpg") 

predictions_2 <- predict(fit_all,newdata = NULL)

# Compare the results of direct and indirect calculation ------------------

mtcars %>%
  bind_cols(predict_direct = predictions_1,
            predict_indirect = predictions_2
            ) %>%
  select(contains("pred"))

# lm() function now works in a pipe
mtcars %>%
  fit_linear(mpg)


# Test function when variables are defined as strings ---------------------

fit_linear_1 <- function(data, response) {
  
  if (is.character(response)){
    formula <- as.formula(str_c(response,"~ ."))
    lm(formula, data = data)
    #
    } else {
      formula <- as.formula(str_c({{response}},"~ ."))
      lm(formula, data = data)
  # } else {
  #   q <- rlang::enexpr(response)
  #   col_string <- rlang::as_string(q)
  #   formula <- str_c(col_string,"~ .")
  #   lm(formula, data = data) 
    }
}

fit_linear_2 <- function(data, response) {

    q <- rlang::enexpr(response)
    col_string <- rlang::as_string(q)
    formula <- str_c(col_string,"~ .")
    lm(formula, data = data) 
}

data("mtcars")
variable <- "mpg"
data_tbl <- mtcars
data_tbl %>% fit_linear_1(mpg) # Fails with error: Error: Can't convert a `tbl_df/tbl/data.frame` object to a string
data_tbl %>% fit_linear_1("mpg") # Works
data_tbl %>% fit_linear_1(variable) # Works

data_tbl %>% fit_linear_2(mpg) # Works
data_tbl %>% fit_linear_2("mpg") # Works
data_tbl %>% fit_linear_2(variable) # Fails
data_tbl %>% fit_linear_2(!!variable) # Works 



fit_linear(data_tbl, "mpg")


# Understand environments in R --------------------------------------------

e <- new.env()
e$x <- 4
x

eval(quote(3+x))
y <- 3

eval(quote(3+y))

eval(quote(3 + x), envir = e)

# Formulae capture an expression and it's environment
# Tilde is a way of quoting something
~ 3 + 3

# Like this
quote(3 + 3)

form <- ~ 3 + x
form
form[[1]] # The tilde part
typeof(form)
str(form)

form[[2]] # The quoted expression part
environment(form)  # Default environment is Global
e
e$x
x
3+x  # Does not work because the value of x is stored in a separate environment

e$x <- 3
environment(form) <- e
eval(expr = form[[2]], envir = environment(form))  # Allows for x to be used in the expression

eval(expr = form[[2]]) # Does not work because x is not stored in the global environment


# Using a data mask to supercede the global environment -------------------

# Using _at suffixes in tidy verbs ----------------------------------------
?vars()

data %>% glimpse()

col_names <- data %>% names()

data %>% 
  summarise_at(
    vars(col_names),
    ~ mean(., na.rm = TRUE)
  )

summary_functions <-  list(
  mean = ~ mean(., na.rm = TRUE),
  sd = ~ sd(., na.rm = TRUE))

data_tbl %>% 
  summarise_at(vars(col_names),summary_functions)

# summarise_at() has been superseded with the use of across()
data_tbl %>% summarise(across(col_names, ~ mean(., na.rm = TRUE)))
data_tbl %>% summarise(across(col_names, summary_functions))

summarise_table <- function(.data, ...) {
  
  # Define functions to use in summary
  summary_functions <-  list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE))
  
  # Select all names in data frame
  col_names <- data %>% names()
  
  data %>% summarise(across(col_names, summary_functions))
}

summarise_table(data)

# This function should work with groups
