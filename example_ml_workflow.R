# Tidy models workflow example
# Matt Rosinski
# Reference: https://supervised-ml-course.netlify.app/chapter1
# 
library(tidymodels)
library(modeldata)
library(skimr)
library(tidyverse)
library(vroom)

data("Sacramento")

# Data used in MS Machine Learning Training material for Azure
data_tbl <- vroom::vroom("~/Documents/github/Azure ML/data/diabetes.csv")

glimpse(data_tbl)

# data_tbl <- Sacramento

response_var <- "Diabetic"
strata <- response_var
# response_var <- quo(response_var)
explanatory_var <- names(data_tbl %>% select(-all_of(response_var)))

# Need to figure out how to put vector value into ggplot

# cars2018 <- read_csv("data/cars2018.csv")

# Visualise data ----
# Print the cars2018 object
glimpse(data_tbl)



# Plot the histogram
plot_histogram <- function(df, column){
  q <- rlang::enexpr(column)
  col_string <- rlang::as_string(q)
  col_string <- str_to_title(str_replace(col_string, pattern = "_", " "))
  column <- enquo(column)
  
  ggplot(df, aes(x = !!column)) +
  geom_histogram(bins = 25) +
  labs(x = col_string,
       y = "Number")
}

plot_histogram(data_tbl, Diabetic)

skim(data_tbl)

# car_train <- readRDS("data/c1_train_10_percent.rds")

lm_mod <- linear_reg() %>%
  set_engine("lm")

rf_mod <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression")

logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")



# Decision Tree Model Specification (classification)
# Computational engine: C5.0
decision_tree() %>%
  set_engine("C5.0") %>%
  set_mode("classification")

# Data cleansing ----
# Deselect any columns to exclude eg: remove characters for lm()
# Random Forest can not handle categorical predictors with more than 53 categories
# Sacramento data cleansing
# Removed cases with only one example from city
# data_vars <- data_tbl %>% 
#   select(-zip) %>% 
#   group_by(city) %>%
#   mutate(count = n()) %>% 
#   ungroup() %>%
#   filter(count > 1) %>%
#   select(-count, -type)
#   # select(-(count(city) %>% filter(n == 1) %>% pull(city)))
# #   select(where(is.numeric))

# Data cleansing for diabetes dataset
data_vars <- data_tbl %>%
  select(-PatientID) %>%
  mutate(Diabetic = factor(Diabetic))

#   
data_vars %>% glimpse()


# Regress Workflow --------------------------------------------------------


# Fit a linear model to all data 

# fit_all <- lm(price ~ ., data = data_vars) ## * bang bang here ?? ----

fit_linear <- function(data_tbl, response) {
  # response <- enquo(response)
  q <- rlang::enexpr(response)
  col_string <- rlang::as_string(q)
  # col_string <- str_to_title(str_replace(col_string, pattern = "_", " "))
  formula <- str_c(col_string,"~ .")
  lm(formula, data = data_tbl) 
  
}
# response_var
# str_c(response_var,"~ .")

fit_all <- lm(Diabetic ~., data_vars)

fit_all <- fit_linear(data_vars, response_var)  # Doesn't work

# Print the summary of the model
summary(fit_all)

# Split data ----
# Split the data into training and test sets
# set.seed(42)
# splits <- data_vars %>%
#   initial_split(prop = 0.8, strata = city)  # substitute with response_var

split_data_tbl <- function(data, strata, prop = 0.8){
  
  strata <- enquo(strata)
  set.seed(42)
  splits <- data %>%
    initial_split(prop = prop, strata = all_of(!!strata))
}

splits <- split_data_tbl(data_vars, response_var)

data_train <- training(splits)
data_test <- testing(splits)

glimpse(data_train)
glimpse(data_test)
# 
# car_train <- readRDS("data/c1_train.rds")
# car_test <- readRDS("data/c1_test.rds")

# Train models ----
# Build a linear regression model specification
lm_mod <- linear_reg() %>%
  set_engine("lm")

# Train a linear regression model
fit_lm <- lm_mod %>%
  fit(log1p(response_var) ~ ., 
      data = data_train)

# Print the model object
fit_lm


# Build a random forest model specification
rf_mod <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("classification")

# Train a random forest model
fit_rf <- rf_mod %>%
  fit(Diabetic ~ ., 
      data = data_train)

# Print the model object
fit_rf

# Evaluate model performance ----

# Create the new columns for model predictions
results <- data_train %>%
  # mutate(price = log(price)) %>%
  # bind_cols(predict(fit_lm, data_train) %>%
  #             rename(.pred_lm = .pred)) %>%
  bind_cols(predict(fit_rf, data_train) %>%
              rename(.pred_rf = .pred_class))

# Evaluate the performance on training data
metrics(results, truth = response_var, estimate = .pred_lm)
metrics(results, truth = response_var, estimate = .pred_rf)

# Evaluate performance on testing data
# Create the new columns
results <- data_test %>%
  # mutate(price = log(price)) %>%
  # bind_cols(predict(fit_lm, data_test) %>%
  #             rename(.pred_lm = .pred)) %>%
  bind_cols(predict(fit_rf, data_test) %>%
              rename(.pred_rf = .pred_class))

# Evaluate the performance
metrics(results, truth = response_var, estimate = .pred_lm)
metrics(results, truth = response_var, estimate = .pred_rf)

# Create bootstrap resamples ----
data_boot <- bootstraps(data_train)

# Evaluate the models with bootstrap resampling
lm_res <- lm_mod %>%
  fit_resamples(
    log(price) ~ .,
    resamples = data_boot,
    control = control_resamples(save_pred = TRUE)
  )

rf_res <- rf_mod %>%
  fit_resamples(
    # log(response_var) ~ .,
    Diabetic ~ .,
    resamples = data_boot,
    control = control_resamples(save_pred = TRUE)
  )

glimpse(rf_res)

# lm_res <- readRDS("data/c1_lm_res.rds")
# rf_res <- readRDS("data/c1_rf_res.rds")

results <-  bind_rows(
  # lm_res %>%
  #   collect_predictions() %>%
  #   mutate(model = "lm"),
  rf_res %>%
    collect_predictions() %>%
    mutate(model = "rf"))

glimpse(results)

# lm_res <- readRDS("data/c1_lm_res.rds")
# rf_res <- readRDS("data/c1_rf_res.rds")

# Plot predictions versus actuals ----
# Bootstrap results
results %>%
  ggplot(aes(`log(price)`, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(aes(color = id), size = 1.5, alpha = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  facet_wrap(~ model)

# Compare models with test dataset

test_results <-  bind_rows(data_test %>%
                        collect_predictions() %>%
                        mutate(model = "lm"),
                        data_test %>%
                        collect_predictions() %>%
                        mutate(model = "rf"))

glimpse(results)