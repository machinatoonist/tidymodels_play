# TIDYMODELS WORKFLOW - REGRESSION ----
# 
# * Definitions/Reminders: ----
# * Accuracy = (TP + TN)/(TP + FP + TN + FN)
# * Selectivity = TN/(TN + FN)
# * False Positive Rate (FPR) = 1 - Selectivity (type I error)
# * Sensitivity =  TP/(TP + FP)
# * False Negative Rate (FNR) = 1 - Sensitivity (type II error)
# 
# * Search parsnip models ----
# https://www.tidymodels.org/find/parsnip/
# 
# 1.0 LOAD LIBRARIES ----
library(tidymodels)

library(modeldata)
# https://cran.r-project.org/web/packages/modeldata/modeldata.pdf
# data("mpg")
# data("drinks")  # Time series data
# data("okc")
# data("Puromycin")
# data("txhousing")
data("attrition")  # Classification
# data("ames")

attrition %>%
  glimpse()

# 2.0 CREATE TRAINING AND TESTING DATASETS ----
# Use rsample package to 
# Create a data split object using the Ames dataset


employee_split <- initial_split(attrition, 
                            prop = 0.75, 
                            strata = Attrition)

# Create the training dataset
employee_training <- employee_split %>%
  training()

# Create a test dataset
employee_test <- employee_split %>% 
  testing()

# Check number of rows in each dataset
nrow(employee_training)
nrow(employee_test)

# 3.0 COMPARE DATA IN TRAINING AND TEST SETS ----
# Distribution of attrition in training data
employee_training %>% 
  count(Attrition) %>%
  mutate(pct = round(n/sum(n)*100, 3))

employee_test %>% 
  count(Attrition) %>%
  mutate(pct = round(n/sum(n)*100, 3))

# 4.0 DATA PRE-PROCESSING RECIPE WORKFLOW ----

employee_log_rec <- recipe(Attrition ~ ., 
                          data = employee_training) %>%
  # Add log transformation step
  step_log(MonthlyRate, base = 10)

# View variable roles and data types
employee_log_rec %>%
  summary() 

# Train the telecom_log_rec object
employee_log_rec_prep <- employee_log_rec %>% 
  prep(training = employee_training)

# View results
employee_log_rec_prep

# Apply to training data
employee_log_rec_prep %>% 
  bake(new_data = NULL)

# The recipe, prep and bake steps can be combined to define the transformed
# dataset
employee_prep_tbl <- recipe(Attrition ~ ., 
                           data = employee_training) %>%
  # Add log transformation step
  step_log(MonthlyRate, base = 10) %>%
  prep() %>% bake(new_data = employee_training)

# Test for multi-collinearity
employee_training %>%
  select_if(is.numeric) %>%
  cor() %>% View()

# * Final recipe ----
prep_tbl <- recipe(Attrition ~ ., 
       data = attrition) %>%
  # Remove collinear predictors (omit outcomes with -all_outcomes() if numeric)
  step_corr(all_numeric(), threshold = 0.9) %>%
  # Normalise - also try step_normalize()
  step_normalize(all_numeric()) %>%
  # step_center(all_numeric()) %>%
  # step_scale(all_numeric()) %>%
  # Add log transformation step for variable that are asymmetric
  # step_log(MonthlyRate, base = 10) %>%
  prep() %>% bake(new_data = attrition)

prep_tbl_split <- initial_split(prep_training_tbl, 
                                prop = 0.75, 
                                strata = Attrition)

# 4.0 DEFINE AND TRAIN MODELS ----
# Specify a logistic regression model
logistic_model <- logistic_reg() %>% 
  # Set the engine
  set_engine('glm') %>% 
  # Set the mode
  set_mode('classification')

# Fit to training data
logistic_fit <- logistic_model %>% 
  fit(Attrition ~ OverTime +
        JobInvolvement + WorkLifeBalance + StockOptionLevel,
      data = employee_training)

# Print model fit object
logistic_fit


# View model parameters
logistic_fit %>% tidy()

# 5.0 MAKE PREDICTIONS ----
# Predict employee attrition
attrition_predictions <- predict(logistic_fit,
                            new_data = employee_test)

# View predicted selling prices
attrition_predictions

employee_test %>% glimpse()

# Predict outcome categories
class_preds <- predict(logistic_fit, new_data = employee_test,
                       type = "class")

# Obtain estimated probabilities for each outcome value
prob_preds <- predict(logistic_fit, new_data = employee_test, 
                      type = 'prob')

# Combine test set results
attrition_results <- employee_test %>% 
  select(Attrition) %>% 
  bind_cols(class_preds, prob_preds)

# View results tibble
attrition_results


# 6.0 EVALUATE MODEL FIT ----
# 
# Confusion matrix calculation
attrition_results %>%
conf_mat(truth = Attrition, estimate = .pred_class) %>%
  summary()

attrition_results %>%
  conf_mat(truth = Attrition, estimate = .pred_class) %>%
  autoplot("mosaic")

# Accuracy
attrition_results %>%
  accuracy(truth = Attrition, estimate = .pred_class)

# Sensitivity: Proportion of all positive cases that were correctly classified
attrition_results %>%
  sens(truth = Attrition, estimate = .pred_class)

# Specificity: Proportion of all negative cases that were correctly classified
# False positive rate (FPR) = 1 - specificity (proportion of false positives 
# among all true negatives)
# The rate of employees predicted to leave that did not
attrition_results %>%
  spec(truth = Attrition, estimate = .pred_class)

# Custom metrics
custom_metrics <- metric_set(accuracy, sens, spec)

?metric_set

attrition_results %>%
  custom_metrics(truth = Attrition, estimate = .pred_class)

#     pos neg
# pos 30  20
# neg 10  40
# 
# accuracy <-  (30 + 40)/(100)
# sensitivity <- 30/40
# specificity <- 40/60
# Calculate the accuracy
# 
#Receiver Operator Curve
#ROC
attrition_results %>%
  roc_curve(truth = Attrition, .pred_No) %>% 
  autoplot()
# The initial model performs poorly
attrition_results %>%
  roc_auc(truth = Attrition, .pred_No)

attrition_results %>%
  conf_mat(truth = Attrition, estimate = .pred_class) %>%
  autoplot("mosaic")


# 7.0 REFIT MODEL TO DATA ----

# Train logistic model with last_fit()
# Add all parameters into the model
logistic_last_fit <- logistic_model %>% 
  last_fit(Attrition ~ ., split = employee_split)

# Collect predictions and view results
predictions_df <- logistic_last_fit %>% 
  collect_predictions()

predictions_df %>% glimpse()

predictions_df %>% 
  roc_curve(truth = Attrition, .pred_No) %>% 
  autoplot()

predictions_df %>% 
  roc_auc(truth = Attrition, .pred_No)

# View test set metrics
logistic_last_fit %>% 
  collect_metrics()

# Custom metrics function
last_fit_metrics <- metric_set(accuracy,sens, spec, roc_auc)

# Calculate metrics
last_fit_metrics(predictions_df,
                 truth = Attrition,
                 estimate = .pred_class,  
                 .pred_No)

predictions_df %>% glimpse()

# Create a confusion matrix
conf_mat(predictions_df,
         truth = Attrition,
         estimate = .pred_class) %>% 
  # Create a mosaic plot
  autoplot(type = "mosaic")

# Final refit on final recipe ----

prep_logistic_fit <- logistic_model %>% 
  last_fit(Attrition ~ ., split = prep_tbl_split)

# Collect predictions and view results
prep_predictions_df <- prep_logistic_fit %>% 
  collect_predictions()

prep_predictions_df %>% 
  roc_curve(truth = Attrition, .pred_No) %>% 
  autoplot()

# Calculate metrics
last_fit_metrics(prep_predictions_df,
                 truth = Attrition,
                 estimate = .pred_class,  
                 .pred_No)


# 8.0 CROSS VALIDATION ----
# 
# Create cross validation folds
# * Define folds and metrics ----
set.seed(290)

employee_folds <- vfold_cv(employee_training, v = 5,
                        strata = Attrition)

# Create custom metrics function
employee_metrics <- metric_set(accuracy, sens, spec, roc_auc)

# * Decision Tree CV ----
# 
# parsnip_addin()
# # install.packages(c("shiny", "miniUI", "rstudioapi"))
# install.packages("parsnip_addin")

dt_model <- decision_tree() %>% 
  # Specify the engine
  set_engine('rpart') %>% 
  # Specify the mode
  set_mode('classification')

# Build feature engineering pipeline
employee_recipe <- recipe(Attrition ~ .,
                       data = employee_training) %>% 
  # Correlation filter
  step_corr(all_numeric(), threshold = 0.85) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric()) %>% 
  # Create dummy variables
  step_dummy(all_nominal(), -all_outcomes())

# Create a workflow
employee_dt_wkfl <- workflow() %>% 
  # Include the model object
  add_model(dt_model) %>% 
  # Include the recipe object
  add_recipe(employee_recipe)

# Fit resamples
employee_dt_rs <- employee_dt_wkfl %>% 
  fit_resamples(resamples = employee_folds,
                metrics = employee_metrics)

# View performance metrics
employee_dt_rs %>% 
  collect_metrics()

# Detailed cross validation results
dt_rs_results <- employee_dt_rs %>% 
  collect_metrics(summarize = FALSE)

# Explore model performance for decision tree
dt_rs_results %>% 
  group_by(.metric) %>% 
  summarize(min = min(.estimate),
            median = median(.estimate),
            max = max(.estimate))

# Set tuning hyperparameters
dt_tune_model <- decision_tree(cost_complexity = tune(),
                               tree_depth = tune(),
                               min_n = tune()) %>% 
  # Specify engine
  set_engine('rpart') %>% 
  # Specify mode
  set_mode('classification')

# Create a tuning workflow
employee_tune_wkfl <- employee_dt_wkfl %>% 
  # Replace model
  update_model(dt_tune_model)

employee_tune_wkfl

# * Logistic Model CV ----

logistic_model <- logistic_reg() %>% 
  # Specify the engine
  set_engine('glm') %>% 
  # Specify the mode
  set_mode('classification')

# Create workflow
employee_logistic_wkfl <- workflow() %>% 
  # Add model
  add_model(logistic_model) %>% 
  # Add recipe
  add_recipe(employee_recipe)

# Fit resamples
employee_logistic_rs <- employee_logistic_wkfl %>% 
  fit_resamples(resamples = employee_folds,
                metrics = employee_metrics)

# View performance metrics
employee_logistic_rs %>%
  collect_metrics()

# Detailed cross validation results
logistic_rs_results <- employee_logistic_rs %>% 
  collect_metrics(summarize = FALSE)

# Explore model performance for logistic regression
logistic_rs_results %>% 
  group_by(.metric) %>% 
  summarize(min = min(.estimate),
            median = median(.estimate),
            max = max(.estimate))
