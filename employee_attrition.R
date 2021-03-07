# TIDYMODELS WORKFLOW - REGRESSION ----
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


