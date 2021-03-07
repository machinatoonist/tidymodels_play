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
# data("attrition")  # Classification
data("ames")

ames %>%
  glimpse()

# 2.0 CREATE TRAINING AND TESTING DATASETS ----
# Use rsample package to 
# Create a data split object using the Ames dataset


home_split <- initial_split(ames, 
                            prop = 0.7, 
                            strata = Sale_Price)

# Create the training dataset
home_training <- home_split %>%
  training()

# Create a test dataset
home_test <- home_split %>% 
  testing()

# Check number of rows in each dataset
nrow(home_training)
nrow(home_test)

# 3.0 COMPARE DATA IN TRAINING AND TEST SETS ----
# Distribution of Sale_Price in training data
home_training %>% 
  summarize(min_sell_price = min(Sale_Price),
            max_sell_price = max(Sale_Price),
            mean_sell_price = mean(Sale_Price),
            sd_sell_price = sd(Sale_Price))

# Distribution of Sale_Price in test data
home_test %>% 
  summarize(min_sell_price = min(Sale_Price),
            max_sell_price = max(Sale_Price),
            mean_sell_price = mean(Sale_Price),
            sd_sell_price = sd(Sale_Price))

# 4.0 DEFINE AND TRAIN MODELS ----
# Initialize a linear regression object, linear_model
linear_model <- linear_reg() %>% 
  # Set the model engine
  set_engine("lm") %>% 
  # Set the model mode
  set_mode("regression")

# Train the model with the training data
lm_fit <- linear_model %>% 
  fit(Sale_Price ~ 
        Longitude + Latitude + Lot_Area + Lot_Frontage + 
        Year_Built + Year_Sold + Bedroom_AbvGr,
      data = home_training)

# Print lm_fit to view model information
lm_fit

# View model parameters
lm_fit %>% tidy()

# 5.0 MAKE PREDICTIONS ----
# Predict selling_price
home_predictions <- predict(lm_fit,
                            new_data = home_test)

# View predicted selling prices
home_predictions

home_test %>% glimpse()

# Combine test data with predictions
home_test_results <- home_test %>% 
  select(Sale_Price, Latitude, Longitude, Lot_Area, 
         Lot_Frontage, Year_Built, Year_Sold, Bedroom_AbvGr) %>% 
  bind_cols(home_predictions)

# View results
home_test_results

# 6.0 EVALUATE MODEL FIT ----
home_test_results %>%
  ggplot(aes(x = Sale_Price, y = .pred)) +
  geom_point() +
  labs(title = "Comparison of Predicted Sales Prices to Actual on Test Data")

# Calculate the RMSE metric
home_test_results %>% 
  rmse(Sale_Price, .pred)

# Calculate the R squared metric
home_test_results %>% 
  rsq(Sale_Price, .pred)

# Calculate the MAE metric
home_test_results %>% 
  mae(Sale_Price, .pred)

# Custom metrics function
custom_metrics_reg <- metric_set(mae, rmse, rsq)

home_test_results %>% 
  custom_metrics_reg(Sale_Price, .pred)


# Create an R squared plot of model performance
ggplot(home_test_results, aes(x = Sale_Price, y = .pred)) +
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(title = "R squared plot for evaluating model performance",
       x = 'Actual Home Selling Price', 
       y = 'Predicted Selling Price')

home_test_results

# 7.0 REFIT MODEL TO DATA ----

# Train linear_model with last_fit()
linear_fit <- linear_model %>% 
  last_fit(Sale_Price ~ ., split = home_split)

# Collect predictions and view results
predictions_df <- linear_fit %>% collect_predictions()
predictions_df

# Make an R squared plot using predictions_df
ggplot(predictions_df, aes(x = Sale_Price, y = .pred)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(title = "R squared plot for evaluating model performance",
       x = 'Actual Home Selling Price', y = 'Predicted Selling Price')

?predict
