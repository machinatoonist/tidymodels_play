# Reference Source: https://tmv.netlify.app/

# Find models at https://www.tidymodels.org/find/parsnip/

library(tidyverse)
library(tidymodels)
library(modeldata)
library(kknn)
library(rpart)
library(rpart.plot)
library(rattle)
library(vip)
library(ranger)
library(partykit)
# install.packages("C50")
# install.packages("tune")
# install.packages("finetune")

set.seed(100)

# Data from a clinical trial of individuals with well-characterized cognitive impairment, 
# and age-matched control participants

data("ad_data")
alz <- ad_data

alz %>% glimpse()

glm(Class ~ tau, family = binomial, data = alz)

alz %>%
  select(Class, tau)

# 1. PICK A MODEL
# 2. SET THE ENGINE
# 3. SET THE MODE (IF NEEDED)
# E.G. # Logistic Regression Model Specification (classification)
# Computational engine: glm
# Adds an engine to power or implement the model.
# Sets the class of problem the model will solve, 
# which influences which output is collected. 
# Not necessary if mode is set in Step 1.
logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")



# Decision Tree Model Specification (classification)
# Computational engine: C5.0
decision_tree() %>%
  set_engine("C5.0") %>%
  set_mode("classification")

nearest_neighbor() %>%              
  set_engine("kknn") %>%             
  set_mode("classification")        
# K-Nearest Neighbor Model Specification (classification)
# Computational engine: kknn

# Specify a model that uses logistic regression
logistic_reg(
  mode = "classification", # "default" mode, if exists
  penalty = NULL,          # model hyper-parameter
  mixture = NULL           # model hyper-parameter
)

tree_mod <- decision_tree() %>%
  set_engine("C5.0") %>%
  set_mode("classification")
tree_mod

lr_mod <- tree_mod %>% 
  fit(Class ~ tau + VEGF, 
      data = alz)

class(lr_mod)
lr_mod

alz_new <- 
  tibble(tau = c(5, 6, 7), 
         VEGF = c(15, 15, 15),
         Class = c("Control", "Control", "Impaired")) %>% 
  mutate(Class = factor(Class, levels = c("Impaired", "Control")))
alz_new

# Training predictions
tree_mod %>% 
  fit(Class ~ tau + VEGF, 
      data = alz) %>% 
  predict(new_data = alz) %>% 
  mutate(true_class = alz$Class) %>% 
  accuracy(truth = true_class, 
           estimate = .pred_class)

# Testing predictions
tree_mod %>% 
  fit(Class ~ tau + VEGF, 
      data = alz) %>% 
  predict(new_data = alz_new) %>%
  mutate(true_class = alz_new$Class) %>%
  accuracy(truth = true_class, 
           estimate = .pred_class)

# Train a model by fitting a model. Returns a parsnip model fit.
tree_fit <- tree_mod %>%                     # parsnip model
  fit(Class ~ tau + VEGF,        # a formula
      data = alz                 # dataframe
  )


# Create training and test datasets
alz_split <- initial_split(alz, strata = Class, prop = .9)
alz_split

train <- training(alz_split)
test <- testing(alz_split)

tree_fit_new <- tree_mod %>%                     # parsnip model
  fit(Class ~ tau + VEGF,        # a formula
      data = train                 # dataframe
  )

tree_fit_new %>% 
  predict(new_data = test) %>% 
  mutate(true_class = test$Class) %>%
  accuracy(truth = true_class, 
           estimate = .pred_class)

lm_mod <- tree_mod %>%            # parsnip model
  fit(Class ~ tau + VEGF,         # a formula
      data = train                # dataframe
  )

acc <- vector(length = 10, mode = "double")
for (i in 1:10) {
  new_split <- initial_split(alz)
  new_train <- training(new_split)
  new_test  <- testing(new_split)
  acc[i] <-
    lr_mod %>% 
    fit(Class ~ tau + VEGF, data = new_train) %>% 
    predict(new_test) %>% 
    mutate(truth = new_test$Class) %>% 
    accuracy(truth, .pred_class) %>% 
    pull(.estimate)
}

