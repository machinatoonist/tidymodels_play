# STACK OVERFLOW SURVEY
# Working with text data
library(tidymodels)
library(skimr)
library(recipes)
library(themis)
library(vroom)
# install.packages("vroom")

stack_overflow <- read.csv("data/survey_results_public.csv")
stack_overflow_2019 <-  read.csv("data/survey_results_public_2019.csv")
stack_overflow_2019 <- vroom::vroom("data/survey_results_public_2019.csv", col_names = TRUE)
schema <-  read.csv("data/survey_results_schema.csv")

glimpse(stack_overflow_2019)
glimpse(schema)

stack_overflow_2019 %>%
  count(WorkRemote, sort = TRUE)

stack_overflow <- stack_overflow_2019 %>%
  drop_na(WorkRemote) %>%
  mutate(remote = case_when(WorkRemote == "All or almost all the time (I'm full-time remote)" ~
                            "remote", 
                            WorkRemote == "Less than once per month / Never" ~ "not remote",
                            TRUE ~ "mix"))

stack_overflow %>% 
  count(remote, sort = TRUE)


skim(stack_overflow_2019)

ggplot(stack_overflow, aes(OpSys, YearsCodePro)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience")

# stack_overflow %>% select(MiscTechWorkedWith)
  # str_split(MiscTechWorkedWith,pattern = ";")
  

  
  # stack_train <- readRDS("data/c2_train.rds")
  
  stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
    step_downsample(remote)
  
  stack_recipe
