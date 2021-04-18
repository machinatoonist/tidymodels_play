# Screen 459.3: A Different App Structure
# In the line below, import the shiny library so that it's available
# in both ui.R and server.R
library(shiny)
library(tidyverse)


# Screen 459.5: Data Introduction And Cleaning
# In the lines below, import the hospital_los.csv file and process it according
# to the screen instructions. Categorical variables should be converted into 
# factor
heart <- read_csv("www/heart.csv") %>% 
  mutate(
    sex = factor(sex),
    cp = factor(cp),
    restecg = factor(restecg),
    exang = factor(exang),
    ca = factor(ca),
    thal = factor(thal)
  )
