library(tidymodels)
library(tidyverse)
nepalA <-  read_csv("nepal_anthro.csv")

nepalData = nepalA %>%filter(age <= 12) %>% ### keep only children with age <= 12
  filter(!is.na(height), !is.na(weight), !is.na(armcirc)) ### remove NAs
nepalData = nepalData %>%
  mutate(gender = recode_factor(sex, `1`="Male", `2`="Female"))





linear_spec <-   linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")
linear_spec
class(linear_spec)

recipe_spec <- recipe(armcirc ~ age + weight + height,data = nepalData) %>% 
  step_normalize(all_predictors(),-all_outcomes())

recipe_spec

linear_workflow <- workflow() %>% 
  add_recipe(recipe_spec) %>% 
  add_model(linear_spec) %>% 
  fit(data=nepalData)

linear_workflow %>% tidy()
linear_workflow


library(caTools)




