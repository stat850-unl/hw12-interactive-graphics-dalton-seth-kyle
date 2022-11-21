# Animated and Interactive Graphics

## Link to App: 
https://kdougherty.shinyapps.io/Interactive_Graphics_App/

## Instructions

Use the [TidyTuesday Cocktail Data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-26/readme.md) to create a shiny applet containing the following:

- A visual exploration of the cocktail data
- A way for a user to search through cocktail recipes by ingredient

Your applet should make use of reactivity in both components, but the specifics of what you do and how you do it are up to you.

Once you're satisfied with your applet, upload it to [ShinyApps.io](https://www.shinyapps.io/). You should be able to use the free hosting - you're not creating any high-traffic applications yet. 

You can find the instructions for deploying your application [here](https://docs.rstudio.com/shinyapps.io/getting-started.html#working-with-shiny-for-r).

When you've deployed your application, edit this README to contain a link to your applet.

# Reading in/editing the data 
## Get the Data

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

## Cleaning Script 
library(tidyverse)
library(janitor)

# source for boston drinks
"https://www.kaggle.com/jenlooper/mr-boston-cocktail-dataset"

# Source for drinks
"https://www.kaggle.com/ai-first/cocktail-ingredients"

# Read in the data --------------------------------------------------------

drinks <- read_csv("all_drinks.csv") %>% 
  janitor::clean_names() %>% 
  rename(row_id = x1)

boston_drks <- read_csv("mr-boston-flattened.csv")


# pivot_longer drinks -----------------------------------------------------

drk_ing <- drinks %>% 
  select(row_id:str_iba, contains("ingredient"), str_video) %>% 
  
  # pivot to take wide data to long
  pivot_longer(cols = contains("ingredient"), 
               names_to = "ingredient_number", 
               values_to = "ingredient") %>% 
  # remove text and extract only the digits
  mutate(ingredient_number = str_extract(ingredient_number, "[:digit:]+") %>% 
           as.integer()) %>% 
  # remove "str_" from any of the col names
  set_names(nm = str_remove(names(.), "str_")) 

drk_measure <- drinks %>% 
  # select only the join ids and cols w/ "measure"
  select(row_id, str_drink, id_drink, contains("measure")) %>% 
  # pivot to take wide data to long
  pivot_longer(cols = contains("measure"), 
               names_to = "measure_number", 
               values_to = "measure") %>% 
  # extract just digits
  mutate(measure_number = str_extract(measure_number, "[:digit:]+") %>% 
           as.integer()) %>% 
  # remove str_ from any col names
  set_names(nm = str_remove(names(.), "str_"))

# join the two long dfs back together
all_drks <- left_join(drk_ing, drk_measure, 
                      by = c("row_id", "drink", "id_drink", 
                             "ingredient_number" = "measure_number")) %>% 
  filter(!is.na(measure) & !is.na(ingredient))

# confirm if missing data
# confirm if missing data
anti_join(drk_ing, drk_measure, 
          by = c("row_id", "drink", "id_drink", 
                 "ingredient_number" = "measure_number"))

write_csv(all_drks, "cocktails.csv")

# pivot_longer boston drinks ----------------------------------------------

bs_drk_ing <- boston_drks %>% 
  mutate(row_id = row_number()) %>% 
  select(name, category, row_id, contains("ingredient")) %>% 
  pivot_longer(cols = contains("ingredient"), 
               names_to = "ingredient_number", 
               values_to = "ingredient") %>% 
  mutate(ingredient_number = str_extract(ingredient_number, "[:digit:]+") %>% 
           as.integer())


bs_drk_ms <- boston_drks %>% 
  mutate(row_id = row_number()) %>% 
  select(name, category, row_id, contains("measurement")) %>% 
  pivot_longer(cols = contains("measurement"), 
               names_to = "measure_number", 
               values_to = "measure") %>% 
  mutate(measure_number = str_extract(measure_number, "[:digit:]+") %>% 
           as.integer())

all_bs_drks <- left_join(bs_drk_ing, bs_drk_ms, 
                         by = c("name", "category", "row_id", 
                                "ingredient_number" = "measure_number")) %>% 
  filter(!is.na(ingredient) & !is.na(measure))

# confirm if missing data
anti_join(bs_drk_ing, bs_drk_ms, 
          by = c("name", "category", "row_id", 
                 "ingredient_number" = "measure_number"))

write_csv(all_bs_drks, "boston_cocktails.csv")

## Link to applet 
