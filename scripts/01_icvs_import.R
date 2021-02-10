# Import and sav_clean data with janitor()

library(haven) # haven for reading .sav file
library(janitor)

library(tidyverse)
library(dplyr) # for glimpse and filter functions
library(sjlabelled)
library(kableExtra)

rawest_data <- read_sav("C:/Users/dalla/Google Drive/R Coding/ICVS2005_3.sav")  # Reading data

jan_data <- rawest_data %>% 
  mutate_all(as_factor) %>%   # convert all variables from chr+lbl to fct type          
  set_names(rawest_data %>%       # convert variable names to informative labels
              sjlabelled::get_label() %>% # pull labels from original tibble
              enframe() %>%               # convert list of column names to a new tibble
              na_if("") %>%               # if variable label is empty string, convert to NA
              mutate(value = coalesce(value, name)) %>%  # fill in NA with original if missing
              pull(value)) %>%            # extract new variable name into a character vector
  janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"     

head(jan_data)

write.csv(jan_data, file="C:/Users/dalla/Google Drive/R Coding/icvs_inequality/data/ICVS_sav_import.csv")


raw_data <- jan_data
