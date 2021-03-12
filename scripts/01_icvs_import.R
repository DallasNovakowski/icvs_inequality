# Import and sav_clean data with janitor()

library(haven) # haven for reading .sav file
library(janitor)

library(tidyverse)
library(dplyr) # for glimpse and filter functions

rawest_data <- read_sav("C:/Users/dalla/Google Drive/R Coding/icvs_inequality/data/ICVS2005_3.sav")  # Reading data

raw_classes <- lapply(as.data.frame(lapply(rawest_data, class)),tail,1)

jan_data <- rawest_data %>% 
  mutate_all(as_factor) %>%   # convert all variables from chr+lbl to fct type          
  set_names(rawest_data %>%       # convert variable names to informative labels
              sjlabelled::get_label() %>% # pull labels from original tibble
              enframe() %>%               # convert list of column names to a new tibble
              na_if("") %>%               # if variable label is empty string, convert to NA
              mutate(value = coalesce(value, name)) %>%  # fill in NA with original if missing
              pull(value)) %>%            # extract new variable name into a character vector
  janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"     

jan_names <- names(jan_data)

jan_data1 <- rawest_data %>% 
  set_names(rawest_data %>%       # convert variable names to informative labels
              sjlabelled::get_label() %>% # pull labels from original tibble
              enframe() %>%               # convert list of column names to a new tibble
              na_if("") %>%               # if variable label is empty string, convert to NA
              mutate(value = coalesce(value, name)) %>%  # fill in NA with original if missing
              pull(value)) %>%            # extract new variable name into a character vector
  mutate_all(as.character) %>%   # convert all variables from chr+lbl to fct type   
  janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"     

#attach names and classes to cleaned dataset
class(jan_data1) <- raw_classes
names(jan_data1) <-jan_names


# importing national data
nation_data <- read_csv("C:/Users/dalla/Google Drive/R Coding/icvs_inequality/data/nation_variables.csv")

nation_data$gini <- nation_data$lis_gini_2004_6

nation_data[,c("country","gini", "wb_gini_2004_6")]

# add missing gini values from WB and 2007 LIS
nation_data$gini <- ifelse(nation_data$country == "Argentina"| nation_data$country== "Bulgaria"| nation_data$country == "Portugal", 
                           nation_data$wb_gini_2004_6 -.0233, nation_data$gini)

nation_data$gini <- ifelse(nation_data$country == "Lithuania",.362, ifelse(nation_data$country == "New Zealand",.31, nation_data$gini))

nation_agg <- nation_data  %>%dplyr::select("country", ends_with("2004_6"), ends_with("gini"))

nation_agg_1 = filter(nation_agg, !is.na(lis_gini_2004_6))

nation_agg_2 = filter(nation_agg_1, !is.na(wb_gini_2004_6))

nation_agg_short <- nation_agg[,c("country","pop_2004_6","gni_cap_currppp_2004_6","gdp_cap_currppp_2004_6","gini")]

summary(jan1$country)

# substituting specific british isles regions with UK

jan_data2 <- jan_data

jan_data2$country <- ifelse(jan_data2$country == "England & Wales" | jan_data2$country == "Scotland" | jan_data2$country == "Northern Ireland", 
                            "United Kingdom",jan_data2$country)
  
jan_data2 <- merge(jan_data, nation_agg_short, by="country")

summary(jan_data2[,c("country","pop_2004_6","gni_cap_currppp_2004_6","gdp_cap_currppp_2004_6","gini")])

# check on classes
jan_classes <- lapply(as.data.frame(lapply(jan_data2, class)),tail,1)

raw_data <- jan_data2