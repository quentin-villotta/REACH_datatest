# Load packages
library(tidyr)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(purrr)
library(readr)
library(readxl)
library(lubridate)
library(hrbrthemes)
library(httr)
library(glue)
library(stringr)
# Functions

summarizer_all <- function(data, vars_expr) {
  data %>% 
    summarise(across(.cols = {{vars_expr}}, 
                     .fns = list(count = ~sum(!is.na(.x)),
                                 unique = ~n_distinct(.x),
                                 count_NA = ~sum(is.na(.x)),
                                 pct_NA = ~mean(is.na(.x)) * 100,
                                 mean = ~ifelse(is.numeric(.x), mean(.x, na.rm = TRUE),  NA), 
                                 median = ~ifelse(is.numeric(.x), median(.x, na.rm = TRUE), NA),
                                 var = ~ifelse(is.numeric(.x), var(.x, na.rm = TRUE), NA),
                                 sd = ~ifelse(is.numeric(.x), sd(.x, na.rm = TRUE), NA),
                                 min = ~ifelse(!is.character(.x), min(.x, na.rm = TRUE), NA),
                                 max = ~ifelse(!is.character(.x), max(.x, na.rm = TRUE), NA),
                                 q05 = ~ifelse(is.numeric(.x), quantile(.x, 0.05, na.rm = TRUE), NA),
                                 q95 = ~ifelse(is.numeric(.x), quantile(.x, 0.95, na.rm = TRUE), NA)),
                     .names = "{.col}///{.fn}")) %>% 
    pivot_longer(cols = everything(),
                 names_to = c(".value", "indicator"),
                 names_sep = "///")
}

summarizer <- function(data, numeric_cols = NULL, ...) {
  data %>%
    group_by(...) %>%
    summarise(across({{numeric_cols}}, list(
      unique = ~n_distinct(.x),
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      q05 = ~quantile(.x, 0.05, na.rm = TRUE),
      q95 = ~quantile(.x, 0.95, na.rm = TRUE)
    ), .names = "{col}_{fn}"))
}

# Load data
df_path <- "~/Documents/Projets/R_projects/impact_data_test/data/data_reach.xlsx"
df <- read_excel(path=df_path)


df_sum <- summarizer_all(df, everything())



df %>%
  mutate(sum_house = rowSums(across(c(`Number household member boy under5 years old`, `Number household member _girl_under5 years old`,
                                      `Number household member boy_5_17 years old`, 
                                      `household_girl_5_17`,`number adult household members years old` ))),
         compare = ifelse(sum_house == `Total household number`, TRUE, FALSE)) %>% 
  select(InterviewID, compare, `Total household number`,sum_house) %>% 
  filter(compare == FALSE)


# drinking_water_source

Improved_water_source <- c("Protected dug well" ,"Piped water to yard or plot",
  "Piped water into dwelling (house)","Bottled water", "Tube well or borehole", "Public tap or standpipe",
  "Protected spring")


Unimproved_water_source <- c("Cart with small tank or drum","Unprotected dug wel", "Unprotected spring",
                                 "Surface water", "Tanker-truck", "Rainwater collection")
  
  
  
  
 df<- df %>%
    mutate(improved_water_source = case_when(drinking_water_source %in% Improved_water_source ~ "Improved water source"  ,
                                             drinking_water_source %in% Unimproved_water_source ~ 'Unimproved water source',
                                             drinking_water_source == "Other" ~ NA)) 

### Question 3
 
 df$`drinking_water_source` %in% Unimproved_water_source

 
 
 df_analyse <-df %>%
   select(`data_collection_round`, `single_headed_household`, `improved_water_source`) %>% 
    group_by(`data_collection_round`, `single_headed_household`) %>% 
    count(improved_water_source)
    
 
 df_analyse %>% 
   filter(single_headed_household == 'Yes') %>% 
   ggplot(aes(x=data_collection_round, y=n, fill=improved_water_source )) +
   geom_bar(stat="identity") +
   # geom_hline(yintercept=100, color="orange", size=.5) +
   theme_ipsum() +
   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) 
 
    
 df_analyse_diarrhea <-df %>%
   select(`data_collection_round`, `diarrhea_under_5`, `improved_water_source`) %>% 
   group_by(`data_collection_round`, `diarrhea_under_5`) %>% 
   count(improved_water_source)
 

 
 df_analyse_diarrhea %>% 
   filter(diarrhea_under_5 == 'Yes') %>% 
   ggplot(aes(x=data_collection_round, y=n, fill=improved_water_source )) +
   geom_bar(stat="identity") +
   # geom_hline(yintercept=100, color="orange", size=.5) +
   theme_ipsum() +
   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) 
