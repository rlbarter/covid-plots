# Clean data for COVID paper
# Author: Rebecca Barter
# Date: 3/31/2020
# Note: CURRENTLY COMMENTED OUT ALL BUT 7-DAY ENSMEBLE (EXPANDED + LINEAR) RESULTS

library(tidyverse)
library(lubridate)
library(scales)

## Load data ##----------------------------------------------------------------

data <- read_csv("../../data/multi_day_predictions.csv")
#data <- read_csv("../../data/line_plots.csv")


## clean data ##----------------------------------------------------------------

# functin to extract whatever kind of count
extractCounts <- function(county, 
                          state, 
                          variable,
                          start_date = ymd("2020/1/22")) {
  # variable is one of deaths, predicted_deaths_ensemble_1 or 
  #     predicted_deaths_ensemble_3
  
  # calculate the number of entries for the variable in question
  length_deaths_vec <- data %>%
    transmute(var = str_replace(.data[[variable]], "\\[", ""),
              var = str_replace(var, "\\]", ""),
              var = str_squish(var)) %>%
    mutate(length = length(str_split(var, " ")[[1]])) %>%
    distinct(length) %>% 
    pull(length)
  
  
  deaths_data <- data %>% filter(CountyName == county, 
                                 StateNameAbbreviation == state) %>% 
    transmute(var = str_replace(.data[[variable]], "\\[", ""),
              var = str_replace(var, "\\]", ""),
              var = str_squish(var)) %>%
    separate(var, into = paste0("day", 1:length_deaths_vec), sep = " ") %>%
    mutate_all(parse_number) %>%
    pivot_longer(cols = everything(), names_to = "day") %>%
    mutate(date = start_date + 0:(length_deaths_vec - 1))  %>%
    select(-day) %>%
    mutate(county = county, state = state) 
  return(deaths_data)
}

# function to extract the counts and join them to the predictions
extractDeaths = function(county, state,
                         predicted_vars, # the names of the columns of the predicted values
                         deaths_start_date = ymd("2020/1/22")) {
  
  # extract the deaths data
  deaths <- extractCounts(county, state, 
                          "deaths", 
                          start_date = deaths_start_date) %>%
    rename(actual_deaths = value)
  
  # get the first date of the predictions
  last_date <- ymd("2020/1/22") + days(nrow(deaths))
  # predicted_deaths_start_date_3days <- last_date - days(3)
  # predicted_deaths_start_date_5days <- last_date - days(5)
  predicted_deaths_start_date_7days <- last_date - days(7)
  #predicted_deaths_start_date_10days <- last_date - days(10)
  
  # predicted_3days <- purrr::map(predicted_vars,
  #     function(.var) {
  #       rename <- c("value")
  #       names(rename) <- paste0(str_replace_all(.var, " ", "_"), "_3days")
  #       extractCounts(county, state, 
  #                                  paste0(.var, " predicting 3 days ahead"),
  #                                  start_date = predicted_deaths_start_date_3days) %>%
  #       rename(!!rename)
  #       }) %>%
  #   reduce(left_join, by = c("date", "county", "state"))
  # predicted_5days <- purrr::map(predicted_vars,
  #                               function(.var) {
  #                                 rename <- c("value")
  #                                 names(rename) <- paste0(str_replace_all(.var, " ", "_"), "_5days")
  #                                 extractCounts(county, state, 
  #                                               paste0(.var, " predicting 5 days ahead"),
  #                                               start_date = predicted_deaths_start_date_5days) %>%
  #                                   rename(!!rename)
  #                               }) %>%
  #   reduce(left_join, by = c("date", "county", "state"))
  predicted_7days <- purrr::map(predicted_vars,
                                function(.var) {
                                  rename <- c("value")
                                  names(rename) <- paste0(str_replace_all(.var, " ", "_"), "_7days")
                                  extractCounts(county, state, 
                                                paste0(.var, " predicting 7 days ahead"),
                                                start_date = predicted_deaths_start_date_7days) %>%
                                    rename(!!rename)
                                }) %>%
    reduce(left_join, by = c("date", "county", "state"))
  # predicted_10days <- purrr::map(predicted_vars,
  #                               function(.var) {
  #                                 rename <- c("value")
  #                                 names(rename) <- paste0(str_replace_all(.var, " ", "_"), "_10days")
  #                                 extractCounts(county, state, 
  #                                               paste0(.var, " predicting 10 days ahead"),
  #                                               start_date = predicted_deaths_start_date_10days) %>%
  #                                   rename(!!rename)
  #                               }) %>%
  #   reduce(left_join, by = c("date", "county", "state"))
  # 
 
  # combine the data
  full_data <- reduce(list(deaths, 
                           #predicted_3days, predicted_5days, 
                           predicted_7days
                           #predicted_10days
                           ), 
                      full_join, by = c("date", "county", "state"))
  return(full_data)
  
}



state_counties_df <- data %>% 
  unite(county_state, 
        CountyName, StateNameAbbreviation, 
        sep = " County, ", remove = F) %>%
  # filter(county_state %in% c("Bronx County, NY",
  #                            "King County, WA",
  #                            "Kings County, NY",
  #                            "New York County, NY",
  #                            "Orleans County, LA",
  #                            "Queens County, NY",
  #                            "Richmond County, NY",
  #                            "Wayne County, MI",
  #                            "Bergen County, NJ",
  #                            "Broward County, FL",
  #                            "Dougherty County, GA",
  #                            "Oakland County, MI",
  #                            "Monmouth County, NJ",
  #                            "Suffolk County, NY")) %>%
  distinct(CountyName, StateNameAbbreviation)

predicted_vars <- c(
  # "exponential model", 
  # "demographics model", 
  # "linear model",
  # "shared exponential model",
  # "expanded shared model",
  "expanded shared and linear model"
  # "ensemble of all models"
  )

deaths_clean <- map2_df(state_counties_df$CountyName, state_counties_df$StateNameAbbreviation,
                        function(.county, .state) {
                          cat(.county, ", ", .state, "\n", sep = "")
                          extractDeaths(.county, .state, 
                                        predicted_vars = predicted_vars) 
                        }) %>%
  unite(county_state, county, state, sep = " County, ")


# days since 10 deaths
date_of_10_deaths <- deaths_clean %>%
  group_by(county_state) %>%
  filter(actual_deaths >= 10) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  select(county_state, date_of_10_deaths = date)


# add days since 10 deaths
deaths_clean <- deaths_clean %>%
  left_join(date_of_10_deaths, by = c("county_state")) %>%
  mutate(days_since_10_deaths = as.numeric(date - date_of_10_deaths)) 

write_csv(deaths_clean, "../../data/deaths_clean_ensemble_only_4_10.csv")
