# Create line plot figures for COVID-19 presentation
# Author: Rebecca Barter
# Date: 3/31/2020

library(tidyverse)
library(lubridate)
library(scales)

## Load data ##----------------------------------------------------------------

data_clean <- read_csv("../../data/deaths_clean_4_10.csv")
.days <- 7
date_7day_start_prediction <- max(data_clean$date) - days(.days)

color_expanded <- "#593C8F" # expanded shared model
color_county <- "#547AA5" # exponential model
color_shared <- "#AA4465" # shared exponential model
color_linear <- "#FF8552" # linear model
color_demographics <- "#931621" # demographics model
color_ensemble <- "#445E93" # shared plus linear model
#   


## Many line plots death projections compared with real ## ---------------------------------------------------------------

createLinePlots <- function(counties) {
  # create a version of the data for plotting
  data_plot_deaths_subset <- data_clean %>% 
    filter(county_state %in% counties) %>%
    # make the deaths data missing after 3 days before
    mutate(actual_deaths_with_missing = if_else(date > date_7day_start_prediction,
                                                as.numeric(NA), actual_deaths)) %>%
    #filter(county_state %in% c("Queens County, NY", "King County, WA")) %>%
    # add the real data to the predicted data for March 27
    mutate(highlight = date >= date_7day_start_prediction) %>%
    # add the 3-day predictions
    group_by(county_state) %>%
    mutate(exponential_model_7days = case_when(
      date == date_7day_start_prediction - days(1) ~ actual_deaths_with_missing, 
      date != date_7day_start_prediction - days(1) ~ exponential_model_7days
    )) %>%
    mutate(demographics_model_7days = case_when(
      date == date_7day_start_prediction - days(1) ~ actual_deaths_with_missing, 
      date != date_7day_start_prediction - days(1) ~ demographics_model_7days
    )) %>%
    mutate(linear_model_7days = case_when(
      date == date_7day_start_prediction - days(1) ~ actual_deaths_with_missing, 
      date != date_7day_start_prediction - days(1) ~ linear_model_7days
    )) %>%
    mutate(shared_exponential_model_7days = case_when(
      date == date_7day_start_prediction - days(1) ~ actual_deaths_with_missing, 
      date != date_7day_start_prediction - days(1) ~ shared_exponential_model_7days
    )) %>%
    mutate(expanded_shared_model_7days = case_when(
      date == date_7day_start_prediction - days(1) ~ actual_deaths_with_missing, 
      date != date_7day_start_prediction - days(1) ~ expanded_shared_model_7days
    )) %>%
    mutate(expanded_shared_and_linear_model_7days = case_when(
      date == date_7day_start_prediction - days(1) ~ actual_deaths_with_missing, 
      date != date_7day_start_prediction - days(1) ~ expanded_shared_and_linear_model_7days
    )) %>%
    ungroup()
  
  
  past_data <- data_plot_deaths_subset %>%
    select(county_state, actual_deaths, date, days_since_10_deaths) %>%
    filter(date <= date_7day_start_prediction - days(1))
  
  future_data <- data_plot_deaths_subset %>%
    select(county_state, actual_deaths, date, 
           days_since_10_deaths,
           exponential_model_7days, 
           shared_exponential_model_7days, 
           demographics_model_7days, 
           linear_model_7days,
           expanded_shared_model_7days,
           expanded_shared_and_linear_model_7days) %>%
    filter(date >= date_7day_start_prediction - days(1))
  
  # get text label locations
  annotation <- data_plot_deaths_subset %>%
    group_by(county_state) %>%
    mutate(x_text = max(days_since_10_deaths),
           y_text = max(actual_deaths, na.rm = T)) %>%
    ungroup() %>%
    distinct(county_state, x_text, y_text)
  # annotation_predicted_county <- data_plot_deaths_subset %>%
  #   group_by(county_state) %>%
  #   mutate(x_text = max(days_since_10_deaths),
  #          y_text = round(max(exponential_model_7days, na.rm = T))) %>%
  #   ungroup() %>%
  #   distinct(county_state, x_text, y_text)
  # annotation_predicted_demographics <- data_plot_deaths_subset %>%
  #   group_by(county_state) %>%
  #   mutate(x_text = max(days_since_10_deaths),
  #          y_text = round(max(demographics_model_7days, na.rm = T))) %>%
  #   ungroup() %>%
  #   distinct(county_state, x_text, y_text)
  # annotation_predicted_linear <- data_plot_deaths_subset %>%
  #   group_by(county_state) %>%
  #   mutate(x_text = max(days_since_10_deaths),
  #          y_text = round(max(linear_model_7days, na.rm = T))) %>%
  #   ungroup() %>%
  #   distinct(county_state, x_text, y_text)
  # annotation_predicted_shared <- data_plot_deaths_subset %>%
  #   group_by(county_state) %>%
  #   mutate(x_text = max(days_since_10_deaths),
  #          y_text = round(max(shared_exponential_model_7days, na.rm = T))) %>%
  #   ungroup() %>%
  #   distinct(county_state, x_text, y_text)
  # annotation_predicted_expanded <- data_plot_deaths_subset %>%
  #   group_by(county_state) %>%
  #   mutate(x_text = max(days_since_10_deaths),
  #          y_text = round(max(expanded_shared_model_7days, na.rm = T))) %>%
  #   ungroup() %>%
  #   distinct(county_state, x_text, y_text)
  annotation_predicted_ensemble <- data_plot_deaths_subset %>%
    group_by(county_state) %>%
    mutate(x_text = max(days_since_10_deaths),
           y_text = round(max(expanded_shared_and_linear_model_7days, na.rm = T))) %>%
    ungroup() %>%
    distinct(county_state, x_text, y_text)
  
  date_start_pred <- data_plot_deaths_subset %>%
    filter(date == date_7day_start_prediction)
  
  date_end_pred <- data_plot_deaths_subset %>%
    filter(date == date_7day_start_prediction + days(7))
  
  # make line plots
  past_data %>%
    ggplot() +
    # past data
    geom_line(aes(x = days_since_10_deaths, y = actual_deaths), col = "grey50") +
    # future data
    geom_line(aes(x = days_since_10_deaths, y = actual_deaths), 
              data = future_data, col = "black", alpha = 0.7) +
    # # future predicted data
    # geom_line(aes(x = days_since_10_deaths, y = exponential_model_7days), 
    #           data = future_data,
    #           linetype = "dashed", col = color_county) +
    # geom_line(aes(x = days_since_10_deaths, y = demographics_model_7days), 
    #           data = future_data,
    #           linetype = "dashed", col = color_demographics) +
    # geom_line(aes(x = days_since_10_deaths, y = linear_model_7days), 
    #           data = future_data,
    #           linetype = "dashed", col = color_linear) +
    # geom_line(aes(x = days_since_10_deaths, y = shared_exponential_model_7days), 
    #           data = future_data,
    #           linetype = "dashed", col = color_shared) +
    # geom_line(aes(x = days_since_10_deaths, y = expanded_shared_model_7days), 
    #           data = future_data,
    #           linetype = "dashed", col = color_expanded) +
    geom_line(aes(x = days_since_10_deaths, y = expanded_shared_and_linear_model_7days), 
              data = future_data,
              linetype = "dashed", col = color_ensemble) +
    # observed final value
    geom_text(aes(x = x_text + 1, y = y_text, label = paste(y_text, " (observed)")),
              data = annotation, hjust = 0, check_overlap = TRUE,
              family = "Avenir", size = 3, alpha = 0.7) +
    # predicted final value
    geom_text(aes(x = x_text + 1, y = y_text, label = paste0(y_text, " (predicted)")),
              data = filter(annotation_predicted_ensemble, 
                            #county_state != "Orleans County, LA",
                            county_state != "Monmouth County, NJ",
                            county_state != "Dougherty County, GA"), 
              hjust = 0, check_overlap = TRUE,
              family = "Avenir", size = 3, col = color_ensemble) +
    # geom_text(aes(x = x_text + 1, y = y_text, label = paste0(y_text, " (separate prediction)")),
    #           data = filter(annotation_predicted_county, 
    #                         county_state != "Orleans County, LA",
    #                         county_state != "Monmouth County, NJ",
    #                         county_state != "Suffolk County, NY"),
    #           hjust = 0, check_overlap = TRUE,
    #           family = "Avenir", size = 3, col = color_separate) +
    # geom_text(aes(x = x_text + 1, y = y_text, label = paste0(y_text, " (demographics shared prediction)")),
    #           data = filter(annotation_predicted_demographics, 
    #                         county_state != "Orleans County, LA",
    #                         county_state != "New York County, NY",
    #                         county_state != "Wayne County, MI",
    #                         county_state != "Monmouth County, NJ",
    #                         county_state != "Suffolk County, NY"), 
    #           hjust = 0, check_overlap = TRUE,
    #           family = "Avenir", size = 3, col = color_demographics) +
    # geom_text(aes(x = x_text + 1, y = y_text, label = paste0(y_text, " (expanded shared prediction)")),
    #           data = filter(annotation_predicted_expanded, 
    #                         county_state != "Orleans County, LA",
    #                         county_state != "Monmouth County, NJ",
    #                         county_state != "Suffolk County, NY"),
    #           hjust = 0, check_overlap = TRUE,
    #           family = "Avenir", size = 3, col = color_expanded) +
    # geom_text(aes(x = x_text + 1, y = y_text, label = paste0(y_text, " (shared prediction)")),
    #           data = filter(annotation_predicted_shared, 
    #                         county_state != "Orleans County, LA",
    #                         county_state != "Monmouth County, NJ",
    #                         county_state != "Suffolk County, NY"),
    #           hjust = 0, check_overlap = TRUE,
    #           family = "Avenir", size = 3, col = color_shared) +
    # geom_text(aes(x = x_text + 1, y = y_text, label = paste0(y_text, " (linear prediction)")),
    #           data = filter(annotation_predicted_linear, 
    #                         county_state != "Orleans County, LA",
    #                         county_state != "Monmouth County, NJ",
    #                         county_state != "Suffolk County, NY"),
    #           hjust = 0, check_overlap = TRUE,
    #           family = "Avenir", size = 3, col = color_linear) +
    # first prediction point
    geom_point(aes(x = days_since_10_deaths, y = actual_deaths), 
               data = date_start_pred, alpha = 0.5) +
    geom_segment(aes(x = days_since_10_deaths, xend = days_since_10_deaths,
                     y = actual_deaths, yend = 0), 
                 data = date_start_pred, col = "grey50", alpha = 0.3) +
    geom_text(aes(x = days_since_10_deaths, y = 0, label = "April 2"), 
              data = date_start_pred, nudge_y = -10, nudge_x = -1.5,  
              family = "Avenir", size = 3, alpha = 0.7) +
    # last prediction point
    geom_point(aes(x = days_since_10_deaths, y = actual_deaths), 
               data = date_end_pred, alpha = 0.5) +
    # geom_point(aes(x = days_since_10_deaths, y = exponential_model_7days), 
    #            data = date_end_pred, alpha = 0.5, col = color_county) +
    # geom_point(aes(x = days_since_10_deaths, y = demographics_model_7days), 
    #            data = date_end_pred, alpha = 0.5, col = color_demographics) +
    # geom_point(aes(x = days_since_10_deaths, y = linear_model_7days), 
    #            data = date_end_pred, alpha = 0.5, col = color_linear) +
    # geom_point(aes(x = days_since_10_deaths, y = shared_exponential_model_7days), 
    #            data = date_end_pred, alpha = 0.5, col = color_shared) +
    # geom_point(aes(x = days_since_10_deaths, y = expanded_shared_model_7days), 
    #            data = date_end_pred, alpha = 0.5, col = color_expanded) +
    geom_point(aes(x = days_since_10_deaths, y = expanded_shared_and_linear_model_7days), 
               data = date_end_pred, alpha = 0.5, col = color_ensemble) +
    # date lines
    geom_segment(aes(x = days_since_10_deaths, xend = days_since_10_deaths,
                     y = actual_deaths, yend = 0), 
                 data = date_end_pred, col = "grey50", alpha = 0.3) +
    geom_text(aes(x = days_since_10_deaths, y = 0, label = "Apr 8"), 
              data = date_end_pred, nudge_y = -10, nudge_x = 1.5,
              family = "Avenir", size = 3, alpha = 0.7) +
    scale_x_continuous(limits = c(-2, 43)) +
    #  scale_y_continuous(limits = c(-20, 310)) +
    scale_color_manual(values = c("black", "grey50"), guide = "none") +
    theme_minimal(base_size = 16)  +
    theme(axis.line = element_line(color = "grey40"),
          axis.ticks = element_line(color = "grey40"),
          axis.title = element_text(family = "Avenir")) +
    labs(x = "Days since 10 deaths", y = "Cumulative confirmed deaths") +
    facet_wrap(~county_state, ncol = 2 )
}

createLinePlots(c("Kings County, NY", 
                  "Queens County, NY",
                  "King County, WA",
                  "New York County, NY",
                  "Orleans County, LA",
                  "Wayne County, MI"))

ggsave("../figures/grid_worst_counties_4_10.png", width = 9, height = 10)    


createLinePlots(c("Bergen County, NJ", 
                  "Broward County, FL",
                  "Dougherty County, GA",
                  "Oakland County, MI",
                  "Monmouth County, NJ",
                  "Suffolk County, NY"))

ggsave("../figures/grid_random_counties_4_10.png", width = 8, height = 10)    
 



