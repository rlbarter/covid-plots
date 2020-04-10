# Create line plot figures for COVID-19 presentation
# Author: Rebecca Barter
# Date: 3/31/2020

library(tidyverse)
library(lubridate)
library(scales)

## Load data ##----------------------------------------------------------------

data_clean <- read_csv("../../data/deaths_clean.csv")






## Many line plots death projections compared with real ## ---------------------------------------------------------------


# create a version of the data for plotting
data_plot_deaths_subset <- data_clean %>% 
  filter(county_state %in% c("Kings County, NY", 
                             "Queens County, NY",
                             "King County, WA",
                             "New York County, NY",
                             "Orleans County, LA",
                             "Wayne County, MI",
                             "Bronx County, NY",
                             "Richmond County, NY")) %>%
  # make the deaths data missing after 3 days before
  mutate(actual_deaths_with_missing = if_else(date > ymd("2020-03-28"),
                                              as.numeric(NA), actual_deaths)) %>%
  #filter(county_state %in% c("Queens County, NY", "King County, WA")) %>%
  # add the real data to the predicted data for March 27
  mutate(highlight = date >= ymd("2020-03-28")) %>%
  # add the 3-day predictions
  group_by(county_state) %>%
  mutate(deaths_predicted_3day = case_when(
    date == ymd("2020-03-27") ~ actual_deaths_with_missing, 
    date != ymd("2020-03-27") ~ deaths_predicted_3day
  )) %>%
  ungroup()


past_data <- data_plot_deaths_subset %>%
  select(county_state, actual_deaths, date, days_since_10_deaths) %>%
  filter(date <= ymd("2020-03-27"))

future_data <- data_plot_deaths_subset %>%
  select(county_state, actual_deaths, date, 
         days_since_10_deaths, deaths_predicted_3day) %>%
  filter(date >= ymd("2020-03-27"))

# get text label locations
annotation <- data_plot_deaths_subset %>%
  group_by(county_state) %>%
  mutate(x_text = max(days_since_10_deaths),
         y_text = max(actual_deaths, na.rm = T)) %>%
  ungroup() %>%
  distinct(county_state, x_text, y_text)
annotation_predicted <- data_plot_deaths_subset %>%
  group_by(county_state) %>%
  mutate(x_text = max(days_since_10_deaths),
         y_text = round(max(deaths_predicted_3day, na.rm = T))) %>%
  ungroup() %>%
  distinct(county_state, x_text, y_text)

march27 <- data_plot_deaths_subset %>%
  filter(date == ymd("2020-03-27"))

march30 <- data_plot_deaths_subset %>%
  filter(date == ymd("2020-03-30"))

# make line plots
past_data %>%
  ggplot() +
  # past data
  geom_line(aes(x = days_since_10_deaths, y = actual_deaths), col = "grey50") +
  # future data
  geom_line(aes(x = days_since_10_deaths, y = actual_deaths), 
            data = future_data, col = "black") +
  # future predicted data
  geom_line(aes(x = days_since_10_deaths, y = deaths_predicted_3day), 
            data = future_data,
            linetype = "dashed", col = "#445E93") +
  # observed final value
  geom_text(aes(x = x_text + 1, y = y_text, label = paste(y_text, " (observed)")),
            data = annotation, hjust = 0, check_overlap = TRUE,
            family = "Avenir", size = 3) +
  # predicted final value
  geom_text(aes(x = x_text + 1, y = y_text, label = paste0(y_text, " (predicted)")),
            data = annotation_predicted, hjust = 0, check_overlap = TRUE,
            family = "Avenir", size = 3, col = "#445E93") +
  # march 27
  geom_point(aes(x = days_since_10_deaths, y = actual_deaths), 
             data = march27, alpha = 0.5) +
  geom_segment(aes(x = days_since_10_deaths, xend = days_since_10_deaths,
                   y = actual_deaths, yend = 0), 
             data = march27, col = "#445E93", alpha = 0.3) +
  geom_text(aes(x = days_since_10_deaths, y = 0, label = "Mar 27"), 
             data = march27, nudge_y = -10, nudge_x = -1.5,  
            family = "Avenir", size = 3, alpha = 0.7) +
  # march 30
  geom_point(aes(x = days_since_10_deaths, y = actual_deaths), 
             data = march30, alpha = 0.5) +
  geom_segment(aes(x = days_since_10_deaths, xend = days_since_10_deaths,
                   y = actual_deaths, yend = 0), 
               data = march30, col = "#445E93", alpha = 0.3) +
  geom_text(aes(x = days_since_10_deaths, y = 0, label = "Mar 30"), 
            data = march30, nudge_y = -10, nudge_x = 1.5,
            family = "Avenir", size = 3, alpha = 0.7) +
  scale_x_continuous(limits = c(-2, 35)) +
  scale_y_continuous(limits = c(-20, 310)) +
  scale_color_manual(values = c("black", "grey50"), guide = "none") +
  theme_minimal(base_size = 16)  +
  theme(axis.line = element_line(color = "grey40"),
        axis.ticks = element_line(color = "grey40"),
        axis.title = element_text(family = "Avenir")) +
  labs(x = "Days since 10 deaths", y = "Cumulative confirmed deaths") +
  facet_wrap(~county_state, ncol = 2)
ggsave("../figures/grid_worst_counties.png", width = 8, height = 10)    









## Random counties ## ---------------------------------------------------------------

#set.seed(234789)
set.seed(24799)
random_counties <- data_clean %>% 
  filter(!(county_state %in% c("Kings County, NY", 
                                    "Queens County, NY",
                                    "King County, WA",
                                    "New York County, NY",
                                    "Orleans County, LA",
                                    "Wayne County, MI",
                                    "Bronx County, NY",
                                    "Richmond County, NY"))) %>%
  group_by(county_state) %>%
  summarise(actual_deaths = max(actual_deaths)) %>%
  ungroup() %>%
  filter(actual_deaths > 10) %>%
  distinct(county_state) %>% 
  sample_n(8) %>% 
  pull(county_state)
# create a version of the data for plotting
data_plot_deaths_subset <- data_clean %>% 
  filter(county_state %in% random_counties) %>%
  # make the deaths data missing after 3 days before
  mutate(actual_deaths_with_missing = if_else(date > ymd("2020-03-28"),
                                              as.numeric(NA), actual_deaths)) %>%
  #filter(county_state %in% c("Queens County, NY", "King County, WA")) %>%
  # add the real data to the predicted data for March 27
  mutate(highlight = date >= ymd("2020-03-28")) %>%
  # add the 3-day predictions
  group_by(county_state) %>%
  mutate(deaths_predicted_3day = case_when(
    date == ymd("2020-03-27") ~ actual_deaths_with_missing, 
    date != ymd("2020-03-27") ~ deaths_predicted_3day
  )) %>%
  ungroup()


past_data <- data_plot_deaths_subset %>%
  select(county_state, actual_deaths, date, days_since_10_deaths) %>%
  filter(date <= ymd("2020-03-27"))

future_data <- data_plot_deaths_subset %>%
  select(county_state, actual_deaths, date, 
         days_since_10_deaths, deaths_predicted_3day) %>%
  filter(date >= ymd("2020-03-27"))

# get text label locations
annotation <- data_plot_deaths_subset %>%
  group_by(county_state) %>%
  mutate(x_text = max(days_since_10_deaths),
         y_text = max(actual_deaths, na.rm = T)) %>%
  ungroup() %>%
  distinct(county_state, x_text, y_text)
annotation_predicted <- data_plot_deaths_subset %>%
  group_by(county_state) %>%
  mutate(x_text = max(days_since_10_deaths),
         y_text = round(max(deaths_predicted_3day, na.rm = T))) %>%
  ungroup() %>%
  distinct(county_state, x_text, y_text)

march27 <- data_plot_deaths_subset %>%
  filter(date == ymd("2020-03-27"))

march30 <- data_plot_deaths_subset %>%
  filter(date == ymd("2020-03-30"))

# make line plots
past_data %>%
  ggplot() +
  # past data
  geom_line(aes(x = days_since_10_deaths, y = actual_deaths), col = "grey50") +
  # future data
  geom_line(aes(x = days_since_10_deaths, y = actual_deaths), 
            data = future_data, col = "black") +
  # future predicted data
  geom_line(aes(x = days_since_10_deaths, y = deaths_predicted_3day), 
            data = future_data,
            linetype = "dashed", col = "#445E93") +
  # observed final value
  geom_text(aes(x = x_text + 1, y = y_text, label = paste(y_text, " (observed)")),
            data = annotation, hjust = 0, check_overlap = TRUE,
            family = "Avenir", size = 3) +
  # predicted final value
  geom_text(aes(x = x_text + 1, y = y_text, label = paste0(y_text, " (predicted)")),
            data = annotation_predicted, hjust = 0, check_overlap = TRUE,
            family = "Avenir", size = 3, col = "#445E93") +
  # march 27
  geom_point(aes(x = days_since_10_deaths, y = actual_deaths), 
             data = march27, alpha = 0.5) +
  geom_segment(aes(x = days_since_10_deaths, xend = days_since_10_deaths,
                   y = actual_deaths, yend = 0), 
               data = march27, col = "#445E93", alpha = 0.3) +
  geom_text(aes(x = days_since_10_deaths, y = 0, label = "Mar 27"), 
            data = march27, nudge_y = -5, nudge_x = -0.5,  
            family = "Avenir", size = 3, alpha = 0.7) +
  # march 30
  geom_point(aes(x = days_since_10_deaths, y = actual_deaths), 
             data = march30, alpha = 0.5) +
  geom_segment(aes(x = days_since_10_deaths, xend = days_since_10_deaths,
                   y = actual_deaths, yend = 0), 
               data = march30, col = "#445E93", alpha = 0.3) +
  geom_text(aes(x = days_since_10_deaths, y = 0, label = "Mar 30"), 
            data = march30, nudge_y = -5, nudge_x = 0.5,
            family = "Avenir", size = 3, alpha = 0.7) +
  scale_x_continuous(limits = c(-4, 12)) +
  #scale_y_continuous(limits = c(-20, 310)) +
  scale_color_manual(values = c("black", "grey50"), guide = "none") +
  theme_minimal(base_size = 16)  +
  theme(axis.line = element_line(color = "grey40"),
        axis.ticks = element_line(color = "grey40"),
        axis.title = element_text(family = "Avenir")) +
  labs(x = "Days since 10 deaths", y = "Cumulative confirmed deaths") +
  facet_wrap(~county_state, ncol = 2)
ggsave("../figures/grid_random_counties.png", width = 8, height = 10)    




