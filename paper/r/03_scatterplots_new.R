# Create scatterplot figures for COVID-19 presentation
# Author: Rebecca Barter
# Date: 3/31/2020

library(tidyverse)
library(lubridate)
library(scales)

## Load data ##----------------------------------------------------------------

data_clean <- read_csv("../../data/deaths_clean_new_full.csv")

## Plot 3-day prediction performance ## ---------------------------------------------------------------

data_clean %>%
  filter(date == ymd("2020-04-1")) %>%
  ggplot() +
  geom_point(aes(x = deaths_predicted_ensemble, y = actual_deaths), 
             col = "#F93943", alpha = 0.8, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(aes(x = deaths_predicted_ensemble, 
                y = actual_deaths, label = county_state), 
            hjust = 0, check_overlap = TRUE, size = 5, 
            family = "Avenir", alpha = 0.7, nudge_x = 5, nudge_y = 5,
            data = filter(data_clean, 
                          deaths_predicted_ensemble > 60,
                          date == ymd("2020-4-1"))) +
  theme_classic(base_size = 22) +
  labs(x = "Predicted deaths by 4/1\n(predicted on 3/29)",
       y = "Actual deaths by 4/1") +
  scale_x_continuous(limits = c(0, 680)) +
  scale_y_continuous(limits = c(0, 680)) +
  theme(axis.line = element_line(color = "grey40"),
        axis.ticks = element_line(color = "grey40"),
        axis.title = element_text(family = "Avenir")) +
  coord_fixed()
ggsave("../figures/scatter_performance_new.png", width = 8, height = 8)    



data_clean %>%
  filter(date == ymd("2020-04-1"), actual_deaths > 3) %>% 
  summarise(county_mse = mean(abs(actual_deaths - deaths_predicted_county)),
            shared_mse = mean(abs(actual_deaths - deaths_predicted_shared)),
            ensemble_mse = mean(abs(actual_deaths - deaths_predicted_ensemble)))
