# Create scatterplot figures for COVID-19 presentation
# Author: Rebecca Barter
# Date: 3/31/2020

library(tidyverse)
library(lubridate)
library(scales)

## Load data ##----------------------------------------------------------------

data_clean <- read_csv("../../data/deaths_clean.csv")

## Plot 3-day prediction performance ## ---------------------------------------------------------------

data_clean %>%
  filter(date == ymd("2020-03-30")) %>%
  ggplot() +
  geom_point(aes(x = deaths_predicted_3day, y = actual_deaths), 
             col = "#F93943", alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(aes(x = deaths_predicted_3day, 
                y = actual_deaths, label = county_state), 
            hjust = 0, check_overlap = TRUE, size = 5, 
            family = "Avenir", alpha = 0.7, nudge_x = 5, nudge_y = 5,
            data = filter(data_clean, 
                          deaths_predicted_3day > 60,
                          date == ymd("2020-03-30"))) +
  theme_classic(base_size = 22) +
  labs(x = "Predicted deaths by 3/30\n(predicted on 3/27)",
       y = "Actual deaths by 3/30") +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(0, 400)) +
  theme(axis.line = element_line(color = "grey40"),
        axis.ticks = element_line(color = "grey40"),
        axis.title = element_text(family = "Avenir")) +
  coord_fixed() 
ggsave("../figures/scatter_performance.png", width = 8, height = 8)    

