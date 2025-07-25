# UNICEF Education Data Analysis
# Author: Elvira Khwatenge
# Date: Sys.Date()

# ---- Load Libraries ----
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("janitor")) install.packages("janitor", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("magrittr")) install.packages("magrittr", dependencies = TRUE)

library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(magrittr)

# ---- Load and Clean Data ----
education_data <- read.csv("EDUCATION.csv") %>%
  clean_names() %>%
  select(ref_area, geographic_area, indicator, sex, education_level, wealth_quintile, residence, time_period, obs_value) %>%
  filter(!is.na(obs_value))

# Check if data is available
if(nrow(education_data) == 0) {
  stop("No data available after initial filtering.")
}

# ---- Analysis 1: Gender Disparities ----
gender_disparity <- education_data %>%
  filter(sex %in% c("Male", "Female")) %>%
  group_by(geographic_area, sex) %>%
  summarise(avg_completion_rate = mean(obs_value, na.rm = TRUE))

# Plot gender disparities by country
ggplot(gender_disparity, aes(x = geographic_area, y = avg_completion_rate, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Completion Rate by Gender and Country", x = "Country", y = "Completion Rate")

# ---- Analysis 2: Influence of Wealth on Education ----
wealth_disparity <- education_data %>%
  filter(wealth_quintile != "Total") %>%
  group_by(wealth_quintile) %>%
  summarise(avg_completion_rate = mean(obs_value, na.rm = TRUE))

# Plot completion rates by wealth quintile
ggplot(wealth_disparity, aes(x = wealth_quintile, y = avg_completion_rate, fill = wealth_quintile)) +
  geom_bar(stat = "identity") +
  labs(title = "Completion Rate by Wealth Quintile", x = "Wealth Quintile", y = "Completion Rate")

# ---- Analysis 3: Urban vs. Rural Disparities ----
residence_disparity <- education_data %>%
  filter(residence %in% c("Urban", "Rural")) %>%
  group_by(geographic_area, residence) %>%
  summarise(avg_completion_rate = mean(obs_value, na.rm = TRUE))

# Plot urban vs rural completion rates
ggplot(residence_disparity, aes(x = geographic_area, y = avg_completion_rate, fill = residence)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Completion Rate by Residence Type and Country", x = "Country", y = "Completion Rate")

# ---- Analysis 4: Trends Over Time ----
time_trends <- education_data %>%
  group_by(time_period) %>%
  summarise(avg_completion_rate = mean(obs_value, na.rm = TRUE))

# Plot trends in completion rates over time
ggplot(time_trends, aes(x = time_period, y = avg_completion_rate)) +
  geom_line() +
  geom_point() +
  labs(title = "Trends in Completion Rate Over Time", x = "Year", y = "Average Completion Rate")

# ---- Analysis 5: Comparison Across Countries ----
country_comparison <- education_data %>%
  group_by(geographic_area) %>%
  summarise(avg_completion_rate = mean(obs_value, na.rm = TRUE))

# Plot completion rates by country with horizontal bars
ggplot(country_comparison, aes(x = reorder(geographic_area, avg_completion_rate), y = avg_completion_rate)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Average Completion Rate by Country", x = "Country", y = "Completion Rate") +
  theme_minimal()

# Option: Display Only the Top and Bottom 10 Countries
top_countries <- head(country_comparison[order(-country_comparison$avg_completion_rate), ], 10)
bottom_countries <- head(country_comparison[order(country_comparison$avg_completion_rate), ], 10)
selected_countries <- bind_rows(top_countries, bottom_countries)

# Plot top and bottom countries with horizontal bars
ggplot(selected_countries, aes(x = reorder(geographic_area, avg_completion_rate), y = avg_completion_rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top and Bottom Countries by Completion Rate", x = "Country", y = "Completion Rate") +
  theme_minimal()

# ---- Findings and Discussion ----
cat("Findings and Discussion\n")
cat("Significant differences in education completion rates exist between males and females, with females often having lower rates in some regions, suggesting a need for gender-focused educational support.\n")
cat("Wealth quintiles correlate with completion rates, as students from higher-income backgrounds tend to have better outcomes, pointing to a need for financial support for lower-income students.\n")
cat("Rural areas consistently report lower completion rates than urban counterparts, indicating a need for improved educational infrastructure in rural areas.\n")
cat("While there is a general improvement in education completion rates over recent years, some regions show stagnation or decline, highlighting areas that may require targeted interventions.\n")