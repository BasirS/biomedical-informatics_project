# ============================================================================
# Step 1: Setting Up R Environment
# ============================================================================

# checking and installing required packages
required_packages = c("tidyverse", "lubridate", "scales", "forecast", 
                      "tseries", "zoo", "car", "broom", "corrplot")

# installing missing packages
new_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# loading required packages for our analysis
library(tidyverse)    # for data manipulation and visualization
library(lubridate)    # for working with dates
library(scales)       # for formatting our plots
library(forecast)     # for forecasting trends
library(tseries)      # for time series analysis
library(zoo)          # for ordered data analysis
library(car)          # for statistical tests
library(broom)        # for model outputs
library(corrplot)     # for correlation plots

# sourcing helper functions
source("../functions/visualization_helpers.R")
source("../functions/time_series_functions.R")
source("../functions/anomaly_detection_functions.R")

# checking our r environment
sessionInfo()

# ============================================================================
# Step 2: Loading and Inspecting Data
# ============================================================================

# loading our datasets
hospital_data = read.csv("../data/raw/HortonGeneralHospital.csv")
# who_data = read.csv("../data/raw/who.csv")

# let's first check the structure of our hospital data
str(hospital_data)
names(hospital_data)

# exploring the hospital dataset
summary(hospital_data)

# creating a proper date column for our time series analysis
hospital_data = hospital_data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# creating incident period flag
hospital_data = hospital_data %>%
  mutate(incident_period = ifelse(date >= as.Date("2003-12-01") &
                                    date <= as.Date("2004-02-28"),
                                  "Incident Period", "Normal Period"))

# let's check our data after creating the date column
head(hospital_data)

# creating time series objects for each metric
admissions_ts = ts(hospital_data$Adm, frequency = 12, start = c(2000, 1))
cardio_ts = ts(hospital_data$Cardio, frequency = 12, start = c(2000, 1))
resp_ts = ts(hospital_data$Resp, frequency = 12, start = c(2000, 1))
hypo_ts = ts(hospital_data$Hypo, frequency = 12, start = c(2000, 1))

# fitting ETS models and generating forecasts
admissions_forecast = fit_ets_forecast(admissions_ts)
cardio_forecast = fit_ets_forecast(cardio_ts)
resp_forecast = fit_ets_forecast(resp_ts)
hypo_forecast = fit_ets_forecast(hypo_ts)

# detecting anomalies in each time series
admissions_anomalies = detect_zscore_anomalies(admissions_ts)
cardio_anomalies = detect_zscore_anomalies(cardio_ts)
resp_anomalies = detect_zscore_anomalies(resp_ts)
hypo_anomalies = detect_zscore_anomalies(hypo_ts)

# preparing data frames for visualization
admissions_df = data.frame(
  date = as.Date(paste(floor(time(admissions_ts)), 
                       round((time(admissions_ts) - floor(time(admissions_ts))) * 12 + 1), 
                       "01", sep = "-")),
  value = as.numeric(admissions_ts),
  is_anomaly = admissions_anomalies
)

cardio_df = data.frame(
  date = as.Date(paste(floor(time(cardio_ts)), 
                       round((time(cardio_ts) - floor(time(cardio_ts))) * 12 + 1), 
                       "01", sep = "-")),
  value = as.numeric(cardio_ts),
  is_anomaly = cardio_anomalies
)

resp_df = data.frame(
  date = as.Date(paste(floor(time(resp_ts)), 
                       round((time(resp_ts) - floor(time(resp_ts))) * 12 + 1), 
                       "01", sep = "-")),
  value = as.numeric(resp_ts),
  is_anomaly = resp_anomalies
)

hypo_df = data.frame(
  date = as.Date(paste(floor(time(hypo_ts)), 
                       round((time(hypo_ts) - floor(time(hypo_ts))) * 12 + 1), 
                       "01", sep = "-")),
  value = as.numeric(hypo_ts),
  is_anomaly = hypo_anomalies
)

# ============================================================================
# Step 2: Creating Visualizations
# ============================================================================

# Define a custom theme for better aesthetics
custom_theme = theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold")
  )

# i. Plot admissions over time
p1 = plot_time_series_with_anomalies(admissions_df, "date", "value", "is_anomaly",
                                     "Monthly Emergency Room Admissions Over Time") +
  custom_theme +
  labs(x = "Date", y = "Number of Admissions")
save_high_quality_plot(p1, "admissions_over_time.png")

# # ii. Plot top regional life expectancy
# life_expectancy = who_data %>%
#   filter(IND_NAME == "Life expectancy at birth (years)" & 
#            DIM_1_CODE == "SEX_BTSX" & 
#            DIM_TIME_YEAR == 2021) %>%
#   group_by(DIM_GEO_NAME) %>%
#   summarize(avg_life_expectancy = mean(VALUE_NUMERIC, na.rm = TRUE)) %>%
#   arrange(desc(avg_life_expectancy)) %>%
#   head(10)
# 
# p2 = ggplot(life_expectancy, aes(x = reorder(DIM_GEO_NAME, avg_life_expectancy), 
#                                  y = avg_life_expectancy)) +
#   geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
#   coord_flip() +
#   labs(title = "Top 10 Regions by Life Expectancy (2021)",
#        x = "Region",
#        y = "Life Expectancy (Years)") +
#   custom_theme
# save_high_quality_plot(p2, "top_regional_life_expectancy.png")

# iii. Plot ER admissions patterns
p3 = hospital_data %>%
  mutate(month_name = month(date, label = TRUE)) %>%
  ggplot(aes(x = month_name, y = Adm)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(title = "Seasonal Patterns in Emergency Room Admissions",
       x = "Month",
       y = "Number of Admissions") +
  custom_theme
save_high_quality_plot(p3, "ER_admissions_patterns.png")

# iv. Plot critical care events patterns
monthly_avg = hospital_data %>%
  group_by(month) %>%
  summarize(avg_admissions = mean(Adm),
            avg_cardio = mean(Cardio),
            avg_resp = mean(Resp),
            avg_hypo = mean(Hypo))

p4 = monthly_avg %>%
  pivot_longer(cols = c(avg_cardio, avg_resp, avg_hypo),
               names_to = "arrest_type",
               values_to = "value") %>%
  ggplot(aes(x = month, y = value, color = arrest_type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Monthly Patterns of Critical Care Events",
       x = "Month",
       y = "Average Number of Events",
       color = "Event Type") +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(values = c("avg_cardio" = "red",
                                "avg_resp" = "blue",
                                "avg_hypo" = "green"),
                     labels = c("Cardio-respiratory",
                                "Respiratory",
                                "Hypoglycaemic")) +
  custom_theme
save_high_quality_plot(p4, "critical_care_events_patterns.png")

# v. Plot critical care to admissions proportion
p5 = hospital_data %>%
  mutate(total_critical = Cardio + Resp + Hypo,
         critical_proportion = total_critical / Adm) %>%
  group_by(month) %>%
  summarize(avg_proportion = mean(critical_proportion)) %>%
  ggplot(aes(x = month, y = avg_proportion)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Monthly Proportion of Critical Care Events to Admissions",
       x = "Month",
       y = "Proportion of Critical Care Events") +
  scale_x_continuous(breaks = 1:12) +
  custom_theme
save_high_quality_plot(p5, "critical_care_to_admissions_proportion.png")

# vi. Plot correlation matrix
correlation_matrix = cor(hospital_data[, c("Adm", "Cardio", "Resp", "Hypo")])
p6 = plot_correlation_matrix(correlation_matrix,
                             "Correlation Between Admissions and Critical Care Transfers",
                             "correlation_matrix_admissions_and_transfers.png") +
  custom_theme

# vii. Plot incident period comparison
incident_summary = hospital_data %>%
  group_by(incident_period) %>%
  summarize(avg_admissions = mean(Adm),
            avg_cardio = mean(Cardio),
            avg_resp = mean(Resp),
            avg_hypo = mean(Hypo),
            .groups = 'drop')

p7 = incident_summary %>%
  pivot_longer(cols = c(avg_admissions, avg_cardio, avg_resp, avg_hypo),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = forcats::fct_recode(metric,
                                      "Average Admissions" = "avg_admissions",
                                      "Average Cardio" = "avg_cardio",
                                      "Average Resp" = "avg_resp",
                                      "Average Hypo" = "avg_hypo")) %>%
  ggplot(aes(x = incident_period, y = value, fill = incident_period)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Comparison of Average Metrics During Incident vs Normal Period",
       x = "Period",
       y = "Average Count",
       fill = "Incident Period") +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  custom_theme
save_high_quality_plot(p7, "avg_metrics_incident_vs_normal.png")

# # viii. Plot global health indicators
# global_health = who_data %>%
#   filter(IND_NAME %in% c("Life expectancy at birth (years)",
#                          "Healthy life expectancy at birth (years)",
#                          "Maternal mortality ratio (per 100 000 live births)",
#                          "Under-five mortality rate (per 1000 live births)") &
#            DIM_TIME_YEAR == 2021) %>%
#   dplyr::select(IND_NAME, DIM_GEO_NAME, VALUE_NUMERIC)
# 
# p8 = ggplot(global_health, aes(x = IND_NAME, y = VALUE_NUMERIC)) +
#   geom_boxplot(fill = "steelblue", alpha = 0.7) +
#   coord_flip() +
#   labs(title = "Distribution of Global Health Indicators (2021)",
#        x = "Health Indicator",
#        y = "Value") +
#   custom_theme
# save_high_quality_plot(p8, "global_health_indicators.png")

# ============================================================================
# Step 3: Saving Results
# ============================================================================

# Basic summary statistics
basic_summary = hospital_data %>%
  dplyr::select(Adm, Cardio, Resp, Hypo) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    q1 = ~quantile(., 0.25, na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    q3 = ~quantile(., 0.75, na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = Variable,
              values_from = value) %>%
  arrange(factor(Statistic, 
                 levels = c("mean", "sd", "min", "q1", "median", "q3", "max")))

# Monthly statistics
monthly_stats = hospital_data %>%
  group_by(month) %>%
  summarize(
    mean_admissions = mean(Adm),
    sd_admissions = sd(Adm),
    mean_cardio = mean(Cardio),
    sd_cardio = sd(Cardio),
    mean_resp = mean(Resp),
    sd_resp = sd(Resp),
    mean_hypo = mean(Hypo),
    sd_hypo = sd(Hypo)
  ) %>%
  mutate(month = month.abb[month]) %>%
  arrange(match(month, month.abb))

# Incident period comparison
incident_stats = hospital_data %>%
  group_by(incident_period) %>%
  summarize(
    n_months = n(),
    mean_admissions = mean(Adm),
    sd_admissions = sd(Adm),
    mean_cardio = mean(Cardio),
    sd_cardio = sd(Cardio),
    mean_resp = mean(Resp),
    sd_resp = sd(Resp),
    mean_hypo = mean(Hypo),
    sd_hypo = sd(Hypo)
  )

# Correlation statistics
corr_stats = correlation_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# # Global health summary
# global_stats = global_health %>%
#   group_by(IND_NAME) %>%
#   summarize(
#     mean_value = mean(VALUE_NUMERIC, na.rm = TRUE),
#     median_value = median(VALUE_NUMERIC, na.rm = TRUE),
#     sd_value = sd(VALUE_NUMERIC, na.rm = TRUE),
#     min_value = min(VALUE_NUMERIC, na.rm = TRUE),
#     max_value = max(VALUE_NUMERIC, na.rm = TRUE),
#     n_countries = n()
#   ) %>%
#   mutate(across(where(is.numeric), ~round(., 2)))

# Saving all summary statistics
write.csv(basic_summary, 
          "../data/processed/basic_summary_stats.csv", 
          row.names = FALSE)

write.csv(monthly_stats, 
          "../data/processed/monthly_summary_stats.csv", 
          row.names = FALSE)

write.csv(incident_stats, 
          "../data/processed/incident_period_stats.csv", 
          row.names = FALSE)

write.csv(corr_stats, 
          "../data/processed/correlation_stats.csv", 
          row.names = FALSE)

# write.csv(global_stats, 
#           "../data/processed/global_health_stats.csv", 
#           row.names = FALSE)

# ~END OF R SCRIPT 01_exploratory_data_analysis~