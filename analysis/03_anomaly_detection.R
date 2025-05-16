# ============================================================================
# Step 1: Setting Up R Environment
# ============================================================================

# loading required packages
library(tidyverse)
library(forecast)
library(tseries)
library(zoo)
library(lubridate)
library(scales)
library(qcc)  # for CUSUM charts
library(anomalize)  # for anomaly detection
library(ggplot2)
library(patchwork)
library(pROC)
library(caret)

# ============================================================================
# Step 2: Loading and Preparing Data
# ============================================================================

# loading hospital data
hospital_data = read.csv("../data/raw/HortonGeneralHospital.csv")
# who_data = read.csv("../data/raw/who.csv")

# creating date column
hospital_data = hospital_data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# creating time series objects
admissions_ts = ts(hospital_data$Adm, 
                   frequency = 12, 
                   start = c(min(hospital_data$year), min(hospital_data$month)))

cardio_ts = ts(hospital_data$Cardio, 
               frequency = 12, 
               start = c(min(hospital_data$year), min(hospital_data$month)))

resp_ts = ts(hospital_data$Resp, 
             frequency = 12, 
             start = c(min(hospital_data$year), min(hospital_data$month)))

hypo_ts = ts(hospital_data$Hypo, 
             frequency = 12, 
             start = c(min(hospital_data$year), min(hospital_data$month)))

# ============================================================================
# Step 3: Z-Score Based Anomaly Detection
# ============================================================================

# function to detect anomalies using z-scores
detect_anomalies_zscore = function(ts_data, threshold = 2) {
  # calculate z-scores
  z_scores = scale(ts_data)
  
  # identify anomalies
  anomalies = abs(z_scores) > threshold
  
  # create results data frame
  results = data.frame(
    date = time(ts_data),
    value = as.numeric(ts_data),
    z_score = as.numeric(z_scores),
    is_anomaly = anomalies
  )
  
  return(results)
}

# detecting anomalies for each series
admissions_anomalies = detect_anomalies_zscore(admissions_ts)
cardio_anomalies = detect_anomalies_zscore(cardio_ts)
resp_anomalies = detect_anomalies_zscore(resp_ts)
hypo_anomalies = detect_anomalies_zscore(hypo_ts)

# ============================================================================
# Step 4: CUSUM Chart Analysis
# ============================================================================

# function to perform CUSUM analysis
perform_cusum = function(ts_data, k = 0.5, h = 5) {
  # convert time series to vector
  data_vector = as.numeric(ts_data)
  
  # calculate mean and standard deviation
  mean_value = mean(data_vector)
  sd_value = sd(data_vector)
  
  # calculate CUSUM
  cusum_result = cusum(data_vector, 
                       center = mean_value,
                       std.dev = sd_value,
                       decision.interval = h,
                       se.shift = k,
                       plot = FALSE)
  
  # get time indices
  time_indices = time(ts_data)
  
  # create results data frame
  results = data.frame(
    date = time_indices,
    value = data_vector,
    cusum = cusum_result$pos,  # use only positive CUSUM
    is_anomaly = FALSE  # initialize all as non-anomalies
  )
  
  # mark anomalies where CUSUM exceeds decision interval
  results$is_anomaly = abs(results$cusum) > h
  
  return(results)
}

# performing CUSUM analysis for each series
admissions_cusum = perform_cusum(admissions_ts)
cardio_cusum = perform_cusum(cardio_ts)
resp_cusum = perform_cusum(resp_ts)
hypo_cusum = perform_cusum(hypo_ts)

# ============================================================================
# Step 5: Residual Analysis from Time Series Models
# ============================================================================

# function to analyze residuals
analyze_residuals = function(ts_data) {
  # print input data length
  print(paste("Input data length:", length(ts_data)))
  
  # convert to numeric vector
  data_vector = as.numeric(ts_data)
  
  # calculate simple moving average
  ma = rollmean(data_vector, k = 3, fill = NA)
  
  # calculate residuals as difference from moving average
  residuals = data_vector - ma
  
  # calculate z-scores for residuals
  z_scores = scale(residuals)
  
  # identify anomalies (points with |z-score| > 2)
  anomalies = abs(z_scores) > 2
  
  # create results data frame
  results = data.frame(
    date = time(ts_data),
    value = data_vector,
    residual = residuals,
    is_anomaly = anomalies
  )
  
  # print results dimensions
  print(paste("Results data frame dimensions:", nrow(results), "x", ncol(results)))
  
  return(results)
}

# analyzing residuals for each series
print("Analyzing admissions residuals...")
admissions_residuals = analyze_residuals(admissions_ts)

print("Analyzing cardio-respiratory residuals...")
cardio_residuals = analyze_residuals(cardio_ts)

print("Analyzing respiratory residuals...")
resp_residuals = analyze_residuals(resp_ts)

print("Analyzing hypoglycaemic residuals...")
hypo_residuals = analyze_residuals(hypo_ts)

# ============================================================================
# Step 6: WHO Indicators Integration
# ============================================================================

# # function to integrate WHO indicators
# integrate_who_indicators = function(hospital_data, who_data) {
#   # filter relevant WHO indicators
#   relevant_indicators = who_data %>%
#     filter(IND_NAME %in% c(
#       "Life expectancy at birth (years)",
#       "Healthy life expectancy at birth (years)",
#       "Maternal mortality ratio (per 100 000 live births)",
#       "Under-five mortality rate (per 1000 live births)"
#     ))
#   
#   # calculate yearly averages for WHO indicators
#   yearly_indicators = relevant_indicators %>%
#     group_by(DIM_TIME_YEAR, IND_NAME) %>%
#     summarize(avg_value = mean(VALUE_NUMERIC, na.rm = TRUE))
#   
#   # merge with hospital data
#   combined_data = hospital_data %>%
#     left_join(yearly_indicators, by = c("year" = "DIM_TIME_YEAR"))
#   
#   return(combined_data)
# }
# 
# # integrating WHO indicators
# combined_data = integrate_who_indicators(hospital_data, who_data)

# ============================================================================
# Step 7: Threshold Determination
# ============================================================================

# function to determine optimal thresholds
determine_thresholds = function(ts_data, method = "zscore") {
  if(method == "zscore") {
    # use standard deviation based threshold
    threshold = 2 * sd(ts_data)
  } else if(method == "iqr") {
    # use IQR based threshold
    q = quantile(ts_data, probs = c(0.25, 0.75))
    threshold = 1.5 * (q[2] - q[1])
  }
  
  return(threshold)
}

# determining thresholds for each series
admissions_threshold = determine_thresholds(admissions_ts)
cardio_threshold = determine_thresholds(cardio_ts)
resp_threshold = determine_thresholds(resp_ts)
hypo_threshold = determine_thresholds(hypo_ts)

# ============================================================================
# Step 8: Visualization of Anomalies
# ============================================================================

# define custom theme for better aesthetics
custom_theme = theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold")
  )

# function to plot anomalies
plot_anomalies = function(data, title, method) {
  # ensure data has the required columns
  if(!"is_anomaly" %in% names(data)) {
    data$is_anomaly = FALSE
  }
  
  # create the plot
  p = ggplot(data, aes(x = date, y = value)) +
    geom_line() +
    geom_point(data = data[data$is_anomaly, ], 
               aes(color = "Anomaly"), size = 2) +
    labs(title = paste(title, "-", method),
         x = "Date",
         y = "Value") +
    theme_minimal() +
    scale_color_manual(values = c("Anomaly" = "red"))
  
  return(p)
}

# create z-score anomaly plots (2x2 grid)
zscore_plots = list(
  admissions = plot_anomalies(admissions_anomalies, "Emergency Room Admissions", "Z-Score") + custom_theme,
  cardio = plot_anomalies(cardio_anomalies, "Cardio-respiratory Events", "Z-Score") + custom_theme,
  respiratory = plot_anomalies(resp_anomalies, "Respiratory Events", "Z-Score") + custom_theme,
  hypoglycaemic = plot_anomalies(hypo_anomalies, "Hypoglycaemic Events", "Z-Score") + custom_theme
)

# save combined z-score plot
combined_zscore = (zscore_plots$admissions | zscore_plots$cardio) / 
  (zscore_plots$respiratory | zscore_plots$hypoglycaemic)
save_high_quality_plot(combined_zscore, "combined_zscore_anomalies.png", width = 16, height = 12)

# create cusum anomaly plots (2x2 grid)
cusum_plots = list(
  admissions = plot_anomalies(admissions_cusum, "Emergency Room Admissions", "CUSUM") + custom_theme,
  cardio = plot_anomalies(cardio_cusum, "Cardio-respiratory Events", "CUSUM") + custom_theme,
  respiratory = plot_anomalies(resp_cusum, "Respiratory Events", "CUSUM") + custom_theme,
  hypoglycaemic = plot_anomalies(hypo_cusum, "Hypoglycaemic Events", "CUSUM") + custom_theme
)

# save combined cusum plot
combined_cusum = (cusum_plots$admissions | cusum_plots$cardio) / 
  (cusum_plots$respiratory | cusum_plots$hypoglycaemic)
save_high_quality_plot(combined_cusum, "combined_cusum_anomalies.png", width = 16, height = 12)

# create residual anomaly plots (2x2 grid)
residual_plots = list(
  admissions = plot_anomalies(admissions_residuals, "Emergency Room Admissions", "Residual") + custom_theme,
  cardio = plot_anomalies(cardio_residuals, "Cardio-respiratory Events", "Residual") + custom_theme,
  respiratory = plot_anomalies(resp_residuals, "Respiratory Events", "Residual") + custom_theme,
  hypoglycaemic = plot_anomalies(hypo_residuals, "Hypoglycaemic Events", "Residual") + custom_theme
)

# save combined residual plot
combined_residual = (residual_plots$admissions | residual_plots$cardio) / 
  (residual_plots$respiratory | residual_plots$hypoglycaemic)
save_high_quality_plot(combined_residual, "combined_residual_anomalies.png", width = 16, height = 12)

# # create who indicators correlation plot
# # First, let's check what data we have
# print("WHO data structure:")
# print(str(combined_data))

# # Create a more focused dataset for correlation
# who_data_clean = combined_data %>% 
#   dplyr::select(Adm, Cardio, Resp, Hypo, avg_value, IND_NAME) %>%  # Keep IND_NAME
#   rename("WHO Indicator" = avg_value) %>%
#   na.omit() %>%
#   group_by(IND_NAME) %>%  # Now we can group by indicator name
#   summarize(
#     Adm = mean(Adm, na.rm = TRUE),
#     Cardio = mean(Cardio, na.rm = TRUE),
#     Resp = mean(Resp, na.rm = TRUE),
#     Hypo = mean(Hypo, na.rm = TRUE),
#     `WHO Indicator` = mean(`WHO Indicator`, na.rm = TRUE)
#   )

# print("Cleaned WHO data structure:")
# print(str(who_data_clean))
# 
# # Create correlation matrix manually if needed
# if(nrow(who_data_clean) > 0) {
#   who_correlation = plot_correlation_matrix(
#     who_data_clean %>% dplyr::select(-IND_NAME),  # Remove IND_NAME for correlation
#     "Correlation with WHO Indicators",
#     output_path = "who_indicators_correlation.png"
#   ) + custom_theme
#   save_high_quality_plot(who_correlation, "who_indicators_correlation.png", width = 12, height = 8)
# } else {
#   print("Warning: No valid data for WHO indicators correlation plot")
# }

# ============================================================================
# Step 9: Export Results
# ============================================================================

# function to create anomaly summary
create_anomaly_summary = function(zscore_data, cusum_data, residual_data, title) {
  # print dimensions of input data frames
  print(paste("Creating summary for", title))
  
  # create a base data frame with dates and values
  summary = data.frame(
    date = as.Date(zscore_data$date),
    value = as.numeric(zscore_data$value),
    series = as.character(title),
    zscore_anomaly = as.logical(if("is_anomaly" %in% names(zscore_data)) zscore_data$is_anomaly else FALSE),
    cusum_anomaly = as.logical(if("is_anomaly" %in% names(cusum_data)) cusum_data$is_anomaly else FALSE),
    residual_anomaly = as.logical(if("is_anomaly" %in% names(residual_data)) residual_data$is_anomaly else FALSE),
    stringsAsFactors = FALSE
  )
  
  # add method-specific values if available
  if("z_score" %in% names(zscore_data)) {
    summary$z_score = as.numeric(zscore_data$z_score)
  } else {
    summary$z_score = NA
  }
  
  if("cusum" %in% names(cusum_data)) {
    summary$cusum = as.numeric(cusum_data$cusum)
  } else {
    summary$cusum = NA
  }
  
  if("residual" %in% names(residual_data)) {
    summary$residual = as.numeric(residual_data$residual)
  } else {
    summary$residual = NA
  }
  
  # print summary of the created data frame
  print(paste("Created summary with", nrow(summary), "rows and", ncol(summary), "columns"))
  print("Column names:")
  print(names(summary))
  
  return(summary)
}

# function to combine summaries
combine_summaries = function() {
  # create individual summaries
  print("Creating individual summaries...")
  
  admissions_summary = create_anomaly_summary(
    admissions_anomalies, 
    admissions_cusum, 
    admissions_residuals, 
    "Admissions"
  )
  
  cardio_summary = create_anomaly_summary(
    cardio_anomalies, 
    cardio_cusum, 
    cardio_residuals, 
    "Cardio-respiratory"
  )
  
  resp_summary = create_anomaly_summary(
    resp_anomalies, 
    resp_cusum, 
    resp_residuals, 
    "Respiratory"
  )
  
  hypo_summary = create_anomaly_summary(
    hypo_anomalies, 
    hypo_cusum, 
    hypo_residuals, 
    "Hypoglycaemic"
  )
  
  # combine summaries
  print("Combining summaries...")
  all_anomalies = bind_rows(
    admissions_summary,
    cardio_summary,
    resp_summary,
    hypo_summary
  )
  
  return(all_anomalies)
}

# main execution
print("Starting summary creation and combination...")
tryCatch({
  # load dplyr if not already loaded
  if(!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  
  # create and combine summaries
  all_anomalies = combine_summaries()
  
  # verify the combined data frame
  print("Verifying combined data frame...")
  print(paste("Total rows:", nrow(all_anomalies)))
  print(paste("Total columns:", ncol(all_anomalies)))
  print("Column names:")
  print(names(all_anomalies))
  
  # save to CSV
  print("Saving results to CSV...")
  write.csv(all_anomalies, "anomaly_detection_results.csv", row.names = FALSE)
  
  # print summary statistics
  print("~ Anomaly Detection Summary ~")
  print(summary(all_anomalies))
  
}, error = function(e) {
  print(paste("Error occurred:", e$message))
  print("Attempting to recover...")
  
  # try to save individual summaries if available
  if(exists("admissions_summary")) {
    write.csv(admissions_summary, "admissions_anomalies.csv", row.names = FALSE)
  }
  if(exists("cardio_summary")) {
    write.csv(cardio_summary, "cardio_anomalies.csv", row.names = FALSE)
  }
  if(exists("resp_summary")) {
    write.csv(resp_summary, "resp_anomalies.csv", row.names = FALSE)
  }
  if(exists("hypo_summary")) {
    write.csv(hypo_summary, "hypo_anomalies.csv", row.names = FALSE)
  }
})

# source helper functions
source("../functions/anomaly_detection_functions.R")
source("../functions/visualization_helpers.R") 

# ~END OF R SCRIPT 03_anomaly_detection~