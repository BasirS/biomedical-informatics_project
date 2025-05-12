# Step 1: Loading and Cleaning Hospital Data

# load required packages
required_packages = c("tidyverse", "lubridate", "scales", "forecast", "tseries", "zoo", "car", "broom", "corrplot", "ggpubr")
new_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
library(tidyverse)
library(lubridate)
library(scales)
library(forecast)
library(tseries)
library(zoo)
library(car)
library(broom)
library(corrplot)
library(ggpubr)

# load hospital data
hospital_data = read.csv("../data/raw/HortonGeneralHospital.csv")

# clean hospital data
hospital_clean = hospital_data %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    Adm = abs(Adm),
    Cardio = abs(Cardio),
    Resp = abs(Resp),
    Hypo = abs(Hypo)
  )

# check data structure and missing values
print("hospital data structure:")
str(hospital_clean)
print("missing values in hospital data:")
print(colSums(is.na(hospital_clean)))

# Step 2: Early Warning System and Analysis
# (continue with your hospital data early warning system, anomaly detection, visualization, etc.)

# ============================================================================
# Step 3: Early Warning System Functions
# ============================================================================

# function to detect anomalies using multiple methods
detect_anomalies = function(ts_data, threshold = 2) {
  # Convert time series to numeric vector and ensure it's valid
  data_vector = as.numeric(ts_data)
  if(any(is.na(data_vector))) {
    warning("NA values found in data, replacing with mean")
    data_vector[is.na(data_vector)] = mean(data_vector, na.rm = TRUE)
  }
  
  # Get time indices
  time_indices = time(ts_data)
  
  # Z-score based detection
  z_scores = scale(data_vector)
  z_score_anomalies = abs(z_scores) > threshold
  
  # Moving average based detection
  ma = rollmean(data_vector, k = 3, fill = NA)
  ma_residuals = data_vector - ma
  ma_anomalies = abs(scale(ma_residuals)) > threshold
  
  # CUSUM based detection - simplified approach
  mean_value = mean(data_vector, na.rm = TRUE)
  sd_value = sd(data_vector, na.rm = TRUE)
  
  # Calculate CUSUM manually
  cusum_anomalies = logical(length(data_vector))
  if(length(data_vector) > 0) {
    # Calculate standardized values
    z = (data_vector - mean_value) / sd_value
    
    # Calculate CUSUM values
    cusum_pos = numeric(length(data_vector))
    cusum_neg = numeric(length(data_vector))
    
    for(i in 2:length(data_vector)) {
      cusum_pos[i] = max(0, cusum_pos[i-1] + z[i] - 0.5)
      cusum_neg[i] = min(0, cusum_neg[i-1] + z[i] + 0.5)
    }
    
    # Flag anomalies where CUSUM exceeds threshold
    cusum_anomalies = (cusum_pos > 5) | (cusum_neg < -5)
  }
  
  # Create results data frame
  results = data.frame(
    date = as.Date(paste(floor(time_indices), 
                        round((time_indices - floor(time_indices)) * 12) + 1, 
                        "01", sep = "-")),
    value = data_vector,
    z_score_anomaly = as.logical(z_score_anomalies),
    ma_anomaly = as.logical(ma_anomalies),
    cusum_anomaly = as.logical(cusum_anomalies)
  )
  
  # Add combined anomaly column
  results$combined_anomaly = results$z_score_anomaly | results$ma_anomaly | results$cusum_anomaly
  
  return(results)
}

# function to evaluate early warning system
evaluate_system = function(anomaly_results, incident_start, incident_end) {
  # Ensure anomaly_results is a data frame with required columns
  if(!is.data.frame(anomaly_results) || 
     !all(c("date", "value", "combined_anomaly") %in% names(anomaly_results))) {
    stop("anomaly_results must be a data frame with 'date', 'value', and 'combined_anomaly' columns")
  }
  
  # Create ground truth
  ground_truth = anomaly_results %>%
    mutate(
      is_incident = date >= incident_start & date <= incident_end,
      true_positive = combined_anomaly & is_incident,
      false_positive = combined_anomaly & !is_incident,
      true_negative = !combined_anomaly & !is_incident,
      false_negative = !combined_anomaly & is_incident
    )
  
  # Calculate performance metrics
  tp = sum(ground_truth$true_positive, na.rm = TRUE)
  fp = sum(ground_truth$false_positive, na.rm = TRUE)
  tn = sum(ground_truth$true_negative, na.rm = TRUE)
  fn = sum(ground_truth$false_negative, na.rm = TRUE)
  
  # Calculate metrics with error handling
  metrics = list(
    sensitivity = if(tp + fn > 0) tp / (tp + fn) else 0,
    specificity = if(tn + fp > 0) tn / (tn + fp) else 0,
    ppv = if(tp + fp > 0) tp / (tp + fp) else 0,
    npv = if(tn + fn > 0) tn / (tn + fn) else 0
  )
  
  # Calculate F1 score
  metrics$f1_score = if(metrics$sensitivity + metrics$ppv > 0) {
    2 * (metrics$sensitivity * metrics$ppv) / (metrics$sensitivity + metrics$ppv)
  } else {
    0
  }
  
  return(list(ground_truth = ground_truth, metrics = metrics))
}

# ============================================================================
# Step 4: Running the Early Warning System
# ============================================================================

# define incident period
incident_start = as.Date("2003-12-01")
incident_end = as.Date("2004-02-28")

# run anomaly detection for each series with error handling
tryCatch({
  print("Running anomaly detection for admissions...")
  admissions_anomalies = detect_anomalies(admissions_ts)
  
  print("Running anomaly detection for cardio-respiratory events...")
  cardio_anomalies = detect_anomalies(cardio_ts)
  
  print("Running anomaly detection for respiratory events...")
  resp_anomalies = detect_anomalies(resp_ts)
  
  print("Running anomaly detection for hypoglycaemic events...")
  hypo_anomalies = detect_anomalies(hypo_ts)
  
  # evaluate system performance
  print("Evaluating system performance...")
  admissions_eval = evaluate_system(admissions_anomalies, incident_start, incident_end)
  cardio_eval = evaluate_system(cardio_anomalies, incident_start, incident_end)
  resp_eval = evaluate_system(resp_anomalies, incident_start, incident_end)
  hypo_eval = evaluate_system(hypo_anomalies, incident_start, incident_end)
}, error = function(e) {
  print(paste("Error occurred:", e$message))
  stop("Failed to complete anomaly detection and evaluation")
})

# ============================================================================
# Step 5: Visualization and Reporting
# ============================================================================

# function to plot results
plot_results = function(anomaly_results, title) {
  # Create incident period rectangle data
  incident_rect = data.frame(
    xmin = incident_start,
    xmax = incident_end,
    ymin = min(anomaly_results$value, na.rm = TRUE),
    ymax = max(anomaly_results$value, na.rm = TRUE)
  )
  
  # Create the plot
  p = ggplot(anomaly_results, aes(x = date, y = value)) +
    # Add incident period rectangle
    geom_rect(data = incident_rect,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    # Add time series line
    geom_line() +
    # Add anomaly points
    geom_point(data = subset(anomaly_results, combined_anomaly), 
               aes(color = "Anomaly"), size = 2) +
    # Add labels and theme
    labs(title = title,
         x = "Date",
         y = "Value") +
    theme_minimal() +
    scale_color_manual(values = c("Anomaly" = "red"))
  
  return(p)
}

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

# create early warning plots (2x2 grid)
warning_plots = list(
  admissions = plot_results(admissions_anomalies, "Emergency Room Admissions") + custom_theme,
  cardio = plot_results(cardio_anomalies, "Cardio-respiratory Events") + custom_theme,
  respiratory = plot_results(resp_anomalies, "Respiratory Events") + custom_theme,
  hypoglycaemic = plot_results(hypo_anomalies, "Hypoglycaemic Events") + custom_theme
)

# save combined warning plot
combined_warning = (warning_plots$admissions | warning_plots$cardio) / 
                  (warning_plots$respiratory | warning_plots$hypoglycaemic)
save_high_quality_plot(combined_warning, "early_warning_results.png", width = 16, height = 12)

# create performance metrics bar plot
performance_data = data.frame(
  Metric = c("Admissions", "Cardio-respiratory", "Respiratory", "Hypoglycaemic"),
  Sensitivity = c(
    admissions_eval$metrics$sensitivity,
    cardio_eval$metrics$sensitivity,
    resp_eval$metrics$sensitivity,
    hypo_eval$metrics$sensitivity
  ),
  Specificity = c(
    admissions_eval$metrics$specificity,
    cardio_eval$metrics$specificity,
    resp_eval$metrics$specificity,
    hypo_eval$metrics$specificity
  ),
  F1_Score = c(
    admissions_eval$metrics$f1_score,
    cardio_eval$metrics$f1_score,
    resp_eval$metrics$f1_score,
    hypo_eval$metrics$f1_score
  )
)

# create performance metrics bar plot
performance_long = performance_data %>%
  pivot_longer(-Metric, names_to = "Measure", values_to = "Value")

performance_bar = ggplot(performance_long, aes(x = Metric, y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Early Warning System Performance Metrics",
       x = "Metric",
       y = "Value",
       fill = "Measure") +
  scale_fill_brewer(palette = "Set2") +
  custom_theme
save_high_quality_plot(performance_bar, "performance_metrics_bar.png", width = 12, height = 8)

# ============================================================================
# Step 6: Performance Summary
# ============================================================================

# create performance summary
performance_summary = data.frame(
  Metric = c("Admissions", "Cardio-respiratory", "Respiratory", "Hypoglycaemic"),
  Sensitivity = c(
    admissions_eval$metrics$sensitivity,
    cardio_eval$metrics$sensitivity,
    resp_eval$metrics$sensitivity,
    hypo_eval$metrics$sensitivity
  ),
  Specificity = c(
    admissions_eval$metrics$specificity,
    cardio_eval$metrics$specificity,
    resp_eval$metrics$specificity,
    hypo_eval$metrics$specificity
  ),
  PPV = c(
    admissions_eval$metrics$ppv,
    cardio_eval$metrics$ppv,
    resp_eval$metrics$ppv,
    hypo_eval$metrics$ppv
  ),
  NPV = c(
    admissions_eval$metrics$npv,
    cardio_eval$metrics$npv,
    resp_eval$metrics$npv,
    hypo_eval$metrics$npv
  ),
  F1_Score = c(
    admissions_eval$metrics$f1_score,
    cardio_eval$metrics$f1_score,
    resp_eval$metrics$f1_score,
    hypo_eval$metrics$f1_score
  )
)

# save performance summary
write.csv(performance_summary, "early_warning_performance.csv", row.names = FALSE)

# print performance summary
print("~ Early Warning System Performance Summary ~")
print(performance_summary)

# ============================================================================
# Step 7: Export Detailed Results
# ============================================================================

# function to create detailed results
create_detailed_results = function(anomaly_results, eval_results, title) {
  results = anomaly_results %>%
    mutate(
      is_incident = date >= incident_start & date <= incident_end,
      true_positive = combined_anomaly & is_incident,
      false_positive = combined_anomaly & !is_incident,
      true_negative = !combined_anomaly & !is_incident,
      false_negative = !combined_anomaly & is_incident
    )
  
  results$series = title
  
  return(results)
}

# create detailed results for each series
detailed_results = bind_rows(
  create_detailed_results(admissions_anomalies, admissions_eval, "Admissions"),
  create_detailed_results(cardio_anomalies, cardio_eval, "Cardio-respiratory"),
  create_detailed_results(resp_anomalies, resp_eval, "Respiratory"),
  create_detailed_results(hypo_anomalies, hypo_eval, "Hypoglycaemic")
)

# save detailed results
write.csv(detailed_results, "early_warning_detailed_results.csv", row.names = FALSE) 

# ~END OF R SCRIPT 04_early_warning_system~