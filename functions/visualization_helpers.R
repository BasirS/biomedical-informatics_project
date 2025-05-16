# Visualization Helper Functions

#' plotting time series with anomalies
#' @param data data frame containing time series data
#' @param date_col name of the date column
#' @param value_col name of the value column
#' @param anomaly_col name of the anomaly indicator column
#' @param title plot title
#' @return ggplot object
plot_time_series_with_anomalies = function(df, date_col, value_col, anomaly_col, title) {
  ggplot(df, aes_string(x = date_col, y = value_col)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(data = df[df[[anomaly_col]], ], aes_string(x = date_col, y = value_col), color = "red", size = 2) +
    labs(title = title, x = "Date", y = "Value") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}

#'plotting forecast results
#' @param forecast_obj forecast object from forecast package
#' @param actual_data original time series data
#' @param title plot title
#' @return ggplot object
plot_forecast_results = function(forecast_obj, actual_data, title) {
  autoplot(forecast_obj) +
    autolayer(actual_data) +
    labs(title = title) +
    theme_minimal()
}

#' plotting performance metrics
#' @param metrics data frame containing performance metrics
#' @param metric_col name of the metric column
#' @param method_col name of the method column
#' @param title plot title
#' @return ggplot object
plot_performance_metrics = function(metrics, metric_col, method_col, title) {
  ggplot(metrics, aes_string(x = method_col, y = metric_col)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = title,
         x = "Method",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' saving high-quality visualization
#' @param plot ggplot object to save
#' @param filename Name of the output file
#' @param width Width in inches (default: 12)
#' @param height Height in inches (default: 8)
#' @param dpi Resolution in dots per inch (default: 300)
#' @return NULL
save_high_quality_plot = function(plot, filename, width = 12, height = 8, dpi = 300) {
  ggsave(filename, plot = plot, width = width, height = height, dpi = dpi)
}

#' plotting seasonal patterns
#' @param ts_data time series data
#' @param title plot title
#' @param output_path path to save the plot
#' @return NULL
plot_seasonal_patterns = function(ts_data, title, output_path) {
  # Convert time series to data frame
  ts_df = data.frame(
    value = as.numeric(ts_data),
    month = cycle(ts_data)
  )
  
  # create the plot
  p = ggplot(ts_df, aes(x = factor(month), y = value)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    labs(title = title,
         x = "Month",
         y = "Value") +
    theme_minimal() +
    scale_x_discrete(labels = month.abb)
  
  # save the plot
  save_high_quality_plot(p, basename(output_path))
}

#' plotting combined seasonal patterns
#' @param monthly_data data frame with monthly averages
#' @param title plot title
#' @param output_path path to save the plot
#' @return NULL
plot_combined_seasonal = function(monthly_data, title, output_path) {
  # create the plot
  p = monthly_data %>%
    pivot_longer(cols = c(avg_cardio, avg_resp, avg_hypo),
                 names_to = "arrest_type",
                 values_to = "value") %>%
    ggplot(aes(x = month, y = value, color = arrest_type)) +
    geom_line() +
    geom_point() +
    labs(title = title,
         x = "Month",
         y = "Average Number of Events",
         color = "Event Type") +
    theme_minimal() +
    scale_x_continuous(breaks = 1:12) +
    scale_color_manual(values = c("avg_cardio" = "red",
                                 "avg_resp" = "blue",
                                 "avg_hypo" = "green"),
                      labels = c("Cardio-respiratory",
                               "Respiratory",
                               "Hypoglycaemic"))
  
  # save the plot
  save_high_quality_plot(p, basename(output_path))
}

#' plotting correlation matrix
#' @param cor_matrix correlation matrix
#' @param title plot title
#' @param output_path path to save the plot
#' @return NULL
plot_correlation_matrix = function(cor_matrix, title, output_path) {
  # convert correlation matrix to long format for ggplot
  corr_data = as.data.frame(cor_matrix) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
    mutate(var1 = factor(var1, levels = rownames(cor_matrix)),
           var2 = factor(var2, levels = colnames(cor_matrix)))
  
  # create the plot
  p = ggplot(corr_data, aes(x = var1, y = var2, fill = correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c(-1, 1), space = "Lab",
                        name = "Correlation") +
    geom_text(aes(label = round(correlation, 2)), color = "black", size = 4) +
    labs(title = title,
         x = "", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # save the plot
  save_high_quality_plot(p, basename(output_path))
}

# hospital data visualization helpers only

# plotting time series with anomalies
detect_zscore_anomalies = function(ts_data, threshold = 2) {
  z_scores = scale(ts_data)
  anomalies = abs(z_scores) > threshold
  return(anomalies)
}