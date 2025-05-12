# hospital data anomaly detection helper functions only

#' detecting anomalies using Z-score method
#' @param ts_data numeric vector of data
#' @param threshold z-score threshold (default: 2)
#' @return logical vector indicating anomalies
detect_zscore_anomalies = function(ts_data, threshold = 2) {
  z_scores = scale(ts_data)
  anomalies = abs(z_scores) > threshold
  return(anomalies)
}

#' detecting anomalies using moving average method
#' @param ts_data numeric vector of data
#' @param window size of moving window
#' @param threshold number of standard deviations
#' @return logical vector indicating anomalies
detect_ma_anomalies = function(ts_data, window = 3, threshold = 2) {
  ma = zoo::rollmean(ts_data, k = window, fill = NA)
  residuals = ts_data - ma
  z_scores = scale(residuals)
  anomalies = abs(z_scores) > threshold
  return(anomalies)
}

#' detecting anomalies using CUSUM method
#' @param ts_data numeric vector of data
#' @param k CUSUM threshold
#' @param h CUSUM threshold
#' @return logical vector indicating anomalies
detect_cusum_anomalies = function(ts_data, k = 0.5, h = 5) {
  data_vector = as.numeric(ts_data)
  mean_value = mean(data_vector)
  sd_value = sd(data_vector)
  cusum = cumsum(data_vector - mean_value - k * sd_value)
  anomalies = abs(cusum) > h * sd_value
  return(anomalies)
}

#' evaluating anomaly detection performance
#' @param anomalies logical vector of detected anomalies
#' @param incident_period logical vector of known incident period
#' @return list of performance metrics
evaluate_anomaly_detection = function(anomalies, incident_period) {
  # calculating confusion matrix
  tp = sum(anomalies & incident_period)
  fp = sum(anomalies & !incident_period)
  fn = sum(!anomalies & incident_period)
  tn = sum(!anomalies & !incident_period)
  
  # calculating metrics
  sensitivity = tp / (tp + fn)
  specificity = tn / (tn + fp)
  precision = tp / (tp + fp)
  f1_score = 2 * (precision * sensitivity) / (precision + sensitivity)
  
  return(list(
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    f1_score = f1_score
  ))
} 