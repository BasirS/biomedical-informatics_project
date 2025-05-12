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

# Step 2: Time Series Modeling and Analysis
# (continue with your hospital data modeling, decomposition, forecasting, etc.)

# ============================================================================
# Step 3: Time Series Decomposition
# ============================================================================

# convert data to tsibble for better visualization
hospital_tsibble = hospital_clean %>%
  mutate(date = as.Date(date)) %>%
  as_tsibble(index = date) %>%
  fill_gaps() %>%
  fill(Adm, Cardio, Resp, Hypo, .direction = "down")

# perform decomposition for each series using feasts
admissions_decomp = hospital_tsibble %>%
  model(STL(Adm ~ trend() + season(period = "1 year"))) %>%
  components()

cardio_decomp = hospital_tsibble %>%
  model(STL(Cardio ~ trend() + season(period = "1 year"))) %>%
  components()

resp_decomp = hospital_tsibble %>%
  model(STL(Resp ~ trend() + season(period = "1 year"))) %>%
  components()

hypo_decomp = hospital_tsibble %>%
  model(STL(Hypo ~ trend() + season(period = "1 year"))) %>%
  components()

# ============================================================================
# Step 4: Stationarity Testing
# ============================================================================

# performing augmented dickey-fuller test for each series
print(adf.test(admissions_ts))
print(adf.test(cardio_ts))
print(adf.test(resp_ts))
print(adf.test(hypo_ts))

# ============================================================================
# Step 5: ARIMA Modeling
# ============================================================================

# function to find best ARIMA model
find_best_arima = function(ts_data, max_p = 2, max_d = 1, max_q = 2) {
  best_aic = Inf
  best_model = NULL
  
  for(p in 0:max_p) {
    for(d in 0:max_d) {
      for(q in 0:max_q) {
        tryCatch({
          model = arima(ts_data, order = c(p,d,q))
          if(model$aic < best_aic) {
            best_aic = model$aic
            best_model = model
          }
        }, error = function(e) {})
      }
    }
  }
  return(best_model)
}

# finding best ARIMA models for each series
admissions_arima = find_best_arima(admissions_ts)
cardio_arima = find_best_arima(cardio_ts)
resp_arima = find_best_arima(resp_ts)
hypo_arima = find_best_arima(hypo_ts)

# ============================================================================
# Step 6: Exponential Smoothing Models
# ============================================================================

# fitting ETS models for each series
admissions_ets = ets(admissions_ts)
cardio_ets = ets(cardio_ts)
resp_ets = ets(resp_ts)
hypo_ets = ets(hypo_ts)

# comparing model accuracies
cat("\n=== Model Comparison ===\n")
cat("\nAdmissions Models:")
print(accuracy(admissions_arima))
print(accuracy(admissions_ets))

cat("\nCardio-respiratory Models:")
print(accuracy(cardio_arima))
print(accuracy(cardio_ets))

cat("\nRespiratory Models:")
print(accuracy(resp_arima))
print(accuracy(resp_ets))

cat("\nHypoglycaemic Models:")
print(accuracy(hypo_arima))
print(accuracy(hypo_ets))

# ============================================================================
# Step 7: Model Evaluation using Train-Test Split
# ============================================================================

# function to evaluate model performance using train-test split
evaluate_model = function(ts_data, model_type = "ETS", test_size = 12) {
  time_index = time(ts_data)
  n = length(ts_data)
  
  train_end = time_index[n - test_size]
  test_start = time_index[n - test_size + 1]
  test_end = time_index[n]
  
  train = window(ts_data, end = train_end)
  test = window(ts_data, start = test_start, end = test_end)
  
  if(model_type == "ARIMA") {
    model = find_best_arima(train)
    if(!is.null(model)) {
      forecast_values = forecast(model, h = test_size)
      return(NULL)
    }
  } else {
    model = ets(train)
    forecast_values = forecast(model, h = test_size)
    mae = mean(abs(test - forecast_values$mean))
    rmse = sqrt(mean((test - forecast_values$mean)^2))
    mape = mean(abs((test - forecast_values$mean)/test)) * 100
    return(list(mae = mae, rmse = rmse, mape = mape))
  }
}

# evaluating models for each series
print("~ Model Evaluation Results (ETS Models Only) ~")

print("\nAdmissions:")
print("ETS Model:")
print(evaluate_model(admissions_ts, "ETS"))

print("\nCardio-respiratory:")
print("ETS Model:")
print(evaluate_model(cardio_ts, "ETS"))

print("\nRespiratory:")
print("ETS Model:")
print(evaluate_model(resp_ts, "ETS"))

print("\nHypoglycaemic:")
print("ETS Model:")
print(evaluate_model(hypo_ts, "ETS"))

# ============================================================================
# Step 8: Visualizing Model Performance
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

# create forecasts for each series
admissions_forecast = forecast(admissions_ets, h = 12)
cardio_forecast = forecast(cardio_ets, h = 12)
respiratory_forecast = forecast(resp_ets, h = 12)
hypoglycaemic_forecast = forecast(hypo_ets, h = 12)

# ix. time series decomposition plots (2x2 grid)
decomp_plots = list(
  admissions = admissions_decomp %>% 
    autoplot() +
    labs(title = "Decomposition of Emergency Room Admissions") +
    custom_theme,
  
  cardio = cardio_decomp %>% 
    autoplot() +
    labs(title = "Decomposition of Cardio-respiratory Events") +
    custom_theme,
  
  respiratory = resp_decomp %>% 
    autoplot() +
    labs(title = "Decomposition of Respiratory Events") +
    custom_theme,
  
  hypoglycaemic = hypo_decomp %>% 
    autoplot() +
    labs(title = "Decomposition of Hypoglycaemic Events") +
    custom_theme
)

# save combined decomposition plot
combined_decomp = (decomp_plots$admissions | decomp_plots$cardio) / 
  (decomp_plots$respiratory | decomp_plots$hypoglycaemic)
save_high_quality_plot(combined_decomp, "combined_decomposition.png", width = 16, height = 12)

# x. ETS model visualization (2x2 grid)
ets_plots = list(
  admissions = autoplot(admissions_ets) +
    labs(title = "ETS Components - Emergency Room Admissions",
         x = "Time",
         y = "Value") +
    custom_theme,
  
  cardio = autoplot(cardio_ets) +
    labs(title = "ETS Components - Cardio-respiratory Events",
         x = "Time",
         y = "Value") +
    custom_theme,
  
  respiratory = autoplot(resp_ets) +
    labs(title = "ETS Components - Respiratory Events",
         x = "Time",
         y = "Value") +
    custom_theme,
  
  hypoglycaemic = autoplot(hypo_ets) +
    labs(title = "ETS Components - Hypoglycaemic Events",
         x = "Time",
         y = "Value") +
    custom_theme
)

# save combined ETS plot
combined_ets = (ets_plots$admissions | ets_plots$cardio) / 
  (ets_plots$respiratory | ets_plots$hypoglycaemic)
save_high_quality_plot(combined_ets, "combined_ets_components.png", width = 16, height = 12)

# helper function for model diagnostics plots (residuals vs fitted, qq, hist, acf)
create_diagnostic_plots = function(model, title) {
  res = residuals(model)
  fitted_vals = fitted(model)
  
  # 1. residuals vs fitted
  p1 = ggplot(data.frame(Fitted=fitted_vals, Residuals=res), aes(x=Fitted, y=Residuals)) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed", color="red") +
    ggtitle(paste("Residuals vs Fitted -", title)) +
    theme_minimal()
  
  # 2. q-q plot
  qq_data = qqnorm(res, plot.it=FALSE)
  p2 = ggplot(data.frame(x=qq_data$x, y=qq_data$y), aes(x=x, y=y)) +
    geom_point() +
    geom_abline(slope=1, intercept=0, color="red") +
    ggtitle(paste("Q-Q Plot -", title)) +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    theme_minimal()
  
  # 3. histogram
  p3 = ggplot(data.frame(Residuals=res), aes(x=Residuals)) +
    geom_histogram(bins=30, fill="skyblue", color="black") +
    ggtitle(paste("Histogram of Residuals -", title)) +
    theme_minimal()
  
  # 4. acf plot
  acf_data = acf(res, plot=FALSE)
  p4 = ggplot(data.frame(Lag=acf_data$lag, ACF=acf_data$acf), aes(x=Lag, y=ACF)) +
    geom_hline(yintercept=0) +
    geom_segment(aes(xend=Lag, yend=0)) +
    ggtitle(paste("ACF of Residuals -", title)) +
    theme_minimal()
  
  return(list(p1, p2, p3, p4))
}

# xii-xv. model diagnostics plots (2x2 grid for each series)
diagnostic_plots = list(
  admissions = create_diagnostic_plots(admissions_ets, "Emergency Room Admissions"),
  cardio = create_diagnostic_plots(cardio_ets, "Cardio-respiratory Events"),
  respiratory = create_diagnostic_plots(resp_ets, "Respiratory Events"),
  hypoglycaemic = create_diagnostic_plots(hypo_ets, "Hypoglycaemic Events")
)

# save combined diagnostic plots for each series
for (name in names(diagnostic_plots)) {
  if (!is.null(diagnostic_plots[[name]])) {
    combined_diagnostic = (diagnostic_plots[[name]][[1]] | diagnostic_plots[[name]][[2]]) /
      (diagnostic_plots[[name]][[3]] | diagnostic_plots[[name]][[4]])
    save_high_quality_plot(
      combined_diagnostic,
      filename = paste0(name, "_diagnostics_combined.png"),
      width = 16,
      height = 12
    )
  }
}

# helper function for count model forecasts
create_count_forecast_plot = function(ts_data, title) {
  # prepare training and test sets
  n = length(ts_data)
  train = ts_data[1:(n-6)]
  test = ts_data[(n-5):n]
  time_train = 1:length(train)
  time_test = (length(train)+1):(length(train)+length(test))
  
  # fit poisson model
  pois_fit = glm(train ~ time_train, family=poisson)
  pois_pred = predict(pois_fit, newdata=data.frame(time_train=time_test), type="response")
  
  # fit negative binomial model
  nb_fit = tryCatch(glm.nb(train ~ time_train), error=function(e) NULL)
  nb_pred = if (!is.null(nb_fit)) {
    predict(nb_fit, newdata=data.frame(time_train=time_test), type="response")
  } else {
    rep(NA, length(test))
  }
  
  # prepare data for plotting
  plot_data = data.frame(
    Time = time_test,
    Actual = test,
    Poisson = pois_pred,
    NegBin = nb_pred
  ) %>%
    pivot_longer(-Time, names_to="Model", values_to="Value")
  
  # create plot
  ggplot(plot_data, aes(x=Time, y=Value, color=Model)) +
    geom_line(data=plot_data %>% filter(Model=="Actual"), 
              aes(group=1), color="black") +
    geom_point(size=3) +
    scale_color_manual(values=c("Actual"="black", 
                               "Poisson"="red", 
                               "NegBin"="blue")) +
    ggtitle(paste("Count Model Forecasts -", title)) +
    theme_minimal() +
    theme(legend.position="bottom")
}

# xvi. count model forecasts (2x2 grid)
count_forecast_plots = list(
  cardio = create_count_forecast_plot(cardio_ts, "Cardio-respiratory Events"),
  respiratory = create_count_forecast_plot(resp_ts, "Respiratory Events"),
  hypoglycaemic = create_count_forecast_plot(hypo_ts, "Hypoglycaemic Events")
)

# save combined count forecast plot
combined_count_forecast = (count_forecast_plots$cardio | count_forecast_plots$respiratory) /
  count_forecast_plots$hypoglycaemic
save_high_quality_plot(combined_count_forecast, "combined_count_forecast.png", width = 16, height = 12)

# ============================================================================
# Step 9: Saving Model Results
# ============================================================================

# create a list to store all model results
model_results = list(
  admissions = list(
    arima = admissions_arima,
    ets = admissions_ets,
    forecast = admissions_forecast
  ),
  cardio = list(
    arima = cardio_arima,
    ets = cardio_ets,
    forecast = cardio_forecast
  ),
  respiratory = list(
    arima = resp_arima,
    ets = resp_ets,
    forecast = respiratory_forecast
  ),
  hypoglycaemic = list(
    arima = hypo_arima,
    ets = hypo_ets,
    forecast = hypoglycaemic_forecast
  )
)

# save model results
saveRDS(model_results, 
        "../data/processed/time_series_model_results.rds")

# ~END OF R SCRIPT 02_time_series_modeling~