# hospital data time series helper functions only

#' fitting ETS model and generating forecast
#' @param ts_data time series data
#' @param h forecast horizon
#' @return list containing fitted model and forecast
fit_ets_forecast = function(ts_data, h = 12) {
  # fitting ETS model
  model = ets(ts_data)
  
  # generating forecast
  forecast_result = forecast::forecast(model, h = h)
  
  return(list(model = model, forecast = forecast_result))
}

#' calculating forecast error metrics
#' @param actual actual values
#' @param forecast forecast values
#' @return list containing error metrics
calculate_forecast_metrics = function(actual, forecast) {
  # calculating metrics
  mae = mean(abs(actual - forecast), na.rm = TRUE)
  rmse = sqrt(mean((actual - forecast)^2, na.rm = TRUE))
  mape = mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
  
  return(list(mae = mae, rmse = rmse, mape = mape))
}

#' fitting count models (Poisson and Negative Binomial)
#' @param ts_data time series data
#' @param h forecast horizon
#' @return list containing fitted models and forecasts
fit_count_models = function(ts_data, h = 12) {
  # fitting Poisson model
  poisson_model = tscount::tsglm(ts_data, model = list(past_obs = 1, past_mean = 1),
                                 distr = "poisson")
  poisson_forecast = predict(poisson_model, n.ahead = h)
  
  # fitting Negative Binomial model
  nb_model = tscount::tsglm(ts_data, model = list(past_obs = 1, past_mean = 1),
                            distr = "nbinom")
  nb_forecast = predict(nb_model, n.ahead = h)
  
  return(list(
    poisson = list(model = poisson_model, forecast = poisson_forecast),
    negbin = list(model = nb_model, forecast = nb_forecast)
  ))
}