source("./Generate_forecasts/temp_lm/forecast_model.R")

tryCatch({
  generate_tg_forecast(forecast_date = Sys.Date(),
                       forecast_model = forecast_model,
                       model_variables = model_variables,
                       model_id = model_id,
                       all_sites = all_sites,
                       sites = sites,
                       target_depths = target_depths,
                       noaa = noaa)
}, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})

#For troubleshooting
#forecast_date = Sys.Date()
