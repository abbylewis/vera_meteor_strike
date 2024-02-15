#Quick function to repeat for all depths
run_all_depths = function(sites,
                        forecast_model,
                        noaa_past_mean,
                        noaa_future_daily,
                        target,
                        horiz,
                        step,
                        theme,
                        var,
                        forecast_date,
                        target_depths) {
  
  message(paste0("Running site: ", sites))
  forecast <- map_dfr(.x = target_depths,
                      .f = forecast_model,
                      site = sites,
                      noaa_past_mean = noaa_past_mean,
                      noaa_future_daily = noaa_future_daily,
                      var = var,
                      target = target,
                      horiz = horiz,
                      step = step,
                      theme = theme,
                      forecast_date = forecast_date)
  
}
