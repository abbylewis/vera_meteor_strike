source("./R/run_all_vars.R")

generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_variables = model_variables,
                                 model_id = model_id,
                                 all_sites = F, #Whether the model is /trained/ across all sites
                                 sites = c("bvre", "fcre", "tubr"), #Sites to forecast
                                 noaa = T,
                                 depth = 'target') {
  
  ### Step 1: Set forecast specifications
  
  horiz = 35
  step = 1
  vars = c("Turbidity_FNU_mean")
  
  if (depth == 'target') {
    # only generates forecasts for target depths
    target_depths <- c(1.5, 1.6, NA)
  } else {
    target_depths <- depth
  }
  
  ### Step 2: Get NOAA driver data (if needed)
  if(noaa){ #Some forecasts do not use any noaa driver data--> in that case skip download
    forecast_date <- as.Date(forecast_date)
    load_met(forecast_date) #This function loads meteorology if and only if it does not already exist
    noaa_future_daily <- read.csv(paste0("./Generate_forecasts/noaa_downloads/noaa_future_daily_",forecast_date,".csv")) |> 
      mutate(datetime = lubridate::as_date(datetime))
    
    # Load stage3 data. 
    noaa_past_mean <- read.csv(paste0("./Generate_forecasts/noaa_downloads/noaa_past_mean_",forecast_date,".csv")) |> 
      mutate(datetime = lubridate::as_date(datetime))
  } else {
    forecast_date <- as.Date(forecast_date)
    noaa_future_daily <- NULL
    noaa_past_mean <- NULL
  }
  
  ### Step 3: Download latest target data
  target = download_target()
  
  ### Step 4: forecast!
  
  ## Test with a single site first!
  #forecast <- map_dfr(vars,
  #                    run_all_vars,
  #                    sites = sites[1],
  #                    forecast_model = forecast_model,
  #                    noaa_past_mean = noaa_past_mean,
  #                    noaa_future_daily = noaa_future_daily,
  #                    target = target,
  #                    horiz = horiz,
  #                    step = step,
  #                    theme = theme,
  #                    forecast_date = forecast_date)
  
  #Visualize the ensemble predictions -- what do you think?
  #forecast |> 
  #  ggplot(aes(x = datetime, y = prediction, color = parameter)) +
  #  geom_line(alpha=0.3) +
  #  facet_wrap(~variable, scales = "free")
  
  # Run all sites -- may be slow!
  if(all_sites == F) {
    forecast <- map_dfr(vars,
                        run_all_vars,
                        sites = sites,
                        forecast_model = forecast_model,
                        noaa_past_mean = noaa_past_mean,
                        noaa_future_daily = noaa_future_daily,
                        target = target,
                        horiz = horiz,
                        step = step,
                        theme = theme,
                        forecast_date = forecast_date)
  } else {
    forecast <- map_dfr(vars,
                        forecast_model,
                        sites = sites,
                        noaa_past_mean = noaa_past_mean,
                        noaa_future_daily = noaa_future_daily,
                        target = target,
                        horiz = horiz,
                        step = step,
                        theme = theme,
                        forecast_date = forecast_date)
  }
  
  ### Step 5: Format and submit
  
  # Write forecast to disk
  forecast_file <- paste0("daily-", forecast_date, "-", model_id, ".csv.gz")
  write_csv(forecast, forecast_file)
  
  # Submit
  vera4castHelpers::submit(forecast_file = forecast_file) #first_submission = FALSE
}
