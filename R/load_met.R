source("./R/convert_to_vera_met_P1D.R")

#' load_met
#' 
#' Currently only set up for real-time forecasts (i.e., not re-analysis)
#'
#' @param forecast_date reference date for forecast generation
#' @param forecast_days days into the future to forecast
#' @param site site
#'
#' @return no return. Exports historical and future met
#'
load_met <- function(site,
                     forecast_date,
                     forecast_days = 35) {
  
  message("Loading met data for site ", site)
  
  #Stop if too many sites
  if(length(site) > 1) {
    stop("length(site) > 1. Only one site can be loaded at a time.")
  }
  
  #Specify variables
  variables <- c("relativehumidity_2m", 
                 "precipitation", 
                 "windspeed_10m", 
                 "temperature_2m")
  
  variables_renamed <- c("RH_percent_mean", 
                         "Rain_mm_sum", 
                         "WindSpeed_ms_mean", 
                         "AirTemp_C_mean")
  
  #Load sites
  site_list <- read_csv("https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv", 
                        show_col_types = FALSE)
  lat <- site_list$latitude[site_list$site_id == site]
  long <-  site_list$longitude[site_list$site_id == site]
  if(!site %in% site_list$site_id){
    stop("Site not found in site list")
  }
  
  #Weather predictions
  message("Loading weather predictions")
  weather_pred <- RopenMeteo::get_ensemble_forecast(
    latitude = lat,
    longitude = long,
    forecast_days = forecast_days, # days into the future
    past_days = 92, # past days that can be used for model fitting
    model = "gfs_seamless", # this is the NOAA gefs ensemble model
    variables = variables) |> 
    # function to convert to EFI standard
    RopenMeteo::convert_to_efi_standard() |>
    # rename variables to match met station
    convert_to_vera_met_P1D() %>%
    mutate(site_id = site)
  
  message("Loading historical weather")
  weather_hist <- RopenMeteo::get_historical_weather(
    latitude = lat,
    longitude = long,
    start_date = as.Date("2010-01-01"),
    end_date = as.Date(Sys.Date()),
    variables = variables) |> 
    # function to convert to EFI standard
    RopenMeteo::convert_to_efi_standard() |>
    # rename variables to match met station
    convert_to_vera_met_P1D() %>%
    mutate(site_id = site)
  
  message("Adjusting forecasts to match historical data")
  comparison_mod <- weather_hist %>%
    rename(hist_pred = prediction) %>%
    filter(!is.na(hist_pred)) %>%
    left_join(weather_pred, by = c("datetime", "variable")) %>%
    filter(!is.na(prediction)) %>%
    mutate(datetime = as.Date(datetime)) %>%
    group_by(datetime, variable) %>%
    summarize(future_sd = sd(prediction),
              future = mean(prediction),
              hist = unique(hist_pred),
              .groups = "drop")
  
  weather_pred_adjust <- weather_pred
  for(var in variables_renamed){
    lm <- lm(future ~ hist, data = comparison_mod %>% filter(variable == var))
    weather_pred_adjust <- weather_pred_adjust %>%
      mutate(prediction = ifelse(variable == var, 
                             prediction - lm$coefficients[1] + (1-lm$coefficients[2]) * prediction, 
                             prediction))
  }
  
  #Filter to the future
  weather_pred_export <- weather_pred_adjust %>%
    filter(datetime >= forecast_date) %>%
    pivot_wider(names_from = variable, values_from = prediction)
  
  if(!dir.exists("met_downloads")){
    dir.create("met_downloads")
  }
  write.csv(weather_pred_export,
            paste0("./met_downloads/future_daily_",site,"_",
                   forecast_date,".csv"),
            row.names = F)
  
  write.csv(weather_hist %>%
              pivot_wider(names_from = variable, values_from = prediction),
            paste0("./met_downloads/past_daily_",site,"_",
                   forecast_date,".csv"),
            row.names = F)
  return()
}
