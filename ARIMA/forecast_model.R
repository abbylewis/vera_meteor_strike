# asl.auto.arima model
# written by ASL


#### Step 0: load packages

library(tidyverse)
#remotes::install_github("LTREB-reservoirs/vera4castHelpers")
#remotes::install_github("eco4cast/read4cast")
library(vera4castHelpers)
library(read4cast)
source("download_target.R")
library(forecast)

#### Step 1: Set model specifications
model_id <- "asl.auto.arima"
# Currently only set up for daily variables
# ARIMA does not work for binary variables
priority_daily <- read_csv("priority_daily.csv", show_col_types = FALSE) %>%
  dplyr::filter(!grepl("binary", `"official" targets name`))
model_variables <- priority_daily$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data


#### Step 2: Define the forecast model
forecast_model <- function(specific_depth,
                           site,
                           var,
                           noaa_past_mean = NULL,
                           noaa_future_daily = NULL,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date) {
  
  message(paste0("Running depth: ", specific_depth))
  
  # Format site data for arima model
  site_target_trimmed <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation, depth_m) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date) 
  
  # Isolate target depth
  if(is.na(specific_depth)){
    site_target_raw = site_target_trimmed %>% filter(is.na(depth_m))
  } else {
    site_target_raw = site_target_trimmed |>
      dplyr::filter(depth_m == specific_depth)
  }
  
  # Format
  site_target_raw <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  if(!var %in% names(site_target_raw) || sum(!is.na(site_target_raw[var])) == 0){
    message(paste0("No target observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }

  site_target = site_target_raw |>
    complete(datetime = full_seq(datetime, 1), site_id)

  h = as.numeric(forecast_date - max(site_target$datetime)+horiz)

  # Fit arima model
  # If there are any non-positive values, don't consider transformation
  if(sum(site_target[var] < 0, na.rm=T) > 0){ 
    fit = auto.arima(site_target[var])
  } else {
    # Otherwise, use lambda = "auto"
    ts <- site_target[var]
    ts[ts == 0] <- min(ts[!is.na(ts) & ts > 0], na.rm = T)/2 #Deal with 0s before transformation
    message("Model run with transformation. Min value is ", min(ts, na.rm = T))
    fit = auto.arima(site_target[var], lambda = "auto")
  }
  
  # use the model to forecast target variable
  forecast_raw <- as.data.frame(forecast(fit, h = h, level=0.68))%>% #One SD
    mutate(sigma = `Hi 68`-`Point Forecast`)
  
  forecast = data.frame(project_id = "vera4cast",
                        model_id = model_id,
                        datetime = (1:h)*step+max(site_target$datetime),
                        reference_datetime = forecast_date,
                        duration = "P1D",
                        depth_m = specific_depth,
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = as.numeric(forecast_raw$`Point Forecast`),
                        sigma = as.numeric(forecast_raw$sigma)
                        )%>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration, depth_m,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

