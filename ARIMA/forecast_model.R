# asl.auto.arima model
# written by ASL


# NOTES: 
# Need to set this up to iterate through hourly and daily variables
# Need to incorporate depths
# Need to deal with binary variables (bernouli distribution)


#### Step 0: load packages

library(tidyverse)
#remotes::install_github("LTREB-reservoirs/vera4castHelpers")
library(vera4castHelpers)
source("download_target.R")
library(forecast)

model_id <- "asl.auto.arima"
priority_daily <- read_csv("priority_daily.csv", show_col_types = FALSE)
model_variables <- priority_daily$`"official" targets name`

#### Define the forecast model for a site
forecast_model <- function(site,
                           var,
                           noaa_past_mean = NULL,
                           noaa_future_daily = NULL,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date) {
  
  message(paste0("Running site: ", site))
  
  # Format site data for arima model
  site_target_raw <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date) |> 
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
  if(sum(site_target[var] < 0, na.rm=T) > 0){ #If there are any negative values, don't consider transformation
    fit = auto.arima(site_target[var])
  } else {
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
                        depth_m = NA,
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

