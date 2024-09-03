# asl.persistence model
# written by ASL


#### Step 0: load packages

library(tidyverse)
#remotes::install_github("LTREB-reservoirs/vera4castHelpers")
library(vera4castHelpers)
#remotes::install_github("eco4cast/read4cast")
library(read4cast)
source("./R/download_target.R")
library(forecast)

#### Step 1: Set model specifications
model_id <- "asl.persistence"
# Currently only set up for daily variables
# ARIMA does not work for binary variables
priority_daily <- read_csv("priority_daily.csv", show_col_types = FALSE) %>%
  dplyr::filter(!grepl("binary", `"official" targets name`))
model_variables <- priority_daily$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
target_depths = "target" #Depths to forecast
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
  
  # Format site data for model
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
      dplyr::filter(!is.na(depth_m)) %>%
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
    complete(datetime = full_seq(datetime, 1), site_id, depth_m)

  h = as.numeric(forecast_date - max(site_target$datetime)+horiz)
  
  # Fit RW model for each horizon individually
  fit_at_hi <- function(hi){
    data <- site_target %>%
      mutate(var_unnamed = get(!!var)) %>%
      tsibble::as_tsibble(index = datetime, key = "site_id") 
    
    if(nrow(data) < 10) {
      return(tibble(site_id = site,
                    .model = "RW",
                    datetime = max(site_target$datetime) + hi,
                    var_unnamed = NA,
                    mu = NA,
                    sigma = NA))
    }
    
    RW_model <- data %>%
      fabletools::model(RW = fable::RW(var_unnamed~ lag(hi)))
    
    forecast <- RW_model %>% 
      fabletools::forecast(h = hi) %>%
      filter(datetime == max(site_target$datetime) + hi)
    
    # extract parameters
    parameters <- distributional::parameters(forecast$var_unnamed)
    
    return(bind_cols(forecast, parameters))
  }
  
  fits <- purrr::pmap(list(1:h), fit_at_hi) |> 
    bind_rows()
  
  # make right format
  forecast <- fits |>
    pivot_longer(mu:sigma,
                 names_to = 'parameter',
                 values_to = 'prediction') |>
    mutate(model_id = model_id,
           family = 'normal',
           reference_datetime=forecast_date,
           variable = var,
           project_id = "vera4cast",
           depth_m = specific_depth,
           duration = "P1D") |>
    select(any_of(c("project_id", "model_id", "datetime", "reference_datetime",
                    "duration", "site_id", "family", "parameter", "depth_m",
                    "variable", "prediction"))) |>
    select(-any_of('.model')) |>
    filter(datetime > reference_datetime) |>
    ungroup() |>
    as_tibble()
  
  return(forecast)
}

