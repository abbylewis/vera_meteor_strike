# asl.climate.window model
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
model_id <- "asl.climate.window"
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
window = 10


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
  
  days <- data.frame(doy = seq(1,366, 1), site_id = site)
  
  # calculate the mean and standard deviation for each doy
  target_clim <- site_target %>%
    mutate(doy = yday(datetime)) %>%
    #Wrap around doys for rolling mean
    mutate(dup = ifelse(doy <= window/2, 2, 1)) %>%
    uncount(dup, .id = "dup") %>%
    mutate(doy = ifelse(dup>1, doy + 365, doy)) %>%
    mutate(dup = ifelse(doy >= 365- window/2, 2, 1)) %>%
    uncount(dup, .id = "dup") %>%
    mutate(doy = ifelse(dup>1, doy - 365, doy)) %>%
    arrange(doy) %>%
    #Calculate rolling mean
    mutate(clim_mean = slider::slide_index_dbl(.x = get(!!var), 
                                               .i = doy, 
                                               .f = mean, 
                                               na.rm = T,
                                               .before = window/2,
                                               .after = window/2),
           clim_sd = slider::slide_index_dbl(.x = get(!!var), 
                                             .i = doy, 
                                             .f = sd, 
                                             na.rm = T,
                                             .before = window/2,
                                             .after = window/2)) %>%
    filter(doy >= 1 & doy <= 366) %>%
    select(-any_of(c("dup", var))) %>%
    full_join(days, by = c("doy", "site_id")) %>%
    select(-datetime) %>%
    distinct()
  
  # what dates do we want a forecast of?
  forecast_dates <- (1:h)*step+max(site_target$datetime)
  forecast_doy <- as.integer(yday(forecast_dates))
  
  # put in a table
  forecast_dates_df <- tibble(datetime = forecast_dates,
                              doy = forecast_doy,
                              depth_m = specific_depth)
  
  forecast <- target_clim %>%
    mutate(doy = as.integer(doy)) %>%
    dplyr::filter(doy %in% forecast_doy) %>%
    full_join(forecast_dates_df) %>%
    arrange(site_id, datetime)
  
  if(sum(!is.na(forecast$clim_mean)) == 0 | sum(!is.na(forecast$clim_sd)) == 0){
    message(paste0("Insufficient historical observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }
  
  # Interpolate
  combined <- forecast %>%
    select(datetime, site_id, clim_mean, clim_sd) %>%
    rename(mean = clim_mean,
           sd = clim_sd) %>%
    mutate(mu = imputeTS::na_interpolation(x = mean),
           sigma = median(sd, na.rm = TRUE))
  
  forecast = data.frame(project_id = "vera4cast",
                        model_id = model_id,
                        datetime = (1:h)*step+max(site_target$datetime),
                        reference_datetime = forecast_date,
                        duration = "P1D",
                        depth_m = specific_depth,
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = as.numeric(combined$mu),
                        sigma = as.numeric(combined$sigma)
                        )%>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration, depth_m,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

