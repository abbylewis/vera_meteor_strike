# asl.temp.lm model
# written by ASL


#### Step 0: load packages

#library(tidyverse)
#library(neon4cast)
#library(lubridate)
#library(rMR)
#library(glue)
#library(tsibble)
#library(fable)
#library(arrow)
#source("download_target.R")
#source("./R/load_met.R")
#source("./R/generate_tg_forecast.R")
#source("./R/run_all_vars.R")

library(tidyverse)
#remotes::install_github("LTREB-reservoirs/vera4castHelpers")
#remotes::install_github("eco4cast/read4cast")
library(vera4castHelpers)
library(read4cast)
source("download_target.R")
#library(forecast)

#### Step 1: Set model specifications
model_id <- "asl.temp.lm"
# Currently only set up for daily variables, and not binary variables
priority_daily <- read_csv("priority_daily.csv", show_col_types = FALSE) %>%
  dplyr::filter(!grepl("binary", `"official" targets name`))
model_variables <- priority_daily$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
target_depths = "target" #Depths to forecast
noaa = T #Whether the model requires NOAA data

#### Define the forecast model for a site
forecast_model <- function(specific_depth,
                           site,
                           var,
                           noaa_past_mean,
                           noaa_future_daily,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date) {
  
  message(paste0("Running depth: ", specific_depth))

  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% c(target_variable), 
                  site_id == site,
                  datetime < forecast_date) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id == site), 
                     by = c("datetime", "site_id"))
  
  if(!target_variable%in%names(site_target)){
    message(paste0("No target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else if(sum(!is.na(site_target$air_temperature)&!is.na(site_target[target_variable]))==0){
    message(paste0("No historical air temp data that corresponds with target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    # Fit linear model based on past data: target = m * air temp + b
    fit <- lm(get(target_variable) ~ air_temperature, data = site_target) #THIS IS THE MODEL
    
    #  Get 30-day predicted temp ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id==site)
    
    # use the linear model (predict.lm) to forecast target variable for each ensemble member
    forecast <- 
      noaa_future |> 
      mutate(site_id = site,
             prediction = predict(fit, tibble(air_temperature)), #THIS IS THE FORECAST STEP
             variable = target_variable)
    
    # Format results to EFI standard
    forecast <- forecast |>
      mutate(reference_datetime = forecast_date,
             family = "ensemble",
             model_id = model_id) |>
      select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
  }
}
