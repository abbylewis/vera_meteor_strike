library(devtools)
SourceURL <- "https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/drivers/download_ensemble_forecast.R"
source_url(SourceURL)

load_met <- function(forecast_date, sites) {
  
  if(file.exists(paste0("./noaa_downloads/noaa_future_daily_",forecast_date,".csv"))){
    message(paste0("Met data has already been saved for reference date: ",forecast_date))
    return()
  } else {
    message(paste0("Forecasts not found, loading met data for: ",forecast_date))
  }
  
  #Date
  noaa_date <- forecast_date - lubridate::days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet
  
  variables <- c('air_temperature',
                 "surface_downwelling_longwave_flux_in_air",
                 "surface_downwelling_shortwave_flux_in_air",
                 "precipitation_flux",
                 "air_pressure",
                 "relative_humidity",
                 "air_temperature",
                 "northward_wind",
                 "eastward_wind")
  
  # Load stage 2 data
  use_s3 <- arrow::s3_bucket("bio230121-bucket01/flare/drivers/met/ensemble_forecast",
                         endpoint_override = "renc.osn.xsede.org",
                         access_key = Sys.getenv("OSN_KEY"),
                         secret_key = Sys.getenv("OSN_SECRET"))
  
  noaa_future <- arrow::open_dataset(use_s3) |>
    dplyr::collect() |>
    dplyr::filter(site_id %in% sites,
                  datetime >= forecast_date,
                  parameter <= 31,
                  variable %in% variables) #It would be more efficient to filter before collecting, but this is not running on my M1 mac
  
  # Format met forecasts
  noaa_future_daily <- noaa_future |> 
    mutate(datetime = lubridate::as_date(datetime)) |> 
    # mean daily forecasts at each site per ensemble
    group_by(datetime, site_id, parameter, variable) |> 
    summarize(prediction = mean(prediction)) |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    # convert to Celsius
    mutate(air_temperature = air_temperature - 273.15) |> 
    select(datetime, site_id, all_of(variables), parameter)
  write.csv(noaa_future_daily,paste0("./Generate_forecasts/noaa_downloads/noaa_future_daily_",forecast_date,".csv"),row.names = F) #Save the past meteorology
  
  # Load stage3 data. 
  #The bucket is somewhat differently organized here, necessitating a different structure. 
  #This will take a LONG TIME to load, especially if we are running all sites (I estimate 10 min on my computer)
  load_stage3 <- function(site,endpoint,variables){
    message('laod met for ', site)
    use_bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage3/parquet/", site)
    use_s3 <- arrow::s3_bucket(use_bucket, endpoint_override = endpoint, anonymous = TRUE)
    parquet_file <- arrow::open_dataset(use_s3) |>
      dplyr::collect() |>
      dplyr::filter(parameter <= 31,
                    datetime >= lubridate::ymd('2017-01-01'),
                    variable %in% variables)|> #It would be more efficient to filter before collecting, but this is not running on my M1 mac
      na.omit() |> 
      mutate(datetime = lubridate::as_date(datetime)) |> 
      group_by(datetime, site_id, variable) |> 
      summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
      pivot_wider(names_from = variable, values_from = prediction) |> 
      # convert air temp to C
      mutate(air_temperature = air_temperature - 273.15)
  }
  
  noaa_past_mean <- map_dfr(all_sites, load_stage3,endpoint,variables)
  write.csv(noaa_past_mean,paste0("./Generate_forecasts/noaa_downloads/noaa_past_mean_",forecast_date,".csv"),row.names = F) #Save the past meteorology
  return()
}
