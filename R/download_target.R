
download_target <- function(type = "res"){  
  if(type == "res"){
    url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
  } else if(type == "met"){
    url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"
  } else if(type == "inf"){
    url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"
  }
  target <- read_csv(url, show_col_types = FALSE, lazy = FALSE, progress = FALSE)
}
