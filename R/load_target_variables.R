#Load from google sheets
#(not run automatically, must be updated manually)
library(googlesheets4)
target_vars <- read_sheet('https://docs.google.com/spreadsheets/d/1fOWo6zlcWA8F6PmRS9AD6n1pf-dTWSsmGKNpaX3yHNE/edit#gid=0')
priority_daily <- target_vars |>
  dplyr::filter(duration == "P1D") |>
  dplyr::filter(`priority target` == T)

write.csv(priority_daily, "priority_daily.csv", row.names = F)
