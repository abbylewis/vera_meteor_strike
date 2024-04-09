# VERA meteor strike

Simple empirical forecasts for the VERA forecasting challenge

## Models currently set up:

-   asl.auto.arima
-   asl.ets
-   asl.tbats
-   asl.temp.lm

## TO DO:

1.  Working on met forecasts (load_met.R is not set up)
2.  asl.temp.lm model
3.  asl.temp.lm.all.sites model
4.  asl.met.lm model (linear model with multiple met vars)
5.  Figure out if we want to add auto_adam, nnetar

## To add new models

1.  Copy an existing model folder (I suggest `ARIMA` for a time series model or `temp_lm` for a model with meteorology)
2.  Update `forecast_model.R` to include your new model (most of the script should be able to stay the same)
3.  Update the the file path to source `rerun_forecasts.R` and `run_forecast.R`
4.  Run everything in your `forecast_model.R` file, then open `.R/generate_tg_forecast` and run the lines of this function individually to make sure everything works.
5.  If that worked, you should be pretty much set up! Push changes and create automation
