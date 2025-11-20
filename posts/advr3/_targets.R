library(targets)
library(tarchetypes)
library(lubridate)
library(ggflags)
library(countrycode)
library(forecast)
library(quarto)
library(dplyr)
library(visNetwork)
library(remotes)
library(htmlwidgets)


# Define custom functions and other global objects.
tar_source("R")

tar_option_set(
  packages = c("htmlwidgets","remotes","visNetwork","dplyr", "ggplot2", "tidyverse", "lubridate", "ggflags", "countrycode", "forecast", "quarto")
)



# Define the pipeline

 list(

  #Load Data
   tar_target(
     raw_data,
     read.csv("daily-caloric-supply-derived-from-carbohydrates-protein-and-fat.csv")
   ),

  #Rename Columns
  tar_target(
    renamed_data,
    rename_columns(raw_data)
  ),

  #Trim Whitespaces and covert empty strings to NA
  tar_target(
    trimmed_data,
    trim_and_na(renamed_data)
  ),

  #Convert Data Types
  tar_target(
    converted_data,
    convert_data_types(trimmed_data)
  ),

  #Check for missing values
  tar_target(
    missing_values,
    colSums(is.na(converted_data))
  ),

  # Remove Aggregated Regions
  tar_target(
    data_cleaned,
    remove_aggregated_regions(converted_data)
  ),

  #Check for missing values after cleaning
  tar_target(
    missing_post_clean,
    colSums(is.na(data_cleaned))
  ),

  #Check for duplicates
  tar_target(duplicate_check,
             sum(duplicated(data_cleaned))),

  #Summarize the data grouped by year
  tar_target(
    summary_data,
    summarize_year(data_cleaned)
  ),

  #Plot line charts for global trends in macronutrient consumption
  tar_target(
    plot_trends,
    plot_global_trends(summary_data)
  ),


  #Top 3 countries for Animal Protein for each decade
  tar_target(
    top3_animal_protein,
    get_top3_by_decade(data_cleaned, "Animal_Protein")
  ),

  #Plot top 3 countries for Animal Protein for each decade
  tar_target(
    plot_top3_animal_protein,
    plot_top3_by_decade(top3_animal_protein, "Animal_Protein")
  ),

  #Calcutate sum column
  tar_target(
    sum_data,
    calculate_sum(data_cleaned)
  ),

  #Calculating Proportions
  tar_target(calorie_prop,
    prop_calc(sum_data)
  ),

 #Format data for proportion plot
 tar_target(long_prop,
            calorie_prop |>
             select(Year, Animal_Protein, Vegetal_Protein, Fat, Carbohydrates) |>
             pivot_longer(cols = -Year, names_to = "Macronutrient", values_to = "Proportion")
            ),

  #Plot proportions of macronutrients
  tar_target(
    plot_proportions,
    proportion_plot(long_prop)
  ),

 #Creating time series object
 tar_target(
   fat_ts,
   ts(summary_data$Fat, start = 1961, end = 2022, frequency = 1)
 ),

 # Summary Time Series
 tar_target(
   summary_fat_ts,
   summary(fat_ts)
 ),

 #Plot Time Series
 tar_target(
   plot_fat_ts,
   {
     p <- autoplot(fat_ts) +
       labs(
         title = "Global Fat Consumption (1961 - 2022)",
         y = "Calories per Person per Day (kcal)"
       ) +
       theme_minimal()
     p
   }
 ),


 #Fit ARIMA Model
 tar_target(
   fat_arima,
   auto.arima(fat_ts)
 ),

 #Summary of ARIMA Model
 tar_target(
   fat_arima_summary,
   summary(fat_arima)
 ),

 # ARIMA Forecasting (Next 10 Years)
 tar_target(
   fat_forecast,
   forecast(fat_arima, h = 10)
 ),

 # Plot Forecast
 tar_target(
   plot_fat_forecast,
   autoplot(fat_forecast) +
     labs(
       title = "Forecasted Global Fat Consumption (Next 10 Years)",
       x = "Year",
       y = "Calories per Person per Day (kcal)"
     ) +
     theme_minimal()
 ),


 # Add ARIMA Predictions to Dataframe
 tar_target(
   global_trends_predicted,
   summary_data |>
     mutate(Predicted_Fat = as.numeric(fitted(fat_arima)))
 ),



 #Plot Actual vs Predicted
 tar_target(
   plot_actual_vs_predicted,
   ggplot(global_trends_predicted, aes(x = Year)) +
     geom_line(aes(y = Fat, color = "Actual"), linewidth = 1) +
     geom_line(aes(y = Predicted_Fat, color = "Predicted"), linewidth = 1, linetype = "dashed") +
     labs(
       title = "Actual vs Predicted Global Fat Consumption",
       x = "Year",
       y = "Calories per Person per Day (kcal)",
       color = "Legend"
     ) +
     theme_minimal()
 ),

 #Residual Diagnostics
 tar_target(
   residual_diagnostics_test,
   {
     test_result <- checkresiduals(fat_arima, plot = FALSE)
     test_result
   }
 ),

 #Residual Diagnostics Plot
 tar_target(
   residual_diagnostics_plot,
   {
     png("residuals_diagnostics_plot.png")
     checkresiduals(fat_arima, plot = TRUE)
     dev.off()
     "residuals_diagnostics_plot.png"
   },
   format = "file"
 ),

# Create a report using Quarto
  tar_quarto(report, "report.qmd")


)
