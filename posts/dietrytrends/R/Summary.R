#Summarize the data grouped by year
summarize_year <- function(data) {
  data |>
    group_by(Year) |>
    summarise(
      Animal_Protein = mean(Animal_Protein, na.rm = TRUE),
      Vegetal_Protein = mean(Vegetal_Protein, na.rm = TRUE),
      Fat = mean(Fat, na.rm = TRUE),
      Carbohydrates = mean(Carbohydrates, na.rm = TRUE)
    )
}



#Top 3 countries by variable for each decade
get_top3_by_decade <- function(data, variable) {
  # Creating Decade column
  data <- data |>
    mutate(Decade = floor(Year / 10) * 10)

  # Calculating the average per decade
  top_decade_data <- data |>
    group_by(Decade, Country) |>
    summarise(Average_Value = mean(.data[[variable]], na.rm = TRUE), .groups = "drop") |>
    arrange(desc(Average_Value))

  # Select the top 3 countries per decade
  top3_per_decade <- top_decade_data |>
    group_by(Decade) |>
    slice_max(order_by = Average_Value, n = 3)

  # Adding flag country codes
  top3_per_decade <- top3_per_decade |>
    mutate(flag_code = tolower(countrycode(Country, "country.name", "iso2c")))

  # Renaming column to match the variable name
  top3_per_decade <- top3_per_decade |>
    rename(!!variable := Average_Value)

  return(top3_per_decade)
}


#Total sum of macronutrients by year
calculate_sum <- function(data) {
  data |>
    group_by(Year) |>
    summarise(
      Animal_Protein = sum(Animal_Protein, na.rm = TRUE),
      Vegetal_Protein = sum(Vegetal_Protein, na.rm = TRUE),
      Fat = sum(Fat, na.rm = TRUE),
      Carbohydrates = sum(Carbohydrates, na.rm = TRUE)
    )

}

#Calculating proportions
prop_calc <- function(data) {
  data |>
    mutate(
      Total = Animal_Protein + Vegetal_Protein + Fat + Carbohydrates,
      Animal_Protein = Animal_Protein / Total,
      Vegetal_Protein = Vegetal_Protein / Total,
      Fat = Fat / Total,
      Carbohydrates = Carbohydrates / Total
    )

}
