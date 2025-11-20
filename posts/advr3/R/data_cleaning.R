#Rename Columns of data
rename_columns <- function(data) {
  data |>
    rename(
      Country = Entity,
      Code = Code,
      Year = Year,
      Animal_Protein = `Daily.calorie.supply.per.person.that.comes.from.animal.protein`,
      Vegetal_Protein = `Daily.calorie.supply.per.person.that.comes.from.vegetal.protein`,
      Fat = `Daily.calorie.supply.per.person.from.fat`,
      Carbohydrates = `Daily.calorie.supply.per.person.from.carbohydrates`
    )
}



# Function to trim whitespaces and convert empty strings to NA
trim_and_na <- function(data) {
  data |>
    mutate(across(everything(), ~na_if(trimws(.), "")))
}


#Convert Data Types
convert_data_types <- function(data) {
  data |>
    mutate(
      Year = as.integer(Year),
      Animal_Protein = as.double(Animal_Protein),
      Vegetal_Protein = as.double(Vegetal_Protein),
      Fat = as.double(Fat),
      Carbohydrates = as.double(Carbohydrates)
    )
}

#Remove Aggregated Regions (No ISO Codes)
remove_aggregated_regions <- function(data) {
  missing_codes <- data |>
    filter(is.na(Code)) |>
    distinct(Country) |>
    pull(Country)

  data |>
    filter(!Country %in% missing_codes)
}

