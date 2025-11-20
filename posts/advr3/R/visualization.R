#Plot line charts for global trends in macronutrient consumption over the years
plot_global_trends <- function(data) {
  ggplot(data, aes(x = Year)) +
    geom_line(aes(y = Animal_Protein, color = "Animal Protein"), linewidth = 1) +
    geom_line(aes(y = Vegetal_Protein, color = "Vegetal Protein"), linewidth = 1) +
    geom_line(aes(y = Fat, color = "Fat"), linewidth = 1) +
    geom_line(aes(y = Carbohydrates, color = "Carbohydrates"), linewidth = 1) +
    labs(
      title = "Global Trends in Macronutrient Consumption 1961 - 2022",
      x = "Year",
      y = "Calories per Person per Day (kcal)",
      color = "Macronutrients"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Animal Protein" = "blue",
                                  "Vegetal Protein" = "green",
                                  "Fat" = "orange",
                                  "Carbohydrates" = "red"))
}




# Plot top 3 countries for each macronutrient by decade
plot_top3_by_decade <- function(top3_per_decade, variable) {
  ggplot(top3_per_decade, aes(x = reorder(Country, .data[[variable]]),
                              y = .data[[variable]], fill = .data[[variable]])) +
    geom_bar(stat = "identity", width = 0.9) +
    geom_flag(aes(y = -15, country = flag_code), size = 5) +
    geom_text(aes(label = round(.data[[variable]], 1)),
              hjust = 1.1, size = 3.5, color = "white") +
    coord_flip() +
    labs(
      title = paste("Top 3 Countries for", gsub("_", " ", variable), "by Decade"),
      x = "Country",
      y = "Calories per Person per Day (kcal)"
    ) +
    facet_wrap(~Decade, scales = "free_y", nrow = 4) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Calories (kcal)") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 10, margin = margin(r = 20)),
      #plot.margin = margin(, 1, 1, 1, "cm"),
      panel.spacing = unit(1.5, "lines")
    )
}





#Plot proportions of macronutrients
proportion_plot <- function(data) {
  ggplot(data, aes(x = Year, y = Proportion, fill = Macronutrient)) +
    geom_area(alpha = 0.7, linewidth = 0.5, colour = "white") +
    scale_fill_manual(values = c("blue", "green", "orange", "red")) +
    labs(
      title = "Macronutrient Share Over Time (1961 - 2022)",
      x = "Year",
      y = "Proportion of Global Caloric Intake",
      fill = "Macronutrient Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
}
