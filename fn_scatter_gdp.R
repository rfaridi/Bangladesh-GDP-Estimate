
plot_macro_relationships <- function(data, start_year, end_year = NULL) {
  
  # data <- bgd_panel
  # start_year <- 2016
  # end_year <- NULL
  
  if (is.null(end_year)) {
    end_year <- max(data$year, na.rm = TRUE)
  }
  
  x_vars <- c("govt_exp_growth", "real_tax_growth", "elec_consumption_growth", 
              "energy_consumption_growth", "real_credit_growth", "export_vol_growth", 
              "import_vol_growth", "gcf_growth")
  
  df_filtered <- data %>%
    filter(year >= start_year & year <= end_year) %>%
    select(year, gdp_growth, all_of(x_vars))
  

  df_long <- df_filtered %>%
    pivot_longer(
      cols = all_of(x_vars), 
      names_to = "indicator", 
      values_to = "value"
    ) %>%
    drop_na(gdp_growth, value)
  
  # 5. Calculate correlation coefficients for each variable to display on the plots
  correlations <- df_long %>%
    group_by(indicator) %>%
    summarize(
      cor_val = cor(value, gdp_growth, use = "complete.obs"),
      .groups = "drop"
    )
  
  # 6. Create clean, readable titles for each facet panel
  facet_labels <- c(
    "govt_exp_growth" = "Govt Expenditure Growth",
    "real_tax_growth" = "Real Direct Tax Growth",
    "elec_consumption_growth" = "Electricity Consumption Growth",
    "energy_consumption_growth" = "Energy Consumption Growth",
    "real_credit_growth" = "Real Bank Credit Growth",
    "export_vol_growth" = "Real Exports Growth",
    "import_vol_growth" = "Real Imports Growth",
    "gcf_growth" = "GCF Growth"
  )
  
  # 7. Generate the plot
  p <- ggplot(df_long, aes(x = value, y = gdp_growth)) +
    # Scatter points (Light Blue)
    geom_point(color = "#8DA0CB", alpha = 0.8, size = 2.5) +
    
    # Regression Line (Dark Blue)
    geom_smooth(method = "lm", se = FALSE, color = "#2C5B82", linewidth = 1) +
    
    # Add correlation text to the bottom right of every plot
    geom_text(
      data = correlations,
      # Inf/-Inf automatically pushes text to the extreme corners of the graph
      aes(x = Inf, y = -Inf, label = sprintf("Correlation = %.2f", cor_val)),
      hjust = 1.1, vjust = -1.5, color = "#8DA0CB", fontface = "bold", size = 4
    ) +
    
    # Create the grid of plots, allowing the X-axis to scale independently for each metric
    facet_wrap(~ indicator, scales = "free_x", ncol = 2, 
               labeller = labeller(indicator = facet_labels)) +
    
    # Labels and Titles
    labs(
      title = paste("GDP Growth vs. Macroeconomic Indicators (", start_year, "-", end_year, ")", sep = ""),
      y = "Real GDP Growth",
      x = "Indicator Growth"
    ) +
    
    # Styling to match the clean aesthetic of your second image
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 11, color = "#2C5B82", hjust = 0.5),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.background = element_rect(fill = "white", color = "grey80"),
      plot.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(face = "bold", size = 11)
    )
  
  return(p)
}