#library(tidyverse)

#data <- bgd_panel
#periods <- c("1980-1995","1996-2015","2016-2024")

plot_macro_periods <- function(data, x_vars, periods) {
  
  # 2. Helper function to assign a year to one of the input periods
  get_period <- function(yr, valid_periods) {
    for (p in valid_periods) {
      bounds <- as.numeric(str_split(p, "-")[[1]])
      if (!is.na(yr) && yr >= bounds[1] && yr <= bounds[2]) {
        return(p)
      }
    }
    return(NA_character_)
  }
  
  # 3. Process the data
  df_filtered <- data %>%
    # Assign the period to each row
    mutate(period = sapply(year, get_period, valid_periods = periods)) %>%
    # Drop years that don't fall into the requested periods
    filter(!is.na(period)) %>%
    select(year, period, gdp_growth, all_of(x_vars))
  
  # Lock the factor levels so the periods always appear in chronological order
  df_filtered$period <- factor(df_filtered$period, levels = periods)
  
  # 4. Reshape data from wide to long
  df_long <- df_filtered %>%
    pivot_longer(
      cols = all_of(x_vars), 
      names_to = "indicator", 
      values_to = "value"
    ) %>%
    drop_na(gdp_growth, value)
  
  # 5. Calculate Correlations AND dynamic text positioning
  correlations <- df_long %>%
    group_by(indicator, period) %>%
    summarize(
      cor_val = cor(value, gdp_growth, use = "complete.obs"), 
      .groups = "drop"
    ) %>%
    mutate(
      cor_text = sprintf("Correlation = %.2f", cor_val),
      # This dynamically stacks the text lines on top of each other
      # Period 1 gets vjust = -0.5, Period 2 gets -2.0, Period 3 gets -3.5
      vjust_val = -0.5 - (as.numeric(period) - 1) * 1.5 
    )
  
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
  
  # 7. Adaptive Color Palette based on number of periods chosen
  palette <- if (length(periods) == 1) {
    c("#2C5B82") # One period: Dark Blue
  } else if (length(periods) == 2) {
    c("#B0C4DE", "#2C5B82") # Two periods: Light Blue, Dark Blue
  } else {
    c("#B0C4DE", "#5E8EAD", "#153C5E") # Three periods: Light, Medium, Dark Blue
  }
  
  # 8. Generate Plot
  p <- ggplot(df_long, aes(x = value, y = gdp_growth, color = period)) +
    # Scatter points
    geom_point(alpha = 0.8, size = 2.5) +
    
    # Regression Lines
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    
    # Correlation Text (Colors match the lines perfectly)
    geom_text(
      data = correlations,
      aes(x = Inf, y = -Inf, label = cor_text, vjust = vjust_val),
      hjust = 1.1, fontface = "bold", size = 2, show.legend = FALSE
    ) +
    
    # Apply colors
    scale_color_manual(values = palette) +
    
    # Facets
    facet_wrap(~ indicator, scales = "free_x", ncol = 2, 
               labeller = labeller(indicator = facet_labels)) +
    
    # Theming
    labs(
      y = "Real GDP Growth",
      x = "Indicator Growth"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 11, color = "#2C5B82", hjust = 0.5),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.background = element_rect(fill = "white", color = "grey80"),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 11, face = "bold"),
      axis.title = element_text(face = "bold", size = 11)
    )
  
  return(p)
}
