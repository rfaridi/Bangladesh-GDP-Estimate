#=======================================

# this script file creates function for regression plot
# as shown in wb india gdp estimate paper. this function
# itself consists of three sub-functions. 

#======================================



# data <- panel_gdp
# start_year <- 2016
# end_year <- NULL
# explain_vars <- x_vars_reg

#======================
# data prep
#=====================


sfn_data_prep <- function(data,start_year,end_year=NULL,explain_vars){
  if (is.null(end_year)) {
    end_year <- max(data$year, na.rm = TRUE)
  }
  df_new <- data |> 
    filter(
      oil_producer == 0,
      small_1995 == 0,
      fragile_state == 0,
      year >= start_year & year <= end_year
    ) |> 
    drop_na(
      all_of(explain_vars)
    )  |> 
    group_by(country_name) |>
    summarise(
      across(
        c(gdp_growth, all_of(explain_vars), bangladesh),
        ~ mean(., na.rm = TRUE)
      ),
      .groups = "drop"
    ) 
  return(df_new)
}

#======================
# reg data
#=====================


sfn_gdp_reg <- function(data,explain_vars) {
  rhs <- paste(explain_vars, collapse = " + ")
  formula_str <- paste("gdp_growth ~", rhs)
  gdp_reg_formula <- as.formula(formula_str)
  mod1 <- lm(gdp_reg_formula, data = data)
  bgd_coef <- coef(mod1)["bangladesh"]

  data_reg <- data |> 
    mutate(
        yhat = predict(mod1),
        yhat_nobgd = yhat - (bgd_coef * bangladesh)
  )

  mod2 <- lm(gdp_growth ~ yhat_nobgd, data = data_reg)

  pred2 <- predict(mod2, se.fit = TRUE)

  data_reg <- data_reg |> 
                          mutate(
                              fit = pred2$fit,
                              sefit = pred2$se.fit,
                              ub = fit + qnorm(0.975) * sefit, # qnorm(0.975) is R's invnormal(0.975)
                              lb = fit - qnorm(0.975) * sefit
                              )
   return(data_reg)
}

#======================
# plot data
#=====================

#data=data_reg

sfn_plot_reg <- function(data,period_title,show_y) {
  ggplot(data, aes(x = yhat_nobgd)) +
      geom_ribbon(aes(ymin = lb, ymax = ub), fill = rgb(186, 181, 213, maxColorValue = 255)) +
      geom_line(aes(y = fit), color = rgb(70, 55, 183, maxColorValue = 255), linewidth = 1) +
      geom_point(data = filter(data, bangladesh == 1), 
             aes(y = gdp_growth), 
             shape = 1, size = 2, color = "blue", stroke = 1.2) +
      geom_text(data = filter(data, bangladesh == 1), 
            aes(y = gdp_growth, label = "Bangladesh"), 
            vjust = -0.9, color = "black", size = 4) +
      labs(title=period_title,
           x = NULL, 
           y = if(show_y) "Actual Growth" else NULL) +
      theme_minimal() +
  theme(
    plot.background = element_rect(fill = rgb(242, 242, 242, maxColorValue = 255), color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )
  
}  

#data=panel_gdp;start_year=1996 ; end_year=2015

fn_regression_plot <- function(data,start_year,end_year=NULL,explain_vars,show_y=TRUE) {
   prepped_data <- sfn_data_prep(data,start_year,end_year = NULL,explain_vars)
   reg_data <- sfn_gdp_reg(prepped_data,explain_vars)
   if (is.null(end_year)) {
     actual_end_year <- max(data$year, na.rm = TRUE)
   } else {
     actual_end_year <- end_year
   }
   period_title <- paste(start_year, "-", actual_end_year)
   sfn_plot_reg(reg_data,period_title,show_y)
}

#========  testing zone ================


# x_vars_reg <- c("real_tax_growth",  
#                 "energy_consumption_growth", "real_credit_growth", "export_vol_growth", 
#                 "import_vol_growth", "bangladesh")

# data=panel_gdp, start_year=2016,end_year=NULL, explain_vars=x_vars_reg

# data=panel_gdp, start_year=2016,end_year=NULL, explain_vars

#prepped_data <- sfn_data_prep(data,start_year=2016,end_year = NULL,explain_vars=x_vars_reg)

#fn_regression_plot(data=panel_gdp, start_year=2016,end_year=NULL, show_y=TRUE,explain_vars=x_vars_reg)

#fn_regression_plot(data=panel_gdp, start_year=1995,end_year=2015, explain_vars)
