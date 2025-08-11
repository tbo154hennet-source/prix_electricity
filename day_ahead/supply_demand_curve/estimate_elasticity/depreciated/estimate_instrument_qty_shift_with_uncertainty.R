
bound = "lower"


linear_interpolate_with_uncertainty <- function(df, variable_name, target_price, bound = "estimate") {
  # Subset data for the variable
  sub_df <- df[df$variable == variable_name, ]
  
  # Sort by price
  sub_df <- sub_df[order(sub_df$price), ]
  
  # Compute the appropriate y-value based on the bound argument
  get_y <- function(estimate, std_error) {
    if (bound == "lower") {
      return(estimate - 1.96 * std_error)
    } else if (bound == "upper") {
      return(estimate + 1.96 * std_error)
    } else {
      return(estimate)
    }
  }
  
  # Check if target price is outside range
  if (target_price <= min(sub_df$price)) {
    idx <- which.min(sub_df$price)
    return(get_y(sub_df$estimate[idx], sub_df$std_error[idx]))
  }
  if (target_price >= max(sub_df$price)) {
    idx <- which.max(sub_df$price)
    return(get_y(sub_df$estimate[idx], sub_df$std_error[idx]))
  }
  
  # Find indices of closest prices below and above target
  below_idx <- max(which(sub_df$price <= target_price))
  above_idx <- min(which(sub_df$price >= target_price))
  
  # Extract points for interpolation
  x0 <- sub_df$price[below_idx]
  x1 <- sub_df$price[above_idx]
  
  y0 <- get_y(sub_df$estimate[below_idx], sub_df$std_error[below_idx])
  y1 <- get_y(sub_df$estimate[above_idx], sub_df$std_error[above_idx])
  
  # Linear interpolation formula
  y <- y0 + (target_price - x0) * (y1 - y0) / (x1 - x0)
  
  return(y)
}


generate_quantity_variation_with_uncertainty <- function(row) {
  
  target_price <- as.numeric(row["price"])  # extract the price from the row
  
  compute_variation <- function(bound) {
    results <- numeric(length(instruments))
    
    for (i in seq_along(instruments)) {
      if (i == 2 | i == 3) {
        var_supply <- paste0(instruments[i], "_supply")
        var_demand <- paste0(instruments[i], "_demand")
      } else {
        var_supply <- instruments[i]
        var_demand <- instruments[i]
      }
      
      # Supply
      if (var_supply %in% coef_supply_unmatched_predicted_indispo_data$variable) {
        supply_coef <- linear_interpolate_with_uncertainty(
          df = coef_supply_unmatched_predicted_indispo_data,
          variable_name = var_supply,
          target_price = target_price,
          bound = bound
        )
        supply_var <- supply_coef * as.numeric(row[var_supply])
      } else {
        supply_var <- NA_real_
      }
      
      # Demand
      if (var_demand %in% coef_demand_unmatched_predicted_indispo_data$variable) {
        demand_coef <- linear_interpolate_with_uncertainty(
          df = coef_demand_unmatched_predicted_indispo_data,
          variable_name = var_demand,
          target_price = target_price,
          bound = bound
        )
        demand_var <- demand_coef * as.numeric(row[var_demand])
      } else {
        demand_var <- NA_real_
      }
      
      results[i] <- supply_var + demand_var
    }
    
    return(sum(results, na.rm = TRUE))
  }
  
  # Compute all bounds
  return(c(
    central = compute_variation("estimate"),
    lower   = compute_variation("lower"),
    upper   = compute_variation("upper")
  ))
}

#price_indispo_merge_test <- price_indispo_merge[1:100,]


handlers(global = TRUE)
progressr::with_progress({
  p <- progressr::progressor(steps = nrow(price_indispo_merge))
  
  delta_q_list <- map2(seq_len(nrow(price_indispo_merge)), price_indispo_merge$price, function(i, price) {
    p()
    tryCatch({
      row <- price_indispo_merge[i, ]
      generate_quantity_variation_with_uncertainty(row)
    }, error = function(e) {
      message(sprintf("Erreur à l'index %d (prix = %.2f) : %s", i, price, e$message))
      rep(NA_real_, length(instruments))
    })
  })
})


delta_q_matrix_uncertain <- do.call(rbind, delta_q_list)
colnames(delta_q_matrix_uncertain) <- paste0("delta_q_", c( "central",
                                                  "lower",
                                                  "upper"))

# Ajouter au dataframe
price_indispo_merge_delta_uncertain <- bind_cols(price_indispo_merge, as.data.frame(delta_q_matrix_uncertain))
write_parquet(price_indispo_merge_delta_uncertain, "price_indispo_merge_delta_uncertain.parquet")




