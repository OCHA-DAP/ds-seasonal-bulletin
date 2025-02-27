library(dplyr)
library(lubridate)

#' Process ERA5 rainfall data by calculating total rainfall for specified months and 
#' creating tercile and quartile classifications.
#' @param df_era5 Data frame with valid_date, mean precipitation, and pcode columns
#' @param months_to_include Numeric vector of months to analyze 
#' @return Data frame with total rainfall, return periods, and quantile classifications by pcode and year
process_era5_rainfall <- function(
    df_era5, 
    months_to_include
) {
  df_precip_summary <- df_era5 %>% 
    mutate(year = year(valid_date), month = month(valid_date)) %>%
    # Transform mm/day to mm/month
    mutate( mean = mean * lubridate::days_in_month(valid_date)) %>%
    filter(month %in% months_to_include) %>%
    group_by(pcode, year) %>% 
    summarise(total_rainfall = sum(mean, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(pcode, year) %>%
    mutate(year = as.numeric(year)) %>%
    group_by(pcode) %>%
    mutate(
      tercile = ntile(total_rainfall, 3),
      quartile = ntile(total_rainfall, 4),
      is_lower_tercile = tercile == 1,
      is_lower_quartile = quartile == 1
    ) %>%
    mutate(
      rank = rank(total_rainfall, ties.method = "average"),
      exceedance_prob = rank / (n() + 1),
      return_period = 1 / exceedance_prob
    ) %>%
    ungroup()
  
  return(df_precip_summary)
}

#' Process SEAS5 rainfall data by calculating total rainfall for specified months and 
#' creating tercile and quartile classifications.
#' @param df_seas5 Data frame with valid_date, mean precipitation, and pcode columns
#' @param months_to_include Numeric vector of months to analyze
#' @param sel_month Month the forecast was issued
#' @return Data frame with total rainfall, return periods, and quantile classifications by pcode and year
process_seas5_rainfall <- function(
    df_seas5,
    months_to_include,
    sel_month
) {

  df_precip_summary <- df_seas5 %>%
    mutate(
      year = year(valid_date), 
      valid_month = month(valid_date), 
      issued_month = month(issued_date)
    ) %>%
    filter(valid_month %in% months_to_include) %>%
    filter(issued_month %in% sel_month) %>%
    # Transform mm/day to mm/month
    mutate( mean = mean * lubridate::days_in_month(valid_date)) %>%
    group_by(pcode, year) %>% 
    summarise(total_rainfall = sum(mean, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(year = as.numeric(year)) %>%
    group_by(pcode) %>%
    mutate(
      tercile = ntile(total_rainfall, 3),
      is_lower_tercile = tercile == 1,
    ) %>%
    mutate(
      rank = rank(total_rainfall, ties.method = "average"),
      exceedance_prob = rank / (n() + 1),
      return_period = 1 / exceedance_prob
    ) %>%
    ungroup()
  
  return(df_precip_summary)
}

#' Joins the population and rainfall dataframes by pcode, identifying 
#' affected population based on occurrence in lower tercile and quartiles
join_pop_rainfall <- function(df_pop, df_rainfall, pcode_col) {
  df_summary <- df_rainfall %>%
    left_join(df_pop, by = setNames(pcode_col, "pcode")) %>%
    mutate(TotalPop_tercile = if_else(is_lower_tercile == FALSE, 0, TotalPop))
  return(df_summary)
}

#' Based on an input dataframe
#' summarizes annual affected population
calc_yearly_impact <- function(df) {
  col <- "TotalPop_tercile"
  df_ <- df %>% 
    group_by(year) %>% 
    summarise(
      total_affected = sum(!!sym(col), na.rm=TRUE),
      affected_zones = paste(pcode[!!sym(col) != 0], collapse = ", "),
      total_unique_pcodes = n_distinct(pcode),
      affected_unique_pcodes = n_distinct(pcode[!!sym(col) != 0]),
      proportion_zones_affected = affected_unique_pcodes / total_unique_pcodes
    ) %>%
    ungroup() %>%
    arrange(year)
  
  return(df_)
}