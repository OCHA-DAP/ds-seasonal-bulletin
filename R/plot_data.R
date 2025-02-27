library(ggplot2)
library(patchwork)
library(ggrepel)
library(dplyr)
library(glue)
library(terra)
library(sf)
library(tidyterra)

#' Creates a side-by-side plot of average precipitation and 
#' current precipitation anomaly
plot_anomalies <- function(season_avg, current_yearly_sum, gdf, save = FALSE) {
  # First, calculate global min and max for avg and current plots
  all_values <- c(values(season_avg), values(current_yearly_sum))
  global_min <- min(all_values, na.rm = TRUE)
  global_max <- max(all_values, na.rm = TRUE)

  # Mask rasters with gdf_sel
  season_avg_masked <- mask(season_avg, gdf)
  current_yearly_sum_masked <- mask(current_yearly_sum, gdf)
  anomaly_masked <- mask(current_yearly_sum - season_avg, gdf)

  # Calculate max absolute anomaly for symmetric scale
  max_abs_anomaly <- max(abs(values(anomaly_masked)), na.rm = TRUE)

  # Plot the average historical
  avg_plot <- ggplot() +
    geom_spatraster(data = season_avg_masked, na.rm = TRUE) +
    geom_sf(data = gdf, fill = NA, color = "darkgrey", linewidth = 0.3) +
    scale_fill_gradient_hdx_sapphire(
      name = "Rainfall\n(mm)",
      limits = c(global_min, global_max)
    ) +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
      axis.text.y = element_blank(),
      axis.text.x = element_blank()
    ) +
    labs(title = "Average historical")

  # Plot the anomalies against the upcoming season
  anomaly_plot <- ggplot() +
    geom_spatraster(data = anomaly_masked, na.rm = TRUE) +
    geom_sf(data = gdf, fill = NA, color = "darkgrey", linewidth = 0.3) +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      midpoint = 0,
      limits = c(-max_abs_anomaly, max_abs_anomaly),
      na.value = NA,
      name = "Rainfall\nAnomaly (mm)"
    ) +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
      axis.text.y = element_blank(),
      axis.text.x = element_blank()
    ) + 
    labs(title = "Anomalies")

  # Combine the two plots
  combined_plot <- wrap_plots(list(avg_plot, anomaly_plot), nrow = 1) +
    plot_annotation(
      title = "Anomalies in total seasonal rainfall",
      theme = theme(plot.title = element_text(size = 14))
    )
  if (save == TRUE) {
    ggsave(
      filename = glue("plots/{ISO3}_{SEASON}_{YEAR}_{DATASET}_anomaly.png"),
      plot = combined_plot,
      dpi = 300, 
      width = 301/300,
      height = 252/300

    )
  }

  return(combined_plot)
}

#' Creates a plot of return periods per pcode,
#' with pcodes in the lower tercile level highlighted
plot_return_periods <- function(gdf_rp, gdf_lower_quantile, save = FALSE) {
  plot_rp <- ggplot() +
    geom_sf(data = gdf_rp, aes(fill = return_period)) +
    geom_sf(data = gdf_lower_quantile, aes(color = glue("Lower tercile rainfall")), fill = NA,linewidth =1 ) +
    scale_color_manual(
      name = NULL,  # No title for boundary legend
      values = c("Lower tercile rainfall" = "tomato")  # Color for the boundary lines
    ) +
    scale_fill_gradient_hdx_tomato(na.value = "lightgrey", name="Return Period (years)")+
    labs(
      title = glue("Rainfall Return Periods"),
      caption = "Higher return periods indicate rarer low-rainfall events"
    )

  if (save == TRUE) {
    ggsave(
      filename = glue("plots/{ISO3}_{SEASON}_{YEAR}_{DATASET}_rp.png"),
      plot = plot_rp,
      dpi = 300, 
      width = 385/300,
      height = 342/300

    )
  }
  return(plot_rp)
}

#' Creates a scatter plot of total seasonal precipitation / affected pop
#' With current and selected years highlighted
plot_annual_scatter <- function(
  df_annual_summary,
  reference_years,
  highlight_year,
  pop_var,
  save = FALSE
) {

  plot_scatter <- ggplot(df_annual_summary, aes(x = total_rainfall, y = total_pop)) +
    geom_point(aes(color = point_color)) +
    geom_text_repel(
      data = df_annual_summary %>% filter(year %in% c(reference_years, highlight_year)),
      aes(label = year, color = point_color),
      box.padding = 0.3,    # Increases space between text and point
      point.padding = 0.2,  # Increases minimum space between text and point
      #segment.color = "grey50",  # Color of connecting lines
      min.segment.length = 0,    # Always draw line segments
      force = 2,                # Increase force of repulsion
      fontface = "bold"
    ) +
    scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
    scale_color_identity() +
    labs(
      title = glue("Total seasonal rainfall vs est. {pop_var} impacted by drought"),
      x = "Total Rainfall (mm)",
      y = glue("Est. {pop_var} Impacted by Drought"),
      subtitle = glue("From 2000 to {highlight_year}")
    )
  if (save == TRUE) {
    ggsave(
      filename = glue("plots/{ISO3}_{SEASON}_{YEAR}_{DATASET}_scatter.png"),
      plot = plot_scatter,
      dpi = 300,
      width = 765/300,
      height = 377/300

    )
  }

    return(plot_scatter)
}