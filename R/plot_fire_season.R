#' Plot fire season statistics and spatial distribution
#'
#' This function generates two plots: a bar chart showing the number of fires by fire season and land use type,
#' and a map visualizing the spatial distribution of fires by fire season.
#'
#' @param firms_sf An `sf` object with at least two columns: `fire_type` and `fire_season`.
#' @param base_map Optional. An `sf` object or file path to a shapefile/GPKG. If `NULL`, Natural Earth data is used.
#'
#' @return A list of two `ggplot` objects: `$bar_plot` (stacked bar chart) and `$map_plot` (spatial plot by season).
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_sf labs scale_fill_brewer scale_color_brewer theme_minimal
#' @importFrom sf st_drop_geometry st_crs st_transform read_sf
#' @importFrom rnaturalearth ne_countries
#' #' @importFrom rlang .data
#'
#' @export

plot_fire_seasons <- function(firms_sf, base_map = NULL) {
  # Check columns
  if (!all(c("fire_season", "fire_type") %in% names(firms_sf))) {
    stop("Input must contain 'fire_season' and 'fire_type' columns.")
  }

  # Drop geometry for bar chart
  df <- sf::st_drop_geometry(firms_sf)

  # --- Bar chart ---
  bar_plot <- ggplot(df, aes(x = .data$fire_season, fill = .data$fire_type)) +
    geom_bar(position = "stack") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "FIRMS Fires by Season and Land Use Type",
      x = "Fire Season", y = "Number of Fires",
      fill = "Land Use"
    ) +
    theme_minimal()

  # --- Base map handling ---
  if (is.null(base_map)) {
    base_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  } else if (is.character(base_map)) {
    base_map <- sf::read_sf(base_map)
  }

  # Reproject base map if needed
  if (!is.null(sf::st_crs(base_map)) && sf::st_crs(base_map) != sf::st_crs(firms_sf)) {
    base_map <- sf::st_transform(base_map, sf::st_crs(firms_sf))
  }

  # --- Map plot ---
  map_plot <- ggplot() +
    geom_sf(data = base_map, fill = "gray95", color = "black", linewidth = 0.2) +
    geom_sf(data = firms_sf, aes(color = fire_season), size = 1.5, alpha = 0.7) +
    scale_color_brewer(palette = "Dark2", name = "Fire Season") +
    labs(
      title = "Spatial Distribution of Fires by Season",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal()

  return(list(bar_plot = bar_plot, map_plot = map_plot))
}
