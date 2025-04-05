#' Plot fire season statistics and spatial distribution
#'
#' @param firms_sf An sf object with columns `fire_type` and `fire_season`
#' @param base_map Optional. Background map (sf object or file path). Default is Natural Earth.
#' @param fire_season_order Optional. A character vector with the desired fire season order (e.g., names from classify_fire_seasons)
#'
#' @return A list with two ggplot objects: $bar_plot and $map_plot
#' @export

plot_fire_seasons <- function(firms_sf, base_map = NULL) {
  # Check columns
  if (!all(c("fire_season", "fire_type") %in% names(firms_sf))) {
    stop("Input must contain 'fire_season' and 'fire_type' columns.")
  }

  # Drop geometry for bar chart
  df <- sf::st_drop_geometry(firms_sf)

  # --- Bar chart ---
  bar_plot <- ggplot(df, aes(x = fire_season, fill = fire_type)) +
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
