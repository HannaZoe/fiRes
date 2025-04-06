#' Plot fire frequency by cell
#'
#' Visualizes the output of `calculate_fire_frequency()` as points scaled by number of fires.
#'
#' @param fire_freq_sf An sf object returned by `calculate_fire_frequency()`.
#' @param base_map Optional. Background map (sf object or path). Defaults to Natural Earth.
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_sf scale_size_continuous coord_sf labs theme_minimal
#' @importFrom sf st_crs st_transform st_bbox read_sf
#' @importFrom rnaturalearth ne_countries

plot_fire_frequency <- function(fire_freq_sf, base_map = NULL) {
  if (!inherits(fire_freq_sf, "sf") || !"n_fires" %in% names(fire_freq_sf)) {
    stop("Input must be an sf object with an 'n_fires' column")
  }

  # Handle base map input
  if (is.null(base_map)) {
    base_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  } else if (is.character(base_map)) {
    base_map <- sf::read_sf(base_map)
  }

  # Ensure CRS match
  if (!is.null(sf::st_crs(base_map)) && sf::st_crs(base_map) != sf::st_crs(fire_freq_sf)) {
    base_map <- sf::st_transform(base_map, sf::st_crs(fire_freq_sf))
  }

  # Get bounding box from fire points to zoom correctly
  fire_bbox <- sf::st_bbox(fire_freq_sf)

  # Plot
  ggplot() +
    geom_sf(data = base_map, fill = "gray95", color = "black", linewidth = 0.2) +
    geom_sf(data = fire_freq_sf, aes(size = .data$n_fires), color = "darkred", alpha = 0.6) +
    scale_size_continuous(name = "Number of Fires", range = c(1, 6)) +
    coord_sf() +
    labs(
      title = "Fire Frequency per Cell",
      x = "Longitude / X (UTM)", y = "Latitude / Y (UTM)"
    ) +
    theme_minimal()
}

