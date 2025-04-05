#' Plot fire frequency by cell
#'
#' Visualizes the output of `calculate_fire_frequency()` as points scaled by number of fires.
#'
#' @param fire_freq_sf An sf object returned by `calculate_fire_frequency()`.
#' @param base_map Optional. Background map (sf object or path). Defaults to Natural Earth.
#'
#' @return A ggplot object
#' @export
plot_fire_frequency <- function(fire_freq_sf, base_map = NULL) {
  if (!inherits(fire_freq_sf, "sf") || !"n_fires" %in% names(fire_freq_sf)) {
    stop("Input must be an sf object with an 'n_fires' column")
  }

  if (is.null(base_map)) {
    base_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  } else if (is.character(base_map)) {
    base_map <- sf::read_sf(base_map)
  }

  if (!is.null(sf::st_crs(base_map)) && sf::st_crs(base_map) != sf::st_crs(fire_freq_sf)) {
    base_map <- sf::st_transform(base_map, sf::st_crs(fire_freq_sf))
  }

  fire_bbox <- sf::st_bbox(fire_freq_sf)

  ggplot() +
    geom_sf(data = base_map, fill = "gray95", color = "black", linewidth = 0.2) +
    geom_sf(data = fire_freq_sf, aes(size = n_fires), color = "darkred", alpha = 0.6) +
    scale_size_continuous(name = "Number of Fires", range = c(1, 6)) +
    coord_sf(xlim = c(fire_bbox$xmin, fire_bbox$xmax), ylim = c(fire_bbox$ymin, fire_bbox$ymax), expand = TRUE) +
    labs(title = "Fire Frequency per Cell", x = "Longitude", y = "Latitude") +
    theme_minimal()
}
