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

  ggplot() +
    geom_sf(data = base_map, fill = "gray95", color = "black", linewidth = 0.2) +
    geom_sf(data = fire_freq_sf, aes(size = n_fires), color = "darkred", alpha = 0.6) +
    scale_size_continuous(name = "Number of Fires", range = c(1, 6)) +
    coord_sf() +
    labs(title = "Fire Frequency per Cell", x = "Longitude", y = "Latitude") +
    theme_minimal()
}


#' Plot fire season counts by type
#'
#' @param firms_sf An sf object with columns `fire_type` and `fire_season`
#'
#' @return A ggplot object showing fire counts by season and type
#' @export
plot_fire_seasons <- function(firms_sf) {
  if (!all(c("fire_season", "fire_type") %in% names(firms_sf))) {
    stop("Input must contain 'fire_season' and 'fire_type' columns.")
  }

  df <- firms_sf |> sf::st_drop_geometry()

  ggplot(df, aes(x = fire_season, fill = fire_type)) +
    geom_bar(position = "stack") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "FIRMS Fires by Season and Land Use Type",
      x = "Fire Season", y = "Number of Fires",
      fill = "Land Use"
    ) +
    theme_minimal()
}
