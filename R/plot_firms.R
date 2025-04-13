#' Plot FIRMS fire detections with brightness and FRP
#'
#' Visualizes FIRMS fire points on a basemap, color-coded by brightness and scaled by Fire Radiative Power (FRP).
#' Accepts both VIIRS (`bright_ti5`) and MODIS (`brightness`) data automatically.
#'
#' @param firms_sf An `sf` object of fire detections. Must include a brightness column (`bright_ti5` or `brightness`) and a `frp` column.
#' @param base_map Optional. Either an `sf` object or a file path (e.g., to shapefile or GeoPackage). If `NULL`, uses Natural Earth.
#'
#' @return A `ggplot` object showing the fire detections.
#'
#' @importFrom ggplot2 ggplot geom_sf aes scale_color_gradient scale_size coord_sf labs theme_minimal
#' @importFrom sf st_crs st_transform st_is_empty read_sf st_geometry
#' @importFrom rnaturalearth ne_countries
#'
#' @export

plot_firms <- function(firms_sf, base_map = NULL) {
  if (!inherits(firms_sf, "sf")) {
    stop("Error: firms_sf must be an sf object.")
  }

  firms_sf <- firms_sf[!sf::st_is_empty(firms_sf), ]

  if (nrow(firms_sf) == 0) {
    stop("No fire detections to plot.")
  }

  # Dynamically detect brightness column
  brightness_col <- if ("bright_ti5" %in% colnames(firms_sf)) {
    "bright_ti5"
  } else if ("brightness" %in% colnames(firms_sf)) {
    "brightness"
  } else {
    stop("Error: Could not find the brightness column (bright_ti5 or brightness).")
  }

  # Handle base_map input: NULL, file path, or sf object
  if (is.null(base_map)) {
    message("No base_map provided. Using Natural Earth country outlines.")
    base_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  } else if (is.character(base_map)) {
    if (!file.exists(base_map)) {
      stop("The base_map path does not exist: ", base_map)
    }
    message("Reading base_map from file: ", base_map)
    base_map <- sf::read_sf(base_map)
  }

  # Ensure CRS matches
  if (!is.null(sf::st_crs(base_map)) && sf::st_crs(base_map) != sf::st_crs(firms_sf)) {
    base_map <- sf::st_transform(base_map, sf::st_crs(firms_sf))
  }

  # Plot
  ggplot() +
    geom_sf(data = base_map, fill = "gray90", color = "black", lwd = 0.3) +
    geom_sf(
      data = firms_sf,
      aes(color = .data[[brightness_col]], size = .data$frp),
      alpha = 0.7
    ) +
    scale_color_gradient(low = "yellow", high = "red", name = "Brightness") +
    scale_size(range = c(1, 5), name = "Fire Radiative Power (FRP)") +
    coord_sf() +
    labs(
      title = "NASA FIRMS Fire Detections",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal()
}
