#' Plot raw FIRMS fire detections with brightness and FRP
#'
#' This function visualizes FIRMS fire detections (from VIIRS or MODIS) on a base map.
#' Fire points are color-coded by brightness (`brightness` or `bright_ti4`, depending on the dataset)
#' and scaled in size by Fire Radiative Power (`frp`).
#'
#' @param firms_sf An `sf` object containing FIRMS fire detections. Must include a `brightness` or `bright_ti4` column,
#' and an `frp` column for Fire Radiative Power.
#' @param base_map Optional. An `sf` object to use as the base map. If `NULL`, a cropped country-level base map from `rnaturalearth` will be used.
#'
#' @return A `ggplot` object displaying fire points with brightness and FRP on a zoomed map.
#'
#' @importFrom ggplot2 ggplot geom_sf geom_point aes scale_color_gradient scale_size coord_sf labs theme_minimal
#' @importFrom sf st_coordinates st_bbox st_intersects
#' @importFrom rnaturalearth ne_countries
#' @export
#'
plot_firms <- function(firms_sf, base_map = NULL) {
  if (!inherits(firms_sf, "sf")) {
    stop("Error: firms_sf must be an sf object.")
  }

  # Dynamically detect brightness column (VIIRS vs. MODIS)
  brightness_col <- if ("bright_ti4" %in% colnames(firms_sf)) "bright_ti4" else "brightness"

  # Check if the brightness column exists
  if (!brightness_col %in% colnames(firms_sf)) {
    stop("Error: Could not find the brightness column (bright_ti4 or brightness).")
  }

  # Determine the bounding box of fire data
  fire_bbox <- st_bbox(firms_sf)

  # If no custom base map is provided, use a cropped country map
  if (is.null(base_map)) {
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    base_map <- world[st_intersects(world, firms_sf, sparse = FALSE), ]  # Filter only relevant areas
  }

  ggplot() +
    geom_sf(data = base_map, fill = "gray90", color = "black", lwd = 0.3) +  # Base map (zoomed)
    geom_point(data = firms_sf,
               aes(x = st_coordinates(firms_sf)[,1],
                   y = st_coordinates(firms_sf)[,2],
                   color = .data[[brightness_col]],
                   size = frp), alpha = 0.7) +  # Fire points
    scale_color_gradient(low = "yellow", high = "red", name = "Brightness") +
    scale_size(range = c(1, 5), name = "Fire Radiative Power (FRP)") +
    coord_sf(xlim = c(fire_bbox$xmin, fire_bbox$xmax), ylim = c(fire_bbox$ymin, fire_bbox$ymax), expand = TRUE) +  # Zoom to fires
    labs(title = "NASA FIRMS Fire Detections",
         x = "Longitude", y = "Latitude") +
    theme_minimal()
}


