#' Calculate FIRMS fire frequency per location
#'
#' Groups fire detections by spatial cell and returns frequency of fire events.
#'
#' @param firms_sf An sf object with point geometries.
#' @param resolution_km Numeric. Size of the grid cell in kilometers. Default is 1km.
#'
#' @return An sf object with `n_fires` column showing number of fires per cell.
#' @export
calculate_fire_frequency <- function(firms_sf, resolution_km = 1) {
  if (!inherits(firms_sf, "sf")) stop("Input must be an sf object")
  if (!sf::st_is_longlat(firms_sf)) {
    firms_sf <- sf::st_transform(firms_sf, 4326)
  }

  # Round coordinates to nearest resolution
  coords <- sf::st_coordinates(firms_sf)
  rounded_lon <- round(coords[, 1] / (resolution_km / 111)) * (resolution_km / 111)
  rounded_lat <- round(coords[, 2] / (resolution_km / 111)) * (resolution_km / 111)
  cell_ids <- paste(rounded_lon, rounded_lat, sep = "_")

  firms_sf$cell_id <- cell_ids

  freq_df <- firms_sf |>
    dplyr::group_by(cell_id) |>
    dplyr::summarise(n_fires = dplyr::n(), .groups = "drop")

  # Create centroid points
  cell_coords <- do.call(rbind, strsplit(freq_df$cell_id, "_")) |>
    apply(2, as.numeric)

  freq_df$geometry <- sf::st_sfc(mapply(function(x, y) sf::st_point(c(x, y)),
                                        cell_coords[,1], cell_coords[,2], SIMPLIFY = FALSE),
                                 crs = 4326)
  freq_df <- sf::st_as_sf(freq_df)
  return(freq_df)
}
