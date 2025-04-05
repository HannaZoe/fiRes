#' Calculate FIRMS fire frequency per location
#'
#' Groups fire detections by spatial cell using a true distance grid in UTM coordinates.
#'
#' @param firms_sf An sf object with point geometries.
#' @param resolution_km Numeric. Size of the grid cell in kilometers. Default is 1km.
#'
#' @return An sf object with `n_fires` column showing number of fires per cell.
#'
#' @importFrom sf st_crs st_transform st_coordinates st_as_sf st_point st_sfc
#' @importFrom dplyr group_by summarise n
#'
#' @export

fire_frequency <- function(firms_sf, resolution_km = 1) {

  if (!inherits(firms_sf, "sf")) stop("Input must be an sf object")

  # Get centroid of all points to choose UTM zone
  centroid <- sf::st_centroid(sf::st_union(firms_sf))
  lon <- sf::st_coordinates(centroid)[1]
  zone_number <- floor((lon + 180) / 6) + 1
  utm_crs <- paste0("+proj=utm +zone=", zone_number, " +datum=WGS84 +units=m +no_defs")

  # Reproject to UTM
  firms_utm <- sf::st_transform(firms_sf, crs = utm_crs)

  # Round UTM coordinates to resolution grid (in meters)
  coords <- sf::st_coordinates(firms_utm)
  res_m <- resolution_km * 1000
  rounded_x <- round(coords[, 1] / res_m) * res_m
  rounded_y <- round(coords[, 2] / res_m) * res_m
  cell_ids <- paste(rounded_x, rounded_y, sep = "_")

  firms_utm$cell_id <- cell_ids

  # Count number of fires per grid cell
  freq_df <- firms_utm |>
    dplyr::group_by(cell_id) |>
    dplyr::summarise(n_fires = dplyr::n(), .groups = "drop")

  # Create centroid geometry for each cell
  cell_coords <- do.call(rbind, strsplit(freq_df$cell_id, "_")) |>
    apply(2, as.numeric)

  freq_df$geometry <- sf::st_sfc(
    mapply(function(x, y) sf::st_point(c(x, y)),
           cell_coords[, 1], cell_coords[, 2], SIMPLIFY = FALSE),
    crs = utm_crs
  )
  freq_df <- sf::st_as_sf(freq_df)

  return(freq_df)
}
