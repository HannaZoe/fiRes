#' Identify uncategorized fire detections
#'
#' This function compares a raw FIRMS fire dataset with a list of previously classified fire points.
#' It returns only those fire detections that were not assigned a classification (i.e., did not appear in any of the classified datasets).
#'
#' @param firms_sf An `sf` object containing the original FIRMS fire detections.
#' @param classified_firms_list A list of `sf` objects, each containing a subset of `firms_sf` with fire classifications (e.g., from `fetch_osm()`).
#'
#' @return An `sf` object with uncategorized fire points. A `fire_type` column is added and set to `"unknown"`.
#'
#' @importFrom sf st_geometry
#' @export

uncategorized_fires <- function(firms_sf, classified_firms_list) {
  # Ensure firms_sf is an sf object
  if (!inherits(firms_sf, "sf")) {
    stop("Error: firms_sf must be an sf object.")
  }

  # Ensure classified_firms_list is a list of sf objects
  if (!is.list(classified_firms_list) || any(!sapply(classified_firms_list, inherits, what = "sf"))) {
    stop("Error: classified_firms_list must be a list of sf objects.")
  }

  # Combine all classified fire data
  classified_fires <- do.call(rbind, classified_firms_list)

  # Identify fires in the original dataset that are NOT in the classified dataset
  uncategorized_fires <- firms_sf[!firms_sf$geometry %in% classified_fires$geometry, ]

  # Assign "unknown" as fire_type
  uncategorized_fires$fire_type <- "unknown"

  message(nrow(uncategorized_fires), " uncategorized fires found.")
  return(uncategorized_fires)
}



