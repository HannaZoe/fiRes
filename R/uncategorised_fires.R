#' Identify uncategorised fires
#'
#' This function compares a raw FIRMS fire dataset with a list of previously classified fire points.
#' It returns only those fire detections that were not assigned a classification (i.e., did not appear in any of the classified datasets).
#' Uses spatial equality to identify overlaps. Also prints a classification summary.
#'
#' @param firms_sf An `sf` object containing the original FIRMS fire detections.
#' @param classified_firms_list A list of `sf` objects, each containing a subset of `firms_sf` with fire classifications (e.g., from `fetch_osm()`).
#'
#' @return An `sf` object with uncategorized fire points. A `fire_type` column is added and set to `"unknown"`.
#'
#' @importFrom sf st_geometry st_equals
#' @importFrom dplyr bind_rows
#'
#' @export
#'
categorised_fires <- function(firms_sf, classified_firms_list) {
  if (!inherits(firms_sf, "sf")) {
    stop("Error: firms_sf must be an sf object.")
  }

  if (!is.list(classified_firms_list) || any(!sapply(classified_firms_list, inherits, what = "sf"))) {
    stop("Error: classified_firms_list must be a list of sf objects.")
  }

  classified_fires <- dplyr::bind_rows(classified_firms_list)

  # Use spatial match to find unclassified fires
  matched <- sf::st_equals(firms_sf, classified_fires, sparse = FALSE)
  uncategorized_fires <- firms_sf[!apply(matched, 1, any), ]
  uncategorized_fires$fire_type <- "unknown"

  # Summary message
  message("FIRMS Fire Classification Summary:")
  message("Total fires: ", nrow(firms_sf))
  message("Uncategorized: ", nrow(uncategorized_fires))
  message("Classified: ", nrow(classified_fires))


  if ("fire_type" %in% colnames(classified_fires)) {
    fire_type_counts <- table(classified_fires$fire_type)
    for (type in names(fire_type_counts)) {
      message("  - ", type, ": ", fire_type_counts[[type]])
    }
  }

  return(uncategorized_fires)
}
