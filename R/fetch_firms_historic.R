#' Fetch FIRMS Historical Fire Data (MODIS_C6, VIIRS_SNPP, VIIRS_NOAA20)
#'
#' This function retrieves historical fire detections from FIRMS, using datasets
#' that support requests by day (e.g., MODIS_SP, VIIRS_SNPP, VIIRS_NOAA20).
#' It loops over each date and merges results, therfore loading times may vary.
#'
#' @param api_key Character. Your NASA API key.
#' @param region_sf An `sf` object representing the region of interest.
#' @param start_date Start date ("YYYY-MM-DD").
#' @param end_date End date ("YYYY-MM-DD").
#' @param dataset One of "MODIS_C6", "VIIRS_SNPP", or "VIIRS_NOAA20".
#' @param confidence_level Optional. One or more of "l", "n", "h".
#' @return An `sf` object of all merged daily detections or NULL if no fires found.
#' @export
#'
#' @importFrom sf st_transform st_crs st_bbox st_filter st_as_sf
#' @importFrom dplyr filter mutate case_when
fetch_firms_historic <- function(api_key, region_sf, start_date, end_date,
                                 dataset = c("MODIS_SP", "VIIRS_SNPP_SP", "VIIRS_NOAA20_SP"),
                                 confidence_level = NULL) {

  dataset <- match.arg(dataset)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  dates <- seq(start_date, end_date, by = "1 day")

  if (!inherits(region_sf, "sf")) {
    stop("Input region must be an 'sf' object.")
  }

  if (sf::st_crs(region_sf)$epsg != 4326) {
    message("Reprojecting input to WGS 84 (EPSG:4326)...")
    region_sf <- sf::st_transform(region_sf, 4326)
  }

  bbox <- sf::st_bbox(region_sf)
  bbox_str <- paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep = ",")
  all_fires <- NULL

  message("Fetching FIRMS historic data now...")

  for (current_date in dates) {
    fires_day <- fetch_firms_day(api_key, region_sf, current_date, dataset, confidence_level, bbox_str)

    if (!is.null(fires_day)) {
      all_fires <- if (is.null(all_fires)) fires_day else rbind(all_fires, fires_day)
    }
  }

  if (is.null(all_fires)) {
    message("No fire detections found in this period.")
    return(NULL)
  }

  message("Successfully fetched all FIRMS historic data.")
  return(all_fires)
}
