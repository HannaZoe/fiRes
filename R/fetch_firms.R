#' Fetch FIRMS fire data from NASA's FIRMS API
#'
#' This function retrieves fire detection data from NASA's FIRMS (Fire Information for Resource Management System) API
#' based on a specified region, time range, and dataset. If the request exceeds 10 days, it will automatically
#' split the request into multiple API calls to respect the API limits.
#'
#' @param api_key Character. Your NASA API key. You can register for one at \url{https://firms.modaps.eosdis.nasa.gov/api/map_key}.
#' @param region_sf An `sf` object representing the area of interest. It will be reprojected to WGS 84 if needed.
#' @param start_date Start date of the time range to fetch fire data (in "YYYY-MM-DD" format).
#' @param end_date End date of the time range to fetch fire data (in "YYYY-MM-DD" format).
#' @param dataset Character. Choose between `"VIIRS_SNPP_NRT"` or `"MODIS_NRT"`. Defaults to `"VIIRS_SNPP_NRT"`.
#' @param confidence_level Optional. Numeric filter for confidence level (depending on the dataset).
#'
#' @return An `sf` object containing fire detections within the selected time range and area.
#' @export

fetch_firms <- function(api_key, region_sf, start_date, end_date,
                             dataset = c("VIIRS_SNPP_NRT", "MODIS_NRT"),
                             confidence_level = NULL) {
  dataset <- match.arg(dataset)  # Ensure valid dataset input

  # Ensure region is an sf object
  if (!inherits(region_sf, "sf")) {
    stop("Error: The input region must be a shapefile in sf format.")
  }

  # Ensure region is in WGS 84
  if (st_crs(region_sf)$epsg != 4326) {
    message("Reprojecting input shapefile to WGS 84 (EPSG:4326)...")
    region_sf <- st_transform(region_sf, crs = 4326)
  }

  # Extract bounding box for API request
  bbox <- st_bbox(region_sf)
  bbox_str <- paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep = ",")

  # Convert dates to Date format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Calculate total number of days
  total_days <- as.numeric(difftime(end_date, start_date, units = "days")) + 1

  # If the request is ≤ 10 days, fetch as usual
  if (total_days <= 10) {
    return(fetch_firms_chunk(api_key, region_sf, start_date, end_date, dataset, confidence_level, bbox_str))
  }

  # If the request is > 10 days, break it into chunks
  message("Splitting request into multiple 10-day chunks...")

  all_fires <- NULL  # Empty object to store results
  current_start <- start_date

  while (current_start <= end_date) {
    # Define end date for this chunk (max 10 days per request)
    current_end <- min(current_start + 9, end_date)

    message("Fetching data from ", current_start, " to ", current_end)

    chunk_data <- fetch_firms_chunk(api_key, region_sf, current_start, current_end, dataset, confidence_level, bbox_str)

    # Merge results
    if (!is.null(chunk_data)) {
      all_fires <- if (is.null(all_fires)) chunk_data else rbind(all_fires, chunk_data)
    }

    # Move to the next chunk
    current_start <- current_end + 1
  }

  message("Successfully retrieved all requested FIRMS data!")
  return(all_fires)
}






















