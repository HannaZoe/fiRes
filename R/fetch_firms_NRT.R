#' Fetch FIRMS NRT Fire Data (max. 10-day window)
#'
#' This function retrieves Near Real-Time (NRT) fire detections from NASA's FIRMS API.
#' For VIIRS data, FIRMS data up to 10 days is available. For MODIS data, up to 7 days.
#' It supports up to 10 days of data.
#' Confidence filtering and spatial clipping is included.
#'
#' @param api_key Character. Your NASA API key.
#' @param region_sf An `sf` object representing the region of interest. Must be in WGS84.
#' @param start_date Character or Date. Start date in "YYYY-MM-DD" format.
#' @param end_date Character or Date. End date in "YYYY-MM-DD" format.
#' @param dataset Character. One of "VIIRS_SNPP_NRT" or "VIIRS_NOAA20_NRT". Defaults to "VIIRS_SNPP_NRT".
#' @param confidence_level Optional. One or more of "l", "n", "h". Filters by fire confidence.
#' @return An `sf` object with fire detections clipped to the region, or NULL if no data.
#'
#' @export
#'
#' @importFrom sf st_crs st_transform st_bbox st_as_sf st_filter
#' @importFrom dplyr filter mutate case_when
#' @importFrom utils read.csv download.file

fetch_firms_NRT <- function(api_key, region_sf, start_date, end_date,
                            dataset = c("VIIRS_SNPP_NRT", "VIIRS_NOAA20_NRT", "MODIS_NRT"),
                            confidence_level = NULL) {

  dataset <- match.arg(dataset)

  if (!inherits(region_sf, "sf")) {
    stop("The input region must be an 'sf' object.")
  }

  # Reproject to WGS 84 if needed
  if (sf::st_crs(region_sf)$epsg != 4326) {
    message("Reprojecting input to WGS 84 (EPSG:4326)...")
    region_sf <- sf::st_transform(region_sf, crs = 4326)
  }

  # Format dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  total_days <- as.numeric(difftime(end_date, start_date, units = "days")) + 1

  # Prevent requests older than ~10 days
  if (as.numeric(Sys.Date() - end_date) > 10) {
    stop("NRT datasets only support dates from the last ~7-10 days. Use 'fetch_firms_historic()' for older data.")
  }

  if (total_days > 10) {
    stop("NRT datasets can only be fetched in a 10-day range or less.")
  }

  # Get bounding box string
  bbox <- sf::st_bbox(region_sf)
  bbox_str <- paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep = ",")

  # Build URL
  base_url <- "https://firms.modaps.eosdis.nasa.gov/api/area/csv/"
  url <- paste0(base_url, api_key, "/", dataset, "/", bbox_str, "/", total_days, "/")

  # Download data
  temp_file <- tempfile(fileext = ".csv")
  suppressMessages(suppressWarnings(
    download.file(url, temp_file, mode = "wb", quiet = TRUE)
  ))

  # Read and parse
  tryCatch({
    firms_data <- suppressWarnings(read.csv(temp_file, stringsAsFactors = FALSE))

    if (nrow(firms_data) == 0 || !all(c("latitude", "longitude") %in% names(firms_data))) {
      message("No fire data returned for this request.")
      return(NULL)
    }

    firms_sf <- sf::st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)
    firms_sf <- sf::st_filter(firms_sf, region_sf)

    if (nrow(firms_sf) == 0) return(NULL)

    # Detect dataset type
    is_viirs <- "bright_ti4" %in% names(firms_data)
    is_modis <- "brightness" %in% names(firms_data)

    detected_dataset <- if (is_viirs) "VIIRS" else if (is_modis) "MODIS" else "MODIS"

    # Apply confidence filter
    if (!is.null(confidence_level) && "confidence" %in% names(firms_sf)) {
      confidence_level <- as.character(confidence_level)

      if (detected_dataset == "VIIRS") {
        firms_sf <- dplyr::filter(firms_sf, confidence %in% confidence_level)
      } else {
        firms_sf$confidence <- suppressWarnings(as.numeric(firms_sf$confidence))
        firms_sf <- firms_sf %>%
          dplyr::mutate(confidence_category = dplyr::case_when(
            confidence <= 30 ~ "l",
            confidence > 30 & confidence <= 80 ~ "n",
            confidence > 80 ~ "h",
            TRUE ~ NA_character_
          )) %>%
          dplyr::filter(!is.na(confidence_category) & confidence_category %in% confidence_level)
      }
    }

    return(firms_sf)
  }, error = function(e) {
    message("Error fetching FIRMS NRT data: ", e$message)
    return(NULL)
  })
}
