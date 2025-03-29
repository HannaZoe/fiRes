#' Helper function to fetch a single chunk of FIRMS fire data
#'
#' This internal function retrieves a chunk (up to 10 days) of FIRMS fire data from NASA's FIRMS API
#' for a specific bounding box and time range. It also applies spatial filtering based on a user-defined region
#' and optionally filters fire points based on confidence levels.
#'
#' @param api_key Character. Your NASA API key.
#' @param region_sf An `sf` object defining the region of interest. Must be in WGS 84.
#' @param start_date Start date of the chunk (in "YYYY-MM-DD" format).
#' @param end_date End date of the chunk (in "YYYY-MM-DD" format).
#' @param dataset Character. Either `"VIIRS_SNPP_NRT"` or `"MODIS_NRT"`.
#' @param confidence_level Optional. A character or numeric vector defining the confidence levels to filter.
#' For MODIS, use `"l"`, `"n"`, `"h"`. For VIIRS, use numeric thresholds.
#' @param bbox_str A comma-separated string of the bounding box coordinates (xmin, ymin, xmax, ymax).
#'
#' @return An `sf` object with fire detections that fall inside the given region and match the confidence filter, or `NULL` if no data was found.
#' @keywords internal
#' @noRd

fetch_firms_chunk <- function(api_key, region_sf, start_date, end_date, dataset, confidence_level, bbox_str) {
  day_range <- as.numeric(difftime(end_date, start_date, units = "days")) + 1

  # Construct the FIRMS API URL
  base_url <- "https://firms.modaps.eosdis.nasa.gov/api/area/csv/"
  url <- paste0(base_url, api_key, "/", dataset, "/", bbox_str, "/", day_range, "/")

  message("Fetching FIRMS data from: ", url)
  temp_file <- tempfile(fileext = ".csv")

  tryCatch({
    download.file(url, temp_file, mode = "wb")

    # Read data
    firms_data <- read.csv(temp_file, stringsAsFactors = FALSE)

    if (nrow(firms_data) == 0) {
      message("No fire data available for this chunk.")
      return(NULL)
    }

    # Convert fire data to an sf object (point data)
    firms_sf <- st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)

    # Filter to include only points inside the shapefile
    firms_sf <- firms_sf[st_within(firms_sf, region_sf, sparse = FALSE), ]

    if (nrow(firms_sf) == 0) {
      message("No fires found inside the provided region for this chunk.")
      return(NULL)
    }

    # Detect dataset (VIIRS/MODIS) based on confidence values
    unique_conf <- unique(firms_sf$confidence)
    message("Unique confidence values in dataset before filtering: ", paste(unique_conf, collapse = ", "))

    if (all(unique_conf %in% c("n", "l", "h"))) {
      detected_dataset <- "MODIS"
    } else {
      detected_dataset <- "VIIRS"
    }
    message("Detected dataset: ", detected_dataset)

    # Confidence Filtering
    if (!is.null(confidence_level)) {
      if (detected_dataset == "MODIS") {
        message("Applying MODIS confidence filtering...")
        firms_sf <- firms_sf %>% filter(confidence %in% confidence_level)
      } else if (detected_dataset == "VIIRS") {
        message("Applying VIIRS confidence filtering...")

        # Convert confidence column to numeric, removing NA values
        firms_sf$confidence <- suppressWarnings(as.numeric(firms_sf$confidence))

        # Debug: Check for NAs after conversion
        if (any(is.na(firms_sf$confidence))) {
          message("Warning: Some confidence values could not be converted to numeric.")
        }

        # Convert numeric confidence into categories
        firms_sf <- firms_sf %>%
          mutate(confidence_category = case_when(
            confidence <= 33 ~ "l",
            confidence > 33 & confidence <= 66 ~ "n",
            confidence > 66 ~ "h",
            TRUE ~ NA_character_
          )) %>%
          filter(!is.na(confidence_category) & confidence_category %in% confidence_level)
      }
    }

    message("Data successfully retrieved for this chunk!")
    return(firms_sf)

  }, error = function(e) {
    message("Error fetching FIRMS data: ", e$message)
    return(NULL)
  })
}
