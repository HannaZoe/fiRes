#' Helper function to fetch a single chunk of FIRMS fire data
#'
#' This internal function (helper) retrieves a chunk (10 days) of FIRMS fire data from NASA's FIRMS API.
#' Data is always retrived for a specific bounding box and time range.
#' Optional filtering of fire points based on confidence levels can be applied.
#'
#' @param api_key Character. Your NASA API key.
#' @param region_sf An `sf` object defining the region of interest. Must be in WGS 84.
#' @param start_date Start date of the chunk (in "YYYY-MM-DD" format).
#' @param end_date End date of the chunk (in "YYYY-MM-DD" format).
#' @param dataset Character. Either `"VIIRS_SNPP_NRT"` or `"MODIS_NRT"`.
#' @param confidence_level Optional. A character or numeric vector defining the confidence levels to filter.
#' For VIIRS, use `"l"`, `"n"`, `"h"`. For MODIS, numeric values will be translated to those categories.
#' @param bbox_str A comma-separated string of the bounding box coordinates (xmin, ymin, xmax, ymax).
#'
#' @return An `sf` object with fire detections that fall inside the given region and match the confidence filter, or `NULL` if no data was found.
#'
#' @importFrom sf st_as_sf st_within
#' @importFrom dplyr filter mutate case_when
#' @importFrom utils read.csv download.file
#'
#' @keywords internal
#' @noRd

fetch_firms_chunk <- function(api_key, region_sf, start_date, end_date, dataset, confidence_level, bbox_str) {
  day_range <- as.numeric(difftime(end_date, start_date, units = "days")) + 1

  # Build FIRMS URL
  base_url <- "https://firms.modaps.eosdis.nasa.gov/api/csv/"
  url <- paste0(base_url, api_key, "/", dataset, "/", start_date, "/", end_date, "/", bbox_str, "/")
  temp_file <- tempfile(fileext = ".csv")

  # Supress rather long download message that otherwise appears in Console
  suppressMessages(
    suppressWarnings(
      download.file(url, temp_file, mode = "wb", quiet = TRUE)
    )
  )

  tryCatch({
    # Another supression of Console messages
    firms_data <- suppressWarnings(read.csv(temp_file, stringsAsFactors = FALSE))

    if (nrow(firms_data) == 0) {

      message("No fire data available for this chunk.")
      return(NULL)
    }

    # Convert to sf
    firms_sf <- sf::st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)

    # Spatial filtering
    firms_sf <- firms_sf[sf::st_within(firms_sf, region_sf, sparse = FALSE), ]
    if (nrow(firms_sf) == 0) {
      return(NULL)
    }

    # Detects whether dataset is MODIS or VIIRS based on unique column names
    is_viirs <- "bright_ti4" %in% names(firms_data)
    is_modis <- "brightness" %in% names(firms_data)

    if (is_viirs) {
      detected_dataset <- "VIIRS"
    } else if (is_modis) {
      detected_dataset <- "MODIS"
    } else if (all(unique(firms_sf$confidence) %in% c("l", "n", "h"))) {
      detected_dataset <- "VIIRS"
    } else {
      detected_dataset <- "MODIS"
    }

    # Application of confidence filtering
    if (!is.null(confidence_level)) {
      confidence_level <- as.character(unlist(confidence_level))

      if (detected_dataset == "VIIRS") {
        firms_sf <- dplyr::filter(firms_sf, confidence %in% confidence_level)

      } else if (detected_dataset == "MODIS") {
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
    message("Error fetching FIRMS data: ", e$message)
    return(NULL)
  })
}
