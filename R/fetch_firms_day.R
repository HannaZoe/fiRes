#' Helper function to fetch one day of historic FIRMS data
#'
#' @param api_key Character. Your NASA API key.
#' @param region_sf An `sf` object representing the region of interest.
#' @param date A single date in "YYYY-MM-DD" format.
#' @param dataset Character. One of "MODIS_C6", "VIIRS_SNPP", or "VIIRS_NOAA20_SP".
#' @param confidence_level Optional. One or more of "l", "n", "h".
#' @param bbox_str A comma-separated bounding box string (xmin,ymin,xmax,ymax).
#'
#' @return An `sf` object for the given day, or NULL if no fires were found.
#'
#' @importFrom sf st_as_sf st_filter
#' @importFrom dplyr filter mutate case_when
#' @importFrom utils read.csv download.file
#'
#' @keywords internal
fetch_firms_day <- function(api_key, region_sf, date, dataset, confidence_level, bbox_str) {
  date_str <- format(as.Date(date), "%Y-%m-%d")

  url <- paste0(
    "https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
    api_key, "/", dataset, "/", bbox_str, "/1?date=", date_str
  )

  temp_file <- tempfile(fileext = ".csv")
  suppressMessages(suppressWarnings(
    download.file(url, temp_file, mode = "wb", quiet = TRUE)
  ))

  tryCatch({
    firms_data <- suppressWarnings(read.csv(temp_file, stringsAsFactors = FALSE))

    if (nrow(firms_data) == 0 || !all(c("latitude", "longitude") %in% names(firms_data))) {
      return(NULL)
    }

    firms_sf <- sf::st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)
    firms_sf <- sf::st_filter(firms_sf, region_sf)
    if (nrow(firms_sf) == 0) return(NULL)

    # Determine dataset type
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
    message("Error on ", date_str, ": ", e$message)
    return(NULL)
  })
}
