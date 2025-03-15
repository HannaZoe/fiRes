# Welcome to fiRes, an R package that helps you to access and process NASAs FIRMS dataset more easily

# Author Hanna Schulten
# Last updated: 15.03.2025

# Library Collection
library(sf)  # Spatial handling
library(dplyr)  # Filtering

# 1. Function: Fetching and pre-filtering FIRMS Data

# Variables Overview:
# "api_key" = Can be recived via NASA https://firms.modaps.eosdis.nasa.gov/api/map_key
# "region" = shapefiles as input that define the aoi
# "start_date" = Beginning date of intrest
# "end_date" last date of interest
# Noteworthy -> 10 day maximum per request sent
# "dataset" = User can define whether they want VIIRS or MODIS dataset
# "confidence_level" = Optional filter that allows to define confidence of interest

fetch_firms_data <- function(api_key, region_sf, start_date, end_date,
                             dataset = c("VIIRS_SNPP_NRT", "MODIS_NRT"),
                             confidence_level = NULL) {
  dataset <- match.arg(dataset)  # Ensure valid dataset input

  # Ensure region is an sf object
  if (!inherits(region_sf, "sf")) {
    stop("Error: The input region must be a shapefile in sf format.")
  }

  # Ensure region is in WGS 84 (EPSG:4326)
  if (st_crs(region_sf)$epsg != 4326) {
    message("Reprojecting input shapefile to WGS 84 (EPSG:4326)...")
    region_sf <- st_transform(region_sf, crs = 4326)
  }

  # Extract bounding box for API request, API wants that as input
  bbox <- st_bbox(region_sf)
  bbox_str <- paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep = ",")

  # Validate date range
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  day_range <- as.numeric(difftime(end_date, start_date, units = "days")) + 1

  if (day_range < 1 || day_range > 10) {
    stop("Invalid date range. NASA FIRMS API only allows a maximum of 10 days.")
  }

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
      message("No fire data available for the given date range and location.")
      return(NULL)
    }

    # Convert fire data to an sf object (-> point data)
    firms_sf <- st_as_sf(firms_data, coords = c("longitude", "latitude"), crs = 4326)

    # Filter to include only points inside the shapefile
    firms_sf <- firms_sf[st_within(firms_sf, region_sf, sparse = FALSE), ]

    if (nrow(firms_sf) == 0) {
      message("No fires found inside the provided region.")
      return(NULL)
    }

    # Detect dataset (VIIRS/MODIS) based on vales in confidence column
    unique_conf <- unique(firms_sf$confidence)
    message("Unique confidence values in dataset before filtering: ", paste(unique_conf, collapse = ", "))

    # Check if confidence values match MODIS ("n", "l", "h")**
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
            TRUE ~ NA_character_  # Handle any unexpected values
          )) %>%
          filter(!is.na(confidence_category) & confidence_category %in% confidence_level)
      }
    }

    message("Data successfully retrieved!")
    return(firms_sf)

  }, error = function(e) {
    message("Error fetching FIRMS data: ", e$message)
    return(NULL)
  })
}













