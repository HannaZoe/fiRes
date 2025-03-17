# Welcome to fiRes, an R package that helps you to access and process NASAs FIRMS dataset more easily

# Author Hanna Schulten
# Last updated: 17.03.2025


# Library Collection
library(sf)  # Spatial handling
library(dplyr)  # Filtering
library(osmdata)

# 1. Function: Fetching and pre-filtering FIRMS Data

# Variables Overview:
# "api_key" = Can be recived via NASA https://firms.modaps.eosdis.nasa.gov/api/map_key
# "region" = shapefiles as input that define the aoi
# "start_date" = Beginning date of intrest
# "end_date" last date of interest
# Noteworthy -> 10 day maximum per request sent
# "dataset" = User can define whether they want VIIRS or MODIS dataset
# "confidence_level" = Optional filter that allows to define confidence of interest

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






















