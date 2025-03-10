# Welcome to fiRes, an R package that helps you to access and process NASAs FIRMS dataset more easily

# Author Hanna Schulten
# Last updated: 09.03.2025

# 1. Fetch FIRMS Data (Google Earth Engine & API)
# This function retrieves FIRMS fire data from either Google Earth Engine (GEE) or the NASA FIRMS API and clips it to the specified region

# @param start_date Start date for fire data retrieval (YYYY-MM-DD).
# @param end_date End date for fire data retrieval (YYYY-MM-DD).
# @param region Geographic boundary (sf object or bounding box) to clip the fire data.
# @param method Retrieval method: "gee" (Google Earth Engine) or "api" (NASA FIRMS API).
# @return A spatial data frame (sf object) of fire detections clipped to the region.
# @export

fetch_firms_data <- function(start_date, end_date, region, method = "api") {
  if (method == "gee") {
    firms <- ee$ImageCollection("FIRMS")$
      filterDate(start_date, end_date)$
      filterBounds(region)
    firms <- firms$clip(region)
    return(firms)
  } else if (method == "api") {
    url <- paste0("https://firms.modaps.eosdis.nasa.gov/api/v1/global?start=", start_date, "&end=", end_date)
    fire_data <- read.csv(url)
    region_sf <- sf::st_read(region)
    fire_sf <- sf::st_as_sf(fire_data, coords = c("longitude", "latitude"), crs = 4326)
    clipped_data <- sf::st_intersection(fire_sf, region_sf)
    return(clipped_data)
  } else {
    stop("Invalid method. Choose either 'gee' or 'api'.")
  }
}
