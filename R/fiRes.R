# Welcome to fiRes, an R package that helps you to access and process NASAs FIRMS dataset more easily

# Author Hanna Schulten
# Last updated: 16.03.2025


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

  # ✅ If the request is ≤ 10 days, fetch as usual
  if (total_days <= 10) {
    return(fetch_firms_chunk(api_key, region_sf, start_date, end_date, dataset, confidence_level, bbox_str))
  }

  # ✅ If the request is > 10 days, break it into chunks
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


# Function 1b: Helper function called in Function 1

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


# Function 2: Fetching OSM Data

fetch_osm <- function(firms_sf, feature_types = c("natural", "agriculture", "urban", "industrial", "military", "waste", "parks", "airport"), must_be_in = TRUE) {
  feature_types <- match.arg(feature_types, several.ok = TRUE)

  # Define OSM filters
  osm_filters <- list(
    "natural" = list("landuse" = c("forest", "meadow", "grass", "heath", "wetland", "nature_reserve"),
                     "natural" = c("wood", "grassland", "heath", "scrub")),
    "agriculture" = list("landuse" = c("farmland", "farmyard", "orchard", "vineyard", "plant_nursery", "greenhouse_horticulture"),
                         "animal_keeping" = "yes"),
    "urban" = list("landuse" = c("residential", "commercial", "retail", "urban")),
    "industrial" = list("landuse" = "industrial",
                        "power" = "plant",
                        "man_made" = c("works", "wastewater_plant", "chimney")),
    "military" = list("landuse" = "military"),
    "waste" = list("landuse" = "landfill",
                   "man_made" = "wastewater_plant"),
    "parks" = list("leisure" = c("park", "recreation_ground", "nature_reserve")),
    "airport" = list("aeroway" = "aerodrome")
  )

  # Ensure CRS is WGS 84
  if (st_crs(firms_sf)$epsg != 4326) {
    message("Reprojecting FIRMS data to WGS 84...")
    firms_sf <- st_transform(firms_sf, crs = 4326)
  }

  # 🔥 **Step 1: Buffer Fire Locations Slightly**
  firms_buffered <- st_buffer(firms_sf, dist = ifelse(nrow(firms_sf) > 0 && "confidence" %in% colnames(firms_sf) & all(firms_sf$confidence <= 100), 187.5, 500))

  # 🔥 **Step 2: Query OSM for Each Feature Type in Small Chunks**
  message("Fetching OSM data near fire locations for: ", paste(feature_types, collapse = ", "))
  osm_polygons <- NULL

  for (feature_type in feature_types) {
    selected_filters <- osm_filters[[feature_type]]

    for (key in names(selected_filters)) {
      value <- selected_filters[[key]]

      tryCatch({
        # 🔥 **Reduce Query Area to Avoid Overpass API Memory Limits**
        sub_bbox <- st_bbox(st_sample(firms_buffered, size = min(500, nrow(firms_buffered))))  # Sample up to 500 points

        osm_query <- opq(bbox = sub_bbox, timeout = 180) %>%
          add_osm_feature(key = key, value = value) %>%
          osmdata_sf()

        # Store only polygons
        if (!is.null(osm_query$osm_polygons) && nrow(osm_query$osm_polygons) > 0) {
          osm_polygons <- if (is.null(osm_polygons)) osm_query$osm_polygons else rbind(osm_polygons, osm_query$osm_polygons)
        } else if (!is.null(osm_query$osm_multipolygons) && nrow(osm_query$osm_multipolygons) > 0) {
          osm_polygons <- if (is.null(osm_polygons)) osm_query$osm_multipolygons else rbind(osm_polygons, osm_query$osm_multipolygons)
        }
      }, error = function(e) {
        message("Error fetching OSM data for ", feature_type, ": ", e$message)
      })
    }
  }

  # 🔥 **Step 3: Classify Fires Based on OSM Intersections**
  if (!is.null(osm_polygons)) {
    intersects_matrix <- st_intersects(firms_buffered, osm_polygons, sparse = FALSE)
    fire_labels <- rep("unknown", nrow(firms_sf))

    for (i in seq_along(feature_types)) {
      feature <- feature_types[i]
      feature_intersects <- apply(intersects_matrix, 1, function(row) any(row))
      fire_labels[feature_intersects] <- feature
    }

    firms_sf$fire_type <- fire_labels

    # 🔥 **Step 4: Apply Inclusion or Exclusion Filter**
    if (must_be_in) {
      firms_sf <- firms_sf[firms_sf$fire_type %in% feature_types, ]
    } else {
      firms_sf <- firms_sf[!firms_sf$fire_type %in% feature_types, ]
    }

    message("✅ Fires classified based on OSM features.")
    return(firms_sf)
  } else {
    message("❌ No relevant OSM features found; classifying fires as 'unknown'.")
    firms_sf$fire_type <- "unknown"
    return(firms_sf)
  }
}















