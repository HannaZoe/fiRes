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

  # Step 1: Buffer each fire location
  firms_buffered <- st_buffer(firms_sf, dist = ifelse(nrow(firms_sf) > 0 &&
                                                        "confidence" %in% colnames(firms_sf) & all(firms_sf$confidence <= 100), 187.5, 500))

  # Step 2: Cluster nearby fires to reduce queries
  firms_clustered <- st_union(firms_buffered)
  fire_groups <- st_cast(firms_clustered, "POLYGON")

  # Ensure fire_groups still has a valid geometry column
  if (!inherits(fire_groups, "sf")) {
    fire_groups <- st_as_sf(fire_groups)
  }

  # Step 3: Query OSM for each **fire cluster** instead of each fire
  osm_polygons <- NULL

  for (i in seq_len(length(fire_groups))) {
    fire_bbox <- st_bbox(fire_groups[i])

    for (feature_type in feature_types) {
      selected_filters <- osm_filters[[feature_type]]

      for (key in names(selected_filters)) {
        value <- selected_filters[[key]]

        tryCatch({
          osm_query <- opq(bbox = fire_bbox, timeout = 120) %>%
            add_osm_feature(key = key, value = value) %>%
            osmdata_sf()

          # Extract only valid polygons
          osm_data <- NULL
          if (!is.null(osm_query$osm_polygons) && nrow(osm_query$osm_polygons) > 0) {
            osm_data <- osm_query$osm_polygons
          } else if (!is.null(osm_query$osm_multipolygons) && nrow(osm_query$osm_multipolygons) > 0) {
            osm_data <- osm_query$osm_multipolygons
          }

          # Ensure OSM data has a geometry column before merging
          if (!is.null(osm_data) && inherits(osm_data, "sf")) {
            if (!"geometry" %in% colnames(osm_data)) {
              st_geometry(osm_data) <- "geometry"
            }
            osm_polygons <- if (is.null(osm_polygons)) osm_data else dplyr::bind_rows(osm_polygons, osm_data)
          }
        }, error = function(e) {
          message("Error fetching OSM data for fire cluster ", i, ": ", e$message)
        })
      }
    }
  }

  # Step 4: Classify Fires Based on OSM Intersections
  if (!is.null(osm_polygons) && inherits(osm_polygons, "sf")) {
    intersects_matrix <- st_intersects(firms_buffered, osm_polygons, sparse = FALSE)
    fire_labels <- rep("unknown", nrow(firms_sf))

    for (i in seq_along(feature_types)) {
      feature <- feature_types[i]
      feature_intersects <- apply(intersects_matrix, 1, function(row) any(row))
      fire_labels[feature_intersects] <- feature
    }

    firms_sf$fire_type <- fire_labels

    # Step 5: Apply Inclusion or Exclusion Filter
    if (must_be_in) {
      firms_sf <- firms_sf[firms_sf$fire_type %in% feature_types, ]
    } else {
      firms_sf <- firms_sf[!firms_sf$fire_type %in% feature_types, ]
    }

    message("Fires classified based on OSM features.")
    return(firms_sf)
  } else {
    message("No relevant OSM features found; classifying fires as 'unknown'.")
    firms_sf$fire_type <- "unknown"
    return(firms_sf)
  }
}
