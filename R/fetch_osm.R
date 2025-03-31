#' Classify fire points based on OpenStreetMap land cover types (Parallel)
#'
#' This function uses the Overpass API to query OpenStreetMap (OSM) for land cover features
#' and classifies fire detections (e.g., from FIRMS) into user-defined land use types. It runs
#' the feature type queries in parallel to speed up processing.
#'
#' @param firms_sf An `sf` object with fire locations (typically from `fetch_firms()`).
#' @param feature_types Character vector of OSM feature types to classify by. Options include:
#' `"natural"`, `"agriculture"`, `"urban"`, `"industrial"`, `"military"`, `"waste"`, `"parks"`, `"airport"`.
#' @param must_be_in Logical. If `TRUE` (default), only fires that intersect with selected OSM features are returned.
#' If `FALSE`, fires not matching any selected type are returned.
#' @param return_osm Logical. If `TRUE`, returns a list with classified fires and the OSM polygons. Default is `FALSE`.
#'
#' @return An `sf` object with classified fire points (with a new `fire_type` column), or a list if `return_osm = TRUE`.
#'
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom sf st_crs st_transform st_buffer st_union st_make_valid st_cast st_as_sf st_bbox st_intersects
#' @importFrom dplyr bind_rows
#' @importFrom future.apply future_lapply
#' @export

fetch_osm <- function(firms_sf,
                      feature_types = c("natural", "agriculture", "urban", "industrial", "military", "waste", "parks", "airport"),
                      must_be_in = TRUE,
                      return_osm = FALSE) {

  feature_types <- match.arg(feature_types, several.ok = TRUE)

  if (is.null(firms_sf) || !inherits(firms_sf, "sf") || nrow(firms_sf) == 0) {
    stop("Input 'firms_sf' must be a non-empty sf object.")
  }
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

  # Ensure CRS is WGS84
  if (is.na(sf::st_crs(firms_sf)$epsg) || sf::st_crs(firms_sf)$epsg != 4326) {
    message("Reprojecting FIRMS data to WGS 84...")
    firms_sf <- sf::st_transform(firms_sf, crs = 4326)
  }

  # Step 1: Buffer fire points
  buffer_dist <- ifelse(nrow(firms_sf) > 0 &&
                          "confidence" %in% colnames(firms_sf) && all(firms_sf$confidence <= 100),
                        187.5, 500)
  firms_buffered <- sf::st_buffer(firms_sf, dist = buffer_dist)

  # Step 2: Calculate a single bounding box over all buffered fires
  full_bbox <- sf::st_bbox(sf::st_union(firms_buffered))

  # Step 3: Sequential OSM querying
  all_osm <- list()

  for (feature_type in feature_types) {
    selected_filters <- osm_filters[[feature_type]]

    for (key in names(selected_filters)) {
      value <- selected_filters[[key]]

      tryCatch({
        query <- opq(bbox = full_bbox, timeout = 120) %>%
          add_osm_feature(key = key, value = value) %>%
          osmdata_sf()

        # Safely combine polygons and multipolygons
        osm_polys <- query$osm_polygons
        osm_multi <- query$osm_multipolygons

        if (!is.null(osm_polys) && nrow(osm_polys) > 0) {
          osm_polys <- sf::st_make_valid(osm_polys)
        } else {
          osm_polys <- NULL
        }

        if (!is.null(osm_multi) && nrow(osm_multi) > 0) {
          osm_multi <- sf::st_make_valid(osm_multi)
        } else {
          osm_multi <- NULL
        }

        if (!is.null(osm_polys) || !is.null(osm_multi)) {
          osm_data <- dplyr::bind_rows(osm_polys, osm_multi)

          if (!is.null(osm_data) && nrow(osm_data) > 0) {
            osm_data$feature_type <- feature_type
            all_osm[[length(all_osm) + 1]] <- osm_data
            message("Fetched ", nrow(osm_data), " features for type ", feature_type, " (key: ", key, ")")
          }
        }
      }, error = function(e) {
        message("Error fetching OSM data for feature type ", feature_type, " (key: ", key, "): ", e$message)
      })
    }
  }

  # Combine all OSM results
  if (length(all_osm) > 0) {
    all_osm <- dplyr::bind_rows(all_osm)
  } else {
    all_osm <- NULL
  }

  # Step 4: Classification
  if (!is.null(all_osm) && nrow(all_osm) > 0) {
    all_osm <- sf::st_make_valid(all_osm)
    firms_sf$fire_type <- "unknown"

    for (feature in feature_types) {
      sub_osm <- all_osm[all_osm$feature_type == feature, ]
      if (nrow(sub_osm) > 0) {
        intersects <- sf::st_intersects(firms_buffered, sub_osm, sparse = TRUE)
        matches <- lengths(intersects) > 0
        firms_sf$fire_type[matches] <- feature
      }
    }

    if (must_be_in) {
      firms_sf <- firms_sf[firms_sf$fire_type %in% feature_types, ]
    } else {
      firms_sf <- firms_sf[!firms_sf$fire_type %in% feature_types, ]
    }

    # Restore original point geometries (remove buffer)
    firms_sf <- sf::st_set_geometry(firms_sf, sf::st_geometry(sf::st_cast(firms_sf, "POINT")))

    message("Fires classified based on OSM features.")
    return(if (return_osm) list(firms = firms_sf, osm = all_osm) else firms_sf)
  } else {
    message("No relevant OSM features found; classifying fires as 'unknown'.")
    firms_sf$fire_type <- "unknown"
    return(firms_sf)
  }
}
