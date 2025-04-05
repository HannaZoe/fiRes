#' Classify fire points based on OpenStreetMap land cover types
#'
#' This function uses the Overpass API to query OpenStreetMap (OSM) for land cover features.
#' Applies user-choosen land use types to FIRMS dataset.
#' Fire points are spatially buffered and classified based on intersection with OSM features.
#'
#'
#' @param firms_sf An `sf` object with fire locations (typically from `fetch_firms()`).
#' @param feature_types Character vector of OSM feature types to classify by. Options include:
#' `"natural"`, `"agriculture"`, `"urban"`, `"industrial"`, `"military"`, `"waste"`, `"parks"`, `"airport"`.
#' Fire points not matching any selected OSM category will be assigned the type 'unknown'
#' @param priority Optional character vector to define the order in which overlapping OSM categories are assigned.
#' Must be a subset of `feature_types`. If not provided, the order of `feature_types` is used.
#' @param must_be_in Logical. If `TRUE`, only fires matching one of the selected feature types are returned.
#' If `FALSE`, only unmatched fires are returned. Default is `TRUE`.
#' @param dataset Character. Must be one of `"VIIRS_SNPP_NRT"` or `"MODIS_NRT"`. Used to determine buffer distance (375m or 1000m).
#' @param return_osm Logical. If `TRUE`, returns a list with the classified fire points and the OSM polygons used. Default is `FALSE`.
#'
#' @return An `sf` object with a `fire_type` column, or a list containing `$firms` and `$osm` if `return_osm = TRUE`.
#'
#' @importFrom sf st_crs st_transform st_buffer st_union st_make_valid st_cast st_as_sf st_bbox st_intersects st_centroid st_coordinates
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom dplyr bind_rows
#' @export

fetch_osm <- function(firms_sf,
                      feature_types = c("natural", "agriculture", "urban", "industrial", "military", "waste", "parks", "airport"),
                      priority = NULL,
                      must_be_in = TRUE,
                      dataset = c("VIIRS_SNPP_NRT", "MODIS_NRT"),
                      return_osm = FALSE) {

  # --- Argument checks ---
  dataset <- match.arg(dataset)
  feature_types <- match.arg(feature_types, several.ok = TRUE)

  if (is.null(firms_sf) || !inherits(firms_sf, "sf") || nrow(firms_sf) == 0) {
    stop("Input 'firms_sf' must be a non-empty sf object.")
  }

  if (is.null(priority)) {
    priority <- feature_types
  } else if (!all(priority %in% feature_types)) {
    stop("All elements in 'priority' must be included in 'feature_types'.")
  }

  # --- Define dataset-based buffer distance ---
  buffer_dist <- if (dataset == "VIIRS_SNPP_NRT") 375 else 1000

  # --- Define helper to get best UTM CRS ---
  get_utm_crs <- function(sf_obj) {
    lon <- sf::st_coordinates(sf::st_centroid(sf::st_union(sf_obj)))[, 1]
    zone <- floor((lon + 180) / 6) + 1
    epsg <- 32600 + zone  # assumes northern hemisphere
    return(sf::st_crs(epsg))
  }

  # --- Reproject and buffer in meters ---
  utm_crs <- get_utm_crs(firms_sf)
  firms_proj <- sf::st_transform(firms_sf, crs = utm_crs)
  firms_buffered <- sf::st_buffer(firms_proj, dist = buffer_dist)
  firms_buffered <- sf::st_transform(firms_buffered, crs = 4326)

  # --- Ensure input data is WGS84 for querying ---
  if (is.na(sf::st_crs(firms_sf)$epsg) || sf::st_crs(firms_sf)$epsg != 4326) {
    firms_sf <- sf::st_transform(firms_sf, crs = 4326)
  }

  # --- OSM filter definitions ---
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

  # --- Build bounding box ---
  full_bbox <- sf::st_bbox(sf::st_union(firms_buffered))

  # --- Query OSM features ---
  all_osm <- list()
  for (feature_type in feature_types) {
    selected_filters <- osm_filters[[feature_type]]
    for (key in names(selected_filters)) {
      value <- selected_filters[[key]]
      tryCatch({
        query <- opq(bbox = full_bbox, timeout = 120) %>%
          add_osm_feature(key = key, value = value) %>%
          osmdata_sf()

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
          osm_data$feature_type <- feature_type
          all_osm[[length(all_osm) + 1]] <- osm_data
          message("Fetched ", nrow(osm_data), " features for type ", feature_type)
        }
      }, error = function(e) {
        message("Error fetching OSM data for ", feature_type, ": ", e$message)
      })
    }
  }

  # --- Combine OSM features ---
  if (length(all_osm) > 0) {
    all_osm <- dplyr::bind_rows(all_osm)
  } else {
    all_osm <- NULL
  }

  # --- Classify fires ---
  if (!is.null(all_osm) && nrow(all_osm) > 0) {
    all_osm <- sf::st_make_valid(all_osm)
    firms_sf$fire_type <- "unknown"

    for (feature in priority) {
      sub_osm <- all_osm[all_osm$feature_type == feature, ]
      if (nrow(sub_osm) > 0) {
        intersects <- sf::st_intersects(firms_buffered, sub_osm, sparse = TRUE)
        matches <- lengths(intersects) > 0 & firms_sf$fire_type == "unknown"
        firms_sf$fire_type[matches] <- feature
      }
    }

    if (must_be_in) {
      firms_sf <- firms_sf[firms_sf$fire_type %in% feature_types, ]
    } else {
      firms_sf <- firms_sf[!firms_sf$fire_type %in% feature_types, ]
    }

    firms_sf <- sf::st_set_geometry(firms_sf, sf::st_geometry(sf::st_cast(firms_sf, "POINT")))

    message("Fires classified based on OSM features.")
    return(if (return_osm) list(firms = firms_sf, osm = all_osm) else firms_sf)
  } else {
    message("No relevant OSM features found; classifying all fires as 'unknown'.")
    firms_sf$fire_type <- "unknown"
    return(firms_sf)
  }
}
