#' Classify fire points based on OpenStreetMap land cover types
#'
#' This function uses OpenStreetMap (OSM) data to classify fire detections (e.g., from FIRMS) into user-defined
#' land cover categories (e.g., natural, urban, industrial). Each fire point is buffered and grouped into clusters,
#' then OSM is queried for relevant land use features around those clusters. Fires are then classified based on
#' whether they intersect with the queried features.
#'
#' @param firms_sf An `sf` object with fire locations (point geometry, typically from `fetch_firms()`).
#' @param feature_types Character vector of OSM feature categories to classify by.
#' Options are `"natural"`, `"agriculture"`, `"urban"`, `"industrial"`, `"military"`, `"waste"`, `"parks"`, `"airport"`.
#' @param must_be_in Logical. If `TRUE` (default), only fires matching one of the selected types are returned.
#' If `FALSE`, fires not matching any selected feature types are returned.
#' @param return_osm Logical. If `TRUE`, the function returns both the classified fires and the queried OSM polygons
#' as a list. Default is `FALSE`.
#'
#' @return Either a filtered `sf` object of fire points with a new `fire_type` column, or a list with `firms` and `osm` if `return_osm = TRUE`.
#'
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom sf st_crs st_transform st_buffer st_union st_make_valid st_cast st_as_sf st_bbox st_intersects
#' @importFrom dplyr bind_rows
#' @export


fetch_osm <- function(firms_sf,
                      feature_types = c("natural", "agriculture", "urban", "industrial", "military", "waste", "parks", "airport"),
                      must_be_in = TRUE,
                      return_osm = FALSE) {

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
  buffer_dist <- ifelse(nrow(firms_sf) > 0 &&
                          "confidence" %in% colnames(firms_sf) && all(firms_sf$confidence <= 100),
                        187.5, 500)
  firms_buffered <- st_buffer(firms_sf, dist = buffer_dist)

  # Step 2: Group nearby fires (via spatial union of overlapping buffers)
  fire_groups <- st_make_valid(st_union(firms_buffered)) %>%
    st_cast("POLYGON") %>%
    st_as_sf()

  # Step 3: Query OSM
  all_osm <- list()

  for (i in seq_len(nrow(fire_groups))) {
    fire_bbox <- st_bbox(fire_groups[i, ])

    for (feature_type in feature_types) {
      selected_filters <- osm_filters[[feature_type]]

      for (key in names(selected_filters)) {
        value <- selected_filters[[key]]

        tryCatch({
          query <- opq(bbox = fire_bbox, timeout = 120) %>%
            add_osm_feature(key = key, value = value) %>%
            osmdata_sf()

          osm_data <- if (!is.null(query$osm_polygons) && nrow(query$osm_polygons) > 0) {
            query$osm_polygons
          } else if (!is.null(query$osm_multipolygons) && nrow(query$osm_multipolygons) > 0) {
            query$osm_multipolygons
          } else {
            NULL
          }

          if (!is.null(osm_data)) {
            osm_data$feature_type <- feature_type
            all_osm[[length(all_osm) + 1]] <- osm_data
          }

        }, error = function(e) {
          message("Error fetching OSM data for fire cluster ", i, ": ", e$message)
        })
      }
    }
  }

  # Step 4: Classify Fires Based on OSM Intersections
  if (length(all_osm) > 0) {
    osm_polygons <- do.call(dplyr::bind_rows, all_osm)
    osm_polygons <- st_make_valid(osm_polygons)

    firms_sf$fire_type <- "unknown"

    for (feature in feature_types) {
      sub_osm <- osm_polygons[osm_polygons$feature_type == feature, ]
      if (nrow(sub_osm) > 0) {
        intersects <- st_intersects(firms_buffered, sub_osm, sparse = FALSE)
        matches <- apply(intersects, 1, any)
        firms_sf$fire_type[matches] <- feature
      }
    }

    # Step 5: Inclusion or Exclusion
    if (must_be_in) {
      firms_sf <- firms_sf[firms_sf$fire_type %in% feature_types, ]
    } else {
      firms_sf <- firms_sf[!firms_sf$fire_type %in% feature_types, ]
    }

    message("Fires classified based on OSM features.")
    return(if (return_osm) list(firms = firms_sf, osm = osm_polygons) else firms_sf)

  } else {
    message("No relevant OSM features found; classifying fires as 'unknown'.")
    firms_sf$fire_type <- "unknown"
    return(firms_sf)
  }
}

