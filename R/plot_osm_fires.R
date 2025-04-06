#' Plot classified and uncategorized FIRMS fire detections
#'
#' This function visualizes categorized fire detections (from `fetch_osm`) and optionally any uncategorized ones.
#' Points are color-coded by their `fire_type`. You can supply a custom background map.
#'
#' @param firms_list A list of `sf` objects, each containing fire detections with a `fire_type` column.
#' @param firms_uncategorized Optional. An `sf` object of uncategorized fires to plot in gray.
#' @param base_map Optional. Either an `sf` object or a file path to a shapefile/GPKG/etc. If NULL, a Natural Earth map is used.
#' @param include_uncategorized Logical. Whether to include uncategorized fires in the plot. Default is TRUE.
#'
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot geom_sf aes scale_color_manual coord_sf labs theme_minimal
#' @importFrom sf st_crs st_transform st_is_empty st_cast st_geometry_type st_geometry read_sf
#' @importFrom rnaturalearth ne_countries
#'
#' @export

plot_osm_fires <- function(firms_list,
                           firms_uncategorized = NULL,
                           base_map = NULL,
                           include_uncategorized = TRUE) {
  # Check input
  if (!is.list(firms_list) || any(!sapply(firms_list, inherits, what = "sf"))) {
    stop("Error: firms_list must be a list of sf objects.")
  }

  # Combine and clean classified fires
  firms_list <- lapply(firms_list, function(f) {
    f <- f[!sf::st_is_empty(f), ]
    if (any(sf::st_geometry_type(f) != "POINT")) {
      f <- sf::st_cast(f, "MULTIPOINT", warn = FALSE)
      f <- sf::st_cast(f, "POINT", warn = FALSE)
    }
    if (!"fire_type" %in% names(f)) f$fire_type <- "unknown"
    return(f)
  })
  all_firms_sf <- do.call(rbind, firms_list)

  if (nrow(all_firms_sf) == 0) {
    stop("No fire points found - cannot plot.")
  }

  # Handle base map input
  if (is.null(base_map)) {
    message("No base_map provided. Using Natural Earth country outlines.")
    base_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  } else if (is.character(base_map)) {
    if (!file.exists(base_map)) {
      stop("The base_map path does not exist: ", base_map)
    }
    message("Reading base_map from file: ", base_map)
    base_map <- sf::read_sf(base_map)
  }

  # Ensure CRS match
  if (!is.null(sf::st_crs(base_map)) && sf::st_crs(base_map) != sf::st_crs(all_firms_sf)) {
    base_map <- sf::st_transform(base_map, sf::st_crs(all_firms_sf))
  }

  # color scheme
  fire_type_colors <- c(
    "natural"     = "forestgreen",
    "agriculture" = "goldenrod",
    "urban"       = "steelblue",
    "industrial"  = "firebrick",
    "military"    = "purple",
    "waste"       = "saddlebrown",
    "parks"       = "darkolivegreen",
    "airport"     = "gray50",
    "unknown"     = "black"
  )

  # Build plot
  p <- ggplot() +
    geom_sf(data = base_map, fill = "gray90", color = "black", lwd = 0.3) +
    geom_sf(
      data = all_firms_sf,
      aes(
        color = fire_type,
        size = ifelse(fire_type == "unknown", 1, 2),
        alpha = ifelse(fire_type == "unknown", 0.4, 0.7)
      )
    ) +
    scale_color_manual(values = fire_type_colors, name = "Fire Classification") +
    coord_sf() +
    labs(
      title = "Classified FIRMS Fire Detections",
      x = "Longitude", y = "Latitude",
      subtitle = if (include_uncategorized) "Uncategorized fires shown as small black dots" else NULL
    ) +
    theme_minimal()

  return(p)
}

