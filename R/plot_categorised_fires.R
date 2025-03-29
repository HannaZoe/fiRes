#' Plot categorized FIRMS fire detections on a map
#'
#' This function visualizes categorized FIRMS fire points (e.g., from `fetch_osm()`) on a base map using `ggplot2`.
#' Fires are colored by their `fire_type` classification. Optional uncategorized fire points can be shown as gray circles.
#'
#' @param firms_list A list of `sf` point objects. Each entry should represent a set of FIRMS fire detections with a `fire_type` column.
#' @param firms_uncategorized Optional. An `sf` object with uncategorized fire detections (e.g., raw FIRMS data before classification).
#' These will be shown as gray background circles on the map.
#' @param base_map Optional. An `sf` object to use as the base map. If not provided, a country-level map will be fetched from `rnaturalearth` and cropped to fire extent.
#'
#' @return A `ggplot` object displaying the fire detections on a map with color-coded classifications.
#'
#' @importFrom sf st_is_empty st_cast st_geometry st_union st_bbox st_coordinates st_drop_geometry st_intersects
#' @importFrom ggplot2 ggplot geom_sf geom_point aes scale_color_manual coord_sf labs theme_minimal
#' @importFrom dplyr bind_rows
#' @importFrom rnaturalearth ne_countries
#' @export
plot_osm_fires <- function(firms_list, firms_uncategorized = NULL, base_map = NULL) {
  # Ensure firms_list is a list of sf objects
  if (!is.list(firms_list) || any(!sapply(firms_list, inherits, what = "sf"))) {
    stop("Error: firms_list must be a list of sf objects.")
  }

  # Ensure all datasets have the same attribute columns and are POINT geometries
  firms_list <- lapply(firms_list, function(f) {
    f <- f[!st_is_empty(f), ]  # Remove empty geometries
    f <- st_cast(f, "POINT")  # Ensure POINT geometries
    if (!"fire_type" %in% colnames(f)) f$fire_type <- "unknown"  # Add fire_type if missing
    st_geometry(f) <- "geometry"  # Explicitly define geometry column
    return(f)
  })

  # Combine all categorized FIRMS data
  all_firms_sf <- do.call(rbind, firms_list)

  # Ensure fire_type column is a factor
  all_firms_sf$fire_type <- as.factor(all_firms_sf$fire_type)

  # Extract bounding box for auto-zooming
  fire_bbox <- st_bbox(all_firms_sf)
  if (!is.null(firms_uncategorized)) {
    firms_uncategorized <- firms_uncategorized[!st_is_empty(firms_uncategorized), ]  # Remove empty geometries
    firms_uncategorized <- st_cast(firms_uncategorized, "POINT")  # Ensure POINT geometries
    fire_bbox <- st_bbox(st_union(all_firms_sf, firms_uncategorized))
  }

  # Use a base map if provided, otherwise fetch a world map cropped to fire locations
  if (is.null(base_map)) {
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    base_map <- world[st_intersects(world, all_firms_sf, sparse = FALSE), ]
  }

  # Define colors for fire classification
  fire_colors <- c("natural" = "green", "agriculture" = "yellow", "urban" = "blue",
                   "industrial" = "red", "military" = "purple", "waste" = "brown",
                   "parks" = "darkgreen", "airport" = "gray", "unknown" = "black")

  # Convert sf objects to data frames for ggplot (prevents ggplot2 warnings)
  firms_df <- st_drop_geometry(all_firms_sf)
  if (!is.null(firms_uncategorized)) {
    firms_uncategorized_df <- st_drop_geometry(firms_uncategorized)
  }

  # Create the plot
  p <- ggplot() +
    geom_sf(data = base_map, fill = "gray90", color = "black", lwd = 0.3) +  # Base map
    geom_point(data = firms_df,
               aes(x = st_coordinates(all_firms_sf)[,1],
                   y = st_coordinates(all_firms_sf)[,2],
                   color = fire_type), size = 2, alpha = 0.7) +  # Categorized fires
    scale_color_manual(values = fire_colors, name = "Fire Classification") +
    coord_sf(xlim = c(fire_bbox$xmin, fire_bbox$xmax), ylim = c(fire_bbox$ymin, fire_bbox$ymax), expand = TRUE) +
    labs(title = "Classified FIRMS Fire Detections",
         x = "Longitude", y = "Latitude") +
    theme_minimal()

  # Add raw FIRMS data as circles if provided
  if (!is.null(firms_uncategorized)) {
    p <- p + geom_point(data = firms_uncategorized_df,
                        aes(x = st_coordinates(firms_uncategorized)[,1],
                            y = st_coordinates(firms_uncategorized)[,2]),
                        shape = 21, fill = "gray", color = "black", size = 3, alpha = 0.5) +
      labs(subtitle = "Uncategorized FIRMS fires shown as gray circles")
  }

  return(p)
}





