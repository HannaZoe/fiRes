#' Classify fires by custom-defined fire seasons
#'
#' Adds a `fire_season` column based on user-defined month groupings.
#'
#' @param firms_sf An sf object with an `acq_date` column.
#' @param fire_seasons A named list, where each element is a vector of month numbers (1–12).
#'
#' @return The input `sf` object with an added `fire_season` column.
#' @export

fire_seasons <- function(firms_sf, fire_seasons) {
  if (!"acq_date" %in% colnames(firms_sf)) {
    stop("Input must have an 'acq_date' column.")
  }

  # Convert to Date (fix!)
  firms_sf$acq_date <- as.Date(firms_sf$acq_date)

  firms_sf$month <- lubridate::month(firms_sf$acq_date)
  firms_sf$fire_season <- NA_character_

  for (season in names(fire_seasons)) {
    months <- fire_seasons[[season]]
    firms_sf$fire_season[firms_sf$month %in% months] <- season
  }

  firms_sf$fire_season <- factor(firms_sf$fire_season, levels = names(fire_seasons))
  return(firms_sf)
}

