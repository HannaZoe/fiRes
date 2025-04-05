#' Classify fires by custom-defined fire seasons
#'
#' Adds a `fire_season` column based on user-defined month groupings.
#' Automatically converts `acq_date` to Date if necessary, and assigns fire season labels accordingly.
#'
#' @param firms_sf An `sf` object with an `acq_date` column (character or Date).
#' @param fire_seasons A named list, where each element is a numeric vector of month values (1–12).
#' The names will be used as fire season labels.
#'
#' @return The original `sf` object with an added `fire_season` column (as factor).
#'
#' @importFrom lubridate month
#'
#' @export

fire_seasons <- function(firms_sf, fire_seasons) {
  if (!"acq_date" %in% colnames(firms_sf)) {
    stop("Input must contain an 'acq_date' column.")
  }

  # Validate fire_seasons input
  if (!is.list(fire_seasons) || is.null(names(fire_seasons)) || any(names(fire_seasons) == "")) {
    stop("fire_seasons must be a named list with each element being a vector of month numbers (1–12).")
  }

  # Ensure acq_date is Date
  if (!inherits(firms_sf$acq_date, "Date")) {
    firms_sf$acq_date <- as.Date(firms_sf$acq_date)
    if (anyNA(firms_sf$acq_date)) {
      stop("Some 'acq_date' values could not be converted to Date.")
    }
  }

  # Assign fire season based on month
  fire_months <- lubridate::month(firms_sf$acq_date)
  season_labels <- rep(NA_character_, length(fire_months))

  for (season in names(fire_seasons)) {
    months <- fire_seasons[[season]]
    season_labels[fire_months %in% months] <- season
  }

  firms_sf$fire_season <- factor(season_labels, levels = names(fire_seasons))
  return(firms_sf)
}
