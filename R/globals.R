# Silence CMD check notes for NSE variables used in dplyr and ggplot2
utils::globalVariables(c(
  "confidence", "confidence_category", "cell_id", "fire_season", "fire_type", "%"
))
