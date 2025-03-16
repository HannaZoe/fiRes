# Load required libraries
library(devtools)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Testing Function 1

# Get Brazil's bounding box
denmark <- ne_countries(scale = "medium", country = "Ghana", returnclass = "sf")


# Example usage
api_key <- "917f143bded8a25852d7f824ca527e13"
start_date <- "2024-05-01"
end_date <- "2024-08-31"

firms_data <- fetch_firms_data(api_key, denmark, start_date, end_date, dataset = "MODIS_NRT", confidence_level = c("h"))

print(head(firms_data))   # See first 6 rows
print(dim(firms_data))    # Get number of rows and columns


# Extract longitude and latitude from the geometry column
firms_data <- firms_data %>%
  mutate(longitude = st_coordinates(.)[,1],  # Extract X (longitude)
         latitude = st_coordinates(.)[,2])   # Extract Y (latitude)


# Now, plot with ggplot
ggplot() +
  geom_sf(data = denmark, fill = "gray80", color = "black") +  # Brazil map
  geom_point(data = firms_data,
             aes(x = longitude, y = latitude, color = bright_t31, size = frp),
             alpha = 0.7) +  # Fire locations
  scale_color_gradient(low = "yellow", high = "red", name = "Brightness (Ti4)") +
  scale_size(range = c(1, 5), name = "Fire Radiative Power (FRP)") +
  labs(title = "NASA FIRMS Fire Detections in Germany",
       subtitle = paste("From", min(firms_data$acq_date), "to", max(firms_data$acq_date)),
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# Testing Function 2

fires_in_nature <- fetch_osm(firms_data, feature_types = c("natural"), must_be_in = TRUE)
fires_in_agriculture <- fetch_osm(firms_data, feature_types = c("agriculture"), must_be_in = TRUE)
industrial_fires <- fetch_osm(firms_data, feature_types = c("industrial", "waste", "urban", "parks", "military"), must_be_in = TRUE)

ggplot() +
  geom_sf(data = denmark, fill = "gray80", color = "black") +
  geom_point(data = fires_in_nature, aes(x = longitude, y = latitude), color = "green", size = 2) +
  geom_point(data = industrial_fires, aes(x = longitude, y = latitude), color = "red", size = 2) +
  geom_point(data = fires_in_agriculture, aes(x = longitude, y = latitude), color = "blue", size = 2) +
  labs(title = "Classified Fire Types",
       subtitle = "Green = Forest/Agriculture Fires, Red = Industrial/Power Fires")

