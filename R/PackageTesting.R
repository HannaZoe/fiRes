# Load required libraries
library(devtools)  # To load the package
library(sf)        # For spatial data handling
library(ggplot2)   # For visualization
library(leaflet)   # For interactive maps
library(rnaturalearth)
library(rnaturalearthdata)

# Get Brazil's bounding box
brazil <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")


# Example usage
api_key <- "917f143bded8a25852d7f824ca527e13"
start_date <- "2025-03-01"
end_date <- "2025-03-05"

firms_data <- fetch_firms_data(api_key, brazil, start_date, end_date, dataset = "MODIS_NRT", confidence_level = c("h"))

print(head(firms_data))   # See first 6 rows
print(dim(firms_data))    # Get number of rows and columns


# Extract longitude and latitude from the geometry column
firms_data <- firms_data %>%
  mutate(longitude = st_coordinates(.)[,1],  # Extract X (longitude)
         latitude = st_coordinates(.)[,2])   # Extract Y (latitude)

# Now, plot with ggplot
ggplot() +
  geom_sf(data = brazil_sf, fill = "gray80", color = "black") +  # Brazil map
  geom_point(data = firms_data,
             aes(x = longitude, y = latitude, color = bright_ti4, size = frp),
             alpha = 0.7) +  # Fire locations
  scale_color_gradient(low = "yellow", high = "red", name = "Brightness (Ti4)") +
  scale_size(range = c(1, 5), name = "Fire Radiative Power (FRP)") +
  labs(title = "NASA FIRMS Fire Detections in Brazil",
       subtitle = paste("From", min(firms_data$acq_date), "to", max(firms_data$acq_date)),
       x = "Longitude", y = "Latitude") +
  theme_minimal()




