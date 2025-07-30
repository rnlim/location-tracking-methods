######### SPATIAL ACCURACY ANALYSIS #########

# Load libraries
library(sf)
library(sp)

# Load in data
data <- read.csv("/Additional File 2 - accuracy data.csv")
handheld_csv <- data[data$method == "handheld",]
watch_csv <- data[data$method == "watch",]
tablet_csv <- data[data$method == "tablet",]

# Specify waypoint names
waypoints <- unique(handheld_csv$ID)

# Project lat&long to UTM Zone 50 for Sabah
handheld_sp = SpatialPoints(cbind(handheld_csv$long, handheld_csv$lat), proj4string=CRS("+proj=longlat"))
handheld_transformed <- spTransform(handheld_sp, CRS("+proj=utm +zone=50 ellps=WGS84"))

watch_sp = SpatialPoints(cbind(watch_csv$long, watch_csv$lat), proj4string=CRS("+proj=longlat"))
watch_transformed <- spTransform(watch_sp, CRS("+proj=utm +zone=50 ellps=WGS84"))

tablet_sp = SpatialPoints(cbind(tablet_csv$long, tablet_csv$lat), proj4string=CRS("+proj=longlat"))
tablet_transformed <- spTransform(tablet_sp, CRS("+proj=utm +zone=50 ellps=WGS84"))

# Format & label coordinates
handheld_df <- as.data.frame(handheld_transformed)
colnames(handheld_df) <- c("easting", "northing")
watch_df <- as.data.frame(watch_transformed)
colnames(watch_df) <- c("easting", "northing")
tablet_df <- as.data.frame(tablet_transformed)
colnames(tablet_df) <- c("easting", "northing")

watch_df$ID <- watch_csv$ID
tablet_df$ID <- tablet_csv$ID
handheld_df$ID <- handheld_csv$ID

# Calculate location error for watch
dist_wat <- c()
for (i in 1:length(waypoints)) {
  dist_wat <- c(dist_wat, sqrt((watch_df$easting[watch_df$ID == waypoints[i]] - handheld_df$easting[i])^2 + (watch_df$northing[watch_df$ID == waypoints[i]] - handheld_df$northing[i])^2))
}

canopy <- rep("light", length(dist_wat))
canopy[grepl("LD", watch_csv$ID)] <- "moderate"
canopy[grepl("DG", watch_csv$ID)] <- "dense"

summary(dist_wat)
summary(dist_wat[canopy == "light"])
summary(dist_wat[canopy == "moderate"])
summary(dist_wat[canopy == "dense"])

# Calculate location error for tablet
dist_tab <- c()
for (i in 1:length(waypoints)) {
  dist_tab <- c(dist_tab, sqrt((tablet_df$easting[tablet_df$ID == waypoints[i]] - handheld_df$easting[i])^2 + (tablet_df$northing[tablet_df$ID == waypoints[i]] - handheld_df$northing[i])^2))
}

canopy_tab <- rep("light", length(dist_tab))
canopy_tab[grepl("LD", tablet_csv$ID)] <- "moderate"
canopy_tab[grepl("DG", tablet_csv$ID)] <- "dense"

summary(dist_tab)
summary(dist_tab[canopy_tab == "light"])
summary(dist_tab[canopy_tab == "moderate"])
summary(dist_tab[canopy_tab == "dense"])

# Calculate percentage of watch points within 5, 10, and 30 m of ground truth
length(dist_wat[dist_wat <= 5]) / length(dist_wat)
length(dist_wat[dist_wat <= 10]) / length(dist_wat)
length(dist_wat[dist_wat <= 30]) / length(dist_wat)

light <- dist_wat[canopy == "light"]
length(light[light <= 5]) / length(light)
length(light[light <= 10]) / length(light)
length(light[light <= 30]) / length(light)

mod <- dist_wat[canopy == "moderate"]
length(mod[mod <= 5]) / length(mod)
length(mod[mod <= 10]) / length(mod)
length(mod[mod <= 30]) / length(mod)

dense <- dist_wat[canopy == "dense"]
length(dense[dense <= 5]) / length(dense)
length(dense[dense <= 10]) / length(dense)
length(dense[dense <= 30]) / length(dense)

# Calculate percentage of tablet points within 5, 10, and 30 m of ground truth
length(dist_tab[dist_tab <= 5]) / length(dist_tab)
length(dist_tab[dist_tab <= 10]) / length(dist_tab)
length(dist_tab[dist_tab <= 30]) / length(dist_tab)

light_tab <- dist_tab[canopy_tab == "light"]
length(light_tab[light_tab <= 5]) / length(light_tab)
length(light_tab[light_tab <= 10]) / length(light_tab)
length(light_tab[light_tab <= 30]) / length(light_tab)

mod_tab <- dist_tab[canopy_tab == "moderate"]
length(mod_tab[mod_tab <= 5]) / length(mod_tab)
length(mod_tab[mod_tab <= 10]) / length(mod_tab)
length(mod_tab[mod_tab <= 30]) / length(mod_tab)

dense_tab <- dist_tab[canopy_tab == "dense"]
length(dense_tab[dense_tab <= 5]) / length(dense_tab)
length(dense_tab[dense_tab <= 10]) / length(dense_tab)
length(dense_tab[dense_tab <= 30]) / length(dense_tab)