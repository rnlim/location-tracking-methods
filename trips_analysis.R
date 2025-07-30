############### TRIPS ANALYSIS ###############

# Load libraries
library(dplyr)
library(sf)
library(terra)
library(lubridate)
library(sp)
library(stringr)
library(ggplot2)

# Load mobile phone data
gps_data <- read.csv("/phone_gps.csv")
gps_data <- gps_data %>% filter(field_1 != 21604)
gps_data$DMYT <- as.POSIXct(gps_data$DMYT, format='%d/%m/%Y %H:%M')
gps_data$px <- gps_data$x_utm
gps_data$py <- gps_data$y_utm
vills <- 1:11

# OR load watch data
# watch_02C6 <- read.csv("/watch_02C6.csv")
# watch_02C6$new_id_ck <- "02C6"
# watch_02C7 <- read.csv("/watch_02C7.csv")
# watch_02C7$new_id_ck <- "02C7"
# watch_02CC <- read.csv("/watch_02CC.csv")
# watch_02CC$new_id_ck <- "02CC"
# watch_02CF <- read.csv("/watch_02CF.csv")
# watch_02CF$new_id_ck <- "02CF"
# 
# # Combine and remove failed attempts
# watch <- rbind(watch_02C6, watch_02C7, watch_02CC, watch_02CF)
# watch <- watch[watch$Longitude < 200, ]
# 
# # Project lat&long to UTM Zone 29 for Guinea
# watch_sp = SpatialPoints(cbind(watch$Longitude, watch$Latitude), proj4string=CRS("+proj=longlat"))
# watch_transformed <- spTransform(watch_sp, CRS("+proj=utm +zone=29 ellps=WGS84"))
# 
# # Format & label coordinates
# watch_df <- as.data.frame(watch_transformed)
# colnames(watch_df) <- c("x_utm", "y_utm")
# date <- sub("T.*", "", watch$Collecting.time)
# time <- gsub(".*T(.+)Z.*", "\\1", watch$Collecting.time)
# datetime <- paste(date, time)
# 
# # Format timestamps
# watch_df$DMYT <- as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S')
# months <- str_pad(month(watch_df$DMYT), 2, pad = "0")
# days <- str_pad(day(watch_df$DMYT), 2, pad = "0")
# hours <- str_pad(hour(watch_df$DMYT), 2, pad = "0")
# minutes <- str_pad(minute(watch_df$DMYT), 2, pad = "0")
# watch_df$DMYT <- paste(days, "/", months, "/", year(watch_df$DMYT), " ", hours,
#                        ":", minutes, sep="")
# watch_df$DMYT <- as.POSIXct(watch_df$DMYT, format='%d/%m/%Y %H:%M')
# watch_df$px <- watch_df$x_utm
# watch_df$py <- watch_df$y_utm
# watch_df$village_na <- "Selega"
# watch_df$new_id_ck <- watch$new_id_ck
# vills <- 10
# gps_data <- watch_df

# Load village boundaries as a spatial polygons dataframe
village_boundaries <- st_read("/villages_buffer100.shp")
village_boundaries <- village_boundaries[vills,c(2,55)]

table(village_boundaries$vill_name,unique(gps_data$village_na))

# Rename some villages
if (length(vills) > 1) {
  village_boundaries$vill_name[village_boundaries$vill_name == "Banon"] <- "Banaou"
  village_boundaries$vill_name[village_boundaries$vill_name == "Singuegou"] <- "Singuega"
}

# TRIPS -------------------------------------------------------------------
# Define the threshold for the maximum allowed gap between points (in seconds)
max_time_gap_seconds <- 3 * 60 * 60  # 3 hours in seconds

table(gps_data$new_id_ck, gps_data$village_na)

# List to store results for each village
village_summaries <- list()

# List of village names
village_names <- unique(gps_data$village_na)
combined_gps_data <- list()

# Loop through each village
for (village_name in village_names) {
  
  current_village_name <- village_name
  
  # Subset the data for the current village
  tou_df <- gps_data %>% filter(village_na == village_name)
  
  # Subset the village boundaries for the current village
  tou_shape <- village_boundaries[village_boundaries$vill_name == village_name, ]
  
  # Set the threshold for the maximum allowed gap between points (in seconds)
  max_time_gap_seconds <- 3 * 60 * 60  # 3 hours in seconds
  
  # Convert data to simple features and calculate distances
  tou_sf <- st_as_sf(tou_df, coords = c("px", "py"), crs = crs(tou_shape))
  tou_sf$hv_dist <- as.numeric(st_distance(x = tou_sf, y = tou_shape, crs = crs(tou_shape)))
  
  # Perform a spatial join to identify points inside/outside the village
  tou_sf <- tou_sf %>%
    st_join(tou_shape, join = st_within) %>%
    mutate(outside_village = ifelse(is.na(vill_name), TRUE, FALSE))
  
  # Calculate time gap and flag
  tou_sf <- tou_sf %>%
    group_by(new_id_ck) %>%
    arrange(new_id_ck, DMYT) %>%
    mutate(
      time_gap = DMYT - lag(DMYT, default = first(DMYT)),
      time_gap_flag = time_gap > max_time_gap_seconds
    ) %>%
    ungroup()
  
  # Assign trip IDs based on sequential groups of points outside the village within each person's ID
  tou_sf <- tou_sf %>%
    group_by(new_id_ck) %>%
    mutate(
      trip_id = cumsum(outside_village != lag(outside_village, default = FALSE) | time_gap_flag)
    )
  
  # Filter out points inside the village
  tou_sf <- tou_sf %>%
    filter(!is.na(outside_village))
  
  # Calculate trip summaries
  trip_summary <- tou_sf %>%
    group_by(new_id_ck, trip_id) %>%
    summarise(
      trip_frequency = n(),
      trip_duration = as.numeric(difftime(max(DMYT), min(DMYT), units = "secs")),
      trip_distance = first(hv_dist) + sum(sqrt((diff(x_utm)^2 + diff(y_utm)^2))),
      max_distance = max(hv_dist),
      trip_days = n_distinct(as.Date(DMYT))
    )
  
  # Convert trip duration from seconds to hours
  trip_summary$trip_duration_hours <- trip_summary$trip_duration / 3600
  
  # Set minimum duration to 10 minutes (600 seconds)
  trip_summary$trip_duration[trip_summary$trip_duration < 600] <- 600
  
  # Produce individual trip summaries
  individual_summary <- trip_summary %>%
    group_by(new_id_ck) %>%
    summarise(
      min_duration = min(trip_duration),
      max_duration = max(trip_duration),
      avg_duration = mean(trip_duration),
      min_travel_distance = min(trip_distance),
      max_travel_distance = max(trip_distance),
      avg_travel_distance = mean(trip_distance),
      min_max_vill_distance = min(max_distance),
      avg_max_vill_distance = mean(max_distance),
      max_max_vill_distance = max(max_distance),
      total_trips = n()
    )
  
  days_tracked <- tou_sf %>%
    group_by(new_id_ck) %>%
    mutate(days_tracked = n_distinct(as.Date(DMYT)))
  
  days_tracked <- cbind(days_tracked$new_id_ck, days_tracked$days_tracked)
  days_tracked <- as.data.frame(unique(days_tracked))
  names(days_tracked) <- c("new_id_ck", "days_tracked")
  
  individual_summary <- left_join(individual_summary, days_tracked)
  individual_summary$days_tracked <- as.numeric(individual_summary$days_tracked)
  
  individual_summary$trips_per_day <- individual_summary$total_trips / individual_summary$days_tracked
  
  # Produce cohort trip summaries
  vill_summary <- individual_summary %>%
    summarise(
      cohort_min_duration = min(min_duration),
      cohort_max_duration = max(max_duration),
      cohort_avg_duration = mean(avg_duration),
      total_cohort_trips = sum(total_trips),
      cohort_trips_per_day = (sum(total_trips)) / (sum(days_tracked)),
      min_travel_distance = min(min_travel_distance),
      max_travel_distance = max(max_travel_distance),
      avg_travel_distance = mean(avg_travel_distance),
      min_max_vill_distance = min(min_max_vill_distance),
      avg_max_vill_distance = mean(avg_max_vill_distance),
      max_max_vill_distance = max(max_max_vill_distance),
      days_tracked = sum(days_tracked)
    )
  
  trip_summary <- trip_summary %>%
    mutate(village_name = current_village_name)  # Add a village name column
  
  individual_summary <- individual_summary %>%
    mutate(village_name = current_village_name)  # Add a village name column
  
  vill_summary <- vill_summary %>%
    mutate(village_name = current_village_name)  # Add a village name column
  
  # Store the summary for the current village in the list
  village_summaries[[current_village_name]] <- list(
    trip_summary = trip_summary,
    individual_summary = individual_summary,
    vill_summary = vill_summary
  )
  
  combined_gps_data[[current_village_name]] <- tou_sf
}

# Combine results into dataframes with all village info
combined_trip_summary <- bind_rows(lapply(village_summaries, function(village) village$trip_summary))
combined_individual_summary <- bind_rows(lapply(village_summaries, function(village) village$individual_summary))
combined_village_summary <- bind_rows(lapply(village_summaries, function(village) village$vill_summary))

cohort_summary <- combined_village_summary %>%
  summarise(
    cohort_min_duration = min(cohort_min_duration),
    cohort_max_duration = max(cohort_max_duration),
    cohort_avg_duration = mean(cohort_avg_duration),
    total_cohort_trips = sum(total_cohort_trips),
    cohort_trips_per_day = (sum(total_cohort_trips)) / (sum(days_tracked)),
    min_travel_distance = min(min_travel_distance),
    max_travel_distance = max(max_travel_distance),
    avg_travel_distance = mean(avg_travel_distance),
    min_max_vill_distance = min(min_max_vill_distance),
    avg_max_vill_distance = mean(avg_max_vill_distance),
    max_max_vill_distance = max(max_max_vill_distance),
    days_tracked = sum(days_tracked)
  )

final_gps_data <- bind_rows(combined_gps_data)


# BAT ROOST VISITS --------------------------------------------------------
# Load the roost shapefile
roost_shapefile <- st_read("/roosts_buffer100.shp")
roost_shapefile<- roost_shapefile[, 168:170]
roost_shapefile$roost_id <- paste("roost", row_number(roost_shapefile), sep = "_")

# Merge roosts within 100m of each other into roost clusters
parts <- st_cast(st_union(roost_shapefile),"POLYGON")
plot(parts)

multiroost <- unlist(st_intersects(roost_shapefile, parts))

merged_roosts <- cbind(roost_shapefile, multiroost) %>%
  group_by(multiroost) %>%
  summarise(roost_ids = paste(roost_id, collapse = ", "))

merged_roosts$roost_group_id <-paste("roost_group", row_number(merged_roosts), sep = "_")
str(merged_roosts)
merged_roosts <- merged_roosts %>% mutate(multiroost = if_else(nchar(roost_ids)>10, TRUE, FALSE))
plot(merged_roosts)

# Spatial join to identify which roosts are intersected by each trip
trips_with_roosts <- st_join(final_gps_data, merged_roosts, join = st_intersects)
# Do not consider points with large time gaps in the trip, as this could be an errant point
trips_with_roosts$multiroost[trips_with_roosts$time_gap_flag == TRUE] <- NA
trips_with_roosts$roost_ids[trips_with_roosts$time_gap_flag == TRUE] <- NA
trips_with_roosts$roost_group_id[trips_with_roosts$time_gap_flag == TRUE] <- NA

# Count the number of intersected roosts per trip
roost_counts <- trips_with_roosts %>%
  group_by(new_id_ck, trip_id) %>%
  mutate(num_intersected_roosts = (n_distinct(roost_group_id)-1)) # -1 to ignore NA  

roost_counts <- roost_counts %>%
  arrange(new_id_ck, trip_id, DMYT)

# set time_gap to 5 minutes where it is 0 (first in a group) - this is the time between points if working well near a roost
roost_counts$time_gap_numeric <- as.numeric(sub(" secs", "", roost_counts$time_gap))

roost_counts$time_gap_numeric[roost_counts$time_gap_numeric == 0] <- 300 

# Produce roost visit summaries
roost_counts_sum <- roost_counts %>%
  group_by(new_id_ck, trip_id, roost_group_id) %>%
  mutate(
    num_intersected_roosts = n_distinct(roost_group_id),
    time_at_roost = sum(time_gap_numeric[!is.na(roost_group_id)]))

roost_counts_sum <- roost_counts_sum %>% 
  mutate(
    date_only = as.Date(DMYT),
    hour = hour(DMYT),
    minute = minute(DMYT),
  ) %>% 
  mutate(
    format_date = format(date_only, "%d/%m/%Y"),
    format_hour = paste(hour, minute, sep = ":")
  )

roost_counts_sum$format_hour <- as.POSIXct(roost_counts_sum$format_hour, format = "%H:%M")

# Per trip with a roost visit
per_roost_trip_roost_sum<- roost_counts_sum %>%
  filter(!is.na(roost_group_id)) %>% 
  group_by(new_id_ck, trip_id, roost_group_id) %>%
  summarise(
    num_intersected_roosts = n_distinct(roost_group_id),
    time_at_roost = sum(time_gap_numeric[!is.na(roost_group_id)]),
    earliest = min(format_hour),
    mid_time = median(format_hour),
    latest = max(format_hour))

# Per trip
per_trip_roost_sum<- per_roost_trip_roost_sum %>%
  group_by(new_id_ck, trip_id) %>%
  summarise(
    num_intersected_roosts = sum(num_intersected_roosts),
    time_at_any_roost = sum(time_at_roost),
    earliest = min(earliest),
    mid_time = median(mid_time),
    latest = max(latest))

# Per person
per_person_roost_sum <- per_trip_roost_sum %>%
  group_by(new_id_ck, trip_id) %>%
  summarise(
    num_roost_visits = sum(num_intersected_roosts),
    time_at_any_roost = sum(time_at_any_roost),
    earliest = min(earliest),
    mid_time = median(mid_time),
    latest = max(latest))

days_tracked <- roost_counts %>%
  group_by(village_na, new_id_ck) %>%
  mutate(days_tracked = n_distinct(as.Date(DMYT)))

days_tracked <- cbind(days_tracked$new_id_ck, days_tracked$village_na, days_tracked$days_tracked)
days_tracked <- as.data.frame(unique(days_tracked))
colnames(days_tracked) <- c("new_id_ck", "village_name", "days_tracked")

per_person_roost_sum$new_id_ck <- as.character(per_person_roost_sum$new_id_ck)
per_person_roost_sum <- left_join(per_person_roost_sum, days_tracked)
per_person_roost_sum$days_tracked <- as.numeric(per_person_roost_sum$days_tracked)

# Per village
per_vill_roost_sum <- per_person_roost_sum %>%
  group_by(new_id_ck) %>%
  summarise(
    num_roost_visits = sum(num_roost_visits),
    time_at_any_roost = sum(time_at_any_roost),
    earliest = min(earliest),
    mid_time = median(mid_time),
    latest = max(latest),
    days_tracked = sum(days_tracked))

# For cohort
cohort_roost_sum <- per_vill_roost_sum %>% 
  summarise(
    num_roost_visits = sum(num_roost_visits),
    time_at_any_roost = sum(time_at_any_roost),
    earliest = min(earliest),
    mid_time = median(mid_time),
    latest = max(latest),
    days_tracked = sum(days_tracked))

# Get histogram of times of day when roosts were visited
hist(roost_counts_sum$format_hour[!is.na(roost_counts_sum$roost_group_id)], breaks = "hours")