# This example is using mock property location data and mock geographical shapes to find
# to find which of the properties are located inside the shape and within 20 miles outside

# packages / install if necessary 
library(sf)
library(sp)
library(tigris)
library(geosphere)
library(dplyr)
library(ggplot2)

# set a working directory
setwd("~/git/properties_inside_out_20mi")

# load mock program service area
shape <- readRDS("data/mock_psa_shape.rds")

# find which states are served by a mock program
# get US state shapefile
states <- st_as_sf(states())

# transform into the same coordinates system
shape$geometry <- st_transform(shape$geometry,st_crs(states))
# indeces of states which contain a geopoint
inds <- st_intersects(shape$geometry, states$geometry, sparse=T)

# initialize columsn to save states FIPS and abbs
shape['stateID'] <- NA
shape['stateABBR'] <- NA

# find the served states by their indices
for (i in 1:length(inds)){
  if (identical(states$NAME[inds[[i]]],character(0))){
    shape$stateID[i] <- NA}
  else{
    shape$stateID[i] <- list(states$GEOID[inds[[i]]])
    shape$stateABBR[i] <- list(states$STUSPS[inds[[i]]])
  }}

# read in mock property location data
data <- readRDS("data/mock_property_locations.RDS")

#################################################
# FIND ALL PROPERTIES WIHIN 20mi OF PROGRAM AREA
#################################################

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
shape <- st_transform(shape, 5070)

# transform property datapoint into coordinates
coords <- data %>%
  st_as_sf(coords = c("property_longitude", "property_latitude"), crs = 4326)

# transform into the same projection as program shape
coords <- st_transform(coords, 5070)

# get property lat/lons that fall within the boundary and a 20mi radius outside
# lists to save data
inside <- list()
outside <- list()

# extend the shape bounsary by 20mi
outer_shape <- st_difference(st_buffer(shape, dist=1609.34*20), st_buffer(shape,0) )
outer_shape <- st_transform(outer_shape, 5070)

# find which of the properties are inside
pts_inside <- st_within(coords$geometry, shape$geometry)
inside <- data[which(sapply(pts_inside,length) > 0 ),]

# find which properties are outside 
pts_outside <- st_within(coords_CL,outer_shape,sparse=TRUE)
outside <- data[which(sapply(pts_outside,length) > 0 ),]

# compute distance to boundary and convert to miles (negative if interior to boundary)
if(nrow(inside) > 0){
  coords_ins <- inside %>% st_as_sf(coords = c("property_longitude", "property_latitude"), crs = 4326)
  coords_ins <- st_transform(coords_ins, 5070)
  inside$dist_shape <- -1 * st_distance(coords_ins, st_cast(shape,"MULTILINESTRING"))/1609.34
}
if(nrow(outside) > 0){
  coords_out <- outside %>% st_as_sf(coords = c("property_longitude", "property_latitude"), crs = 4326)
  coords_out <- st_transform(coords_out, 5070)
  outside$dist_shape <- st_distance(coords_out, st_cast(shape,"MULTILINESTRING"))/1609.34
}

# put the dataset together
out_data <- rbind(inside, outside)

#####################
# PLOT
#####################

# plot with outer border and propery locations
ggplot() + 
  geom_sf(data = states$geometry[states$GEOID %in% shape$stateID[[1]]], fill = shape$stateID[[1]]) +
  geom_sf_text(data=states[states$GEOID %in% shape$stateID[[1]],], aes(label = NAME), colour = "black", size=3) +
  geom_point(data=outside, aes(x=property_longitude, y=property_latitude), color="orange", size=0.5) +
  geom_point(data=inside, aes(x=property_longitude, y=property_latitude), color="lightgreen", size=0.5) +
  geom_sf(data = shape$geometry, col="black", fill = NA) +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA)

