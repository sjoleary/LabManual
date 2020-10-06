
# load libraries
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
library("tidyverse")

# read data
occurence <- read_delim("data/bfro_reports_geocoded.txt", delim = "\t") %>%
  filter(!is.na(longitude),
         !is.na(latitude))

# get minimum and maximum lat/longs
max.lat <- ceiling(max(occurence$latitude))
min.lat <- floor(min(occurence$latitude))
max.lon <- ceiling(max(occurence$longitude))
min.lon <- floor(min(occurence$longitude))

# create an extent object
geo_range <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# get base map
data(wrld_simpl)

# plot the base map
plot(wrld_simpl,
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE,
     col = "grey75")

# add individual occurrences
points(x = occurence$longitude,
       y = occurence$latitude,
       col = "darkorange",
       pch = 20,
       cex = 0.75)

# draw box around figure
box()


# get list of files
files <- list.files("data/", pattern='^CM10_1975H', full.names=TRUE )

# import and convert to raster stack
predictors <- stack(files)


# create df with just xy coordinates
xy <- occurence %>%
  select(longitude, latitude)

# crop bioclim data to geographic range
cropped_predictors <- crop(x = predictors, y = geo_range)

# extract values
presence <- raster::extract(cropped_predictors, xy)

# fit model
model.fit <- bioclim(presence)

# raster with predicted map
prediction <- dismo::predict(x = cropped_predictors,
                             object = model.fit,
                             ext = geo_range)


# plot model probabilities
plot(prediction,
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE)

# add map
plot(wrld_simpl, add = TRUE, border = "black")

# draw box around it
box()
