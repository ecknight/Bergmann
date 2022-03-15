library(rgee)
ee_Initialize() 

library(sf)
library(tidyverse)
library(lubridate)
library(data.table)
library(usdm)

#CHANGE ME!!!!!
setwd("/Users/ellyknight/Documents/UoA/Projects/Projects/Morphometrics/")

#1. Initialize rgee----
#ee_install()
ee_check()

#2. Settings----

#Distance kernel
kern <- ee$Kernel$euclidean(radius=20000, units="meters")

#Buffer radius
rad <- 300

#2. Write functions----
#2a. Function to add property with time in milliseconds
add_date<-function(feature) {
  date <- ee$Date(ee$String(feature$get("Date")))$millis()
  feature$set(list(date_millis=date))
}

#2b. Function to buffer points
buffer_points <- function(feature){
  properties <- c("TagID", "year") #CHANGE THESE!!!!!!!!!
  new_geometry <- feature$geometry()$buffer(rad)
  ee$Feature(new_geometry)$copyProperties(feature, properties)
}

#3. Import data----
dat <- read.csv("Bergs_rule.csv") %>% 
  dplyr::select(TagID, Banding.Date, B.Lat, B.Long) %>% 
  rename(X = B.Lat, Y = B.Long, Date = Banding.Date) %>% 
  mutate(Date = mdy(Date))

#4. Wrangle data----
dat$Date <- as.POSIXct(dat$Date, format = "%Y-%m-%d %H:%M:%S", tz="UTC") 
dat$Date <- as.factor(dat$Date)
dat$year <- year(ymd_hms(dat$Date))
dat$Date <- sub(" ", "T", dat$Date) #Put in a format that can be read by javascript

#5. Create sf object----
datasf <- st_as_sf(dat, coords = c('X','Y'), crs = 4326)

#6. Send data to GEE----
data <- sf_as_ee(datasf)

#7. Load EVI image collection----
imagecoll <- ee$ImageCollection('JAXA/ALOS/AW3D30/V3_2')
dsm <- imagecoll$select('DSM')

#8. Extract EVI point values----
data.evi <- ee_extract(
  x = dsm,
  y = datasf,
  scale = 30,
  sf = FALSE
)

#9. Load landcover image----
lc <- ee$Image('COPERNICUS/Landcover/100m/Proba-V-C3/Global/2017')

#10. Extract landcover point values----
data.lc <- ee_extract(
  x = lc,
  y = datasf,
  scale = 100,
  sf = FALSE
)

#11. Buffer points----
data.buff <- data$map(buffer_points)

#12. Extract buffer mean lc values----
lc.buff <- lc$reduceRegions(collection=data.buff, 
                            reducer=ee$Reducer$mean(), 
                            scale=30)

#13. Export lc task to google drive----
task_vector <- ee_table_to_drive(collection=lc.buff,
                                 description=paste0("LC_test_"),
                                 folder="Test",
                                 timePref=FALSE)
task_vector$start()
ee_monitoring(task_vector) # optional