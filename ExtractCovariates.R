library(rgee)
ee_Initialize() 

library(sf)
library(tidyverse)
library(lubridate)
library(data.table)
library(usdm)

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
dat.raw <- read.csv("capri.fac.csv") %>% 
  rename(ID=X)

dat.b <- dat.raw %>% 
  rename(Y = B.Lat, X = B.Long, Date = Banding.Date) %>% 
  mutate(Date = ymd(Date),
         season = "Breed") %>% 
  dplyr::select(ID, Species, Date, X, Y, season)

dat.w <- dat.raw %>% 
  mutate(ID = row_number()) %>% 
  rename(Y = W.Lat, X = W.Long) %>% 
  mutate(Date = mdy(paste0("12-15-", Year)),
         season = "Winter") %>% 
  dplyr::select(ID, Species, Date, X, Y, season)

dat <- rbind(dat.b, dat.w) %>% 
  mutate(Species = case_when(Species %in% c("Ceur", "European Nightjar", "European Nigthtjar") ~ "EUNI",
                             !is.na(Species) ~ Species))

#4. Visualize----
ggplot(dat) +
  geom_point(aes(x=X, y=Y, colour=Species)) +
  facet_wrap(~season)

#4. Wrangle data----
dat$Date <- as.POSIXct(dat$Date, format = "%Y-%m-%d %H:%M:%S", tz="UTC") 
dat$Date <- as.factor(dat$Date)
dat$year <- year(ymd_hms(dat$Date))
dat$Date <- sub(" ", "T", dat$Date) #Put in a format that can be read by javascript

#5. Create sf object----
datasf <- st_as_sf(dat, coords = c('X','Y'), crs = 4326)

#6. Send data to GEE----
data <- sf_as_ee(datasf)

#7. EVI----
evi <- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_EVI')

data.evi <- ee_extract(
  x = evi,
  y = datasf,
  scale = 30,
  sf = FALSE
)
data.evi.wide <- left_join(dat, data.evi)

data.evi.long <- data.evi.wide %>% 
  pivot_longer(8:ncol(data.evi.wide), names_to="layer", values_to="value") %>% 
  mutate(covdate = str_sub(layer, 1, 9),
         cov = str_sub(layer, 11, 100)) %>% 
  dplyr::select(-layer) %>% 
  pivot_wider(names_from="cov", values_from="value") %>% 
  mutate(covyear = as.numeric(str_sub(covdate, 2, 5)),
         covmonth = as.numeric(str_sub(covdate, 6, 7)),
         covday = as.numeric(str_sub(covdate, 8, 9)))

write.csv(data.evi.long, "EVI.csv", row.names = FALSE)

#8. DEM----
dem <- ee$Image('USGS/GTOPO30')

data.dem <- ee_extract(
  x = dem,
  y = datasf,
  sf = FALSE
)
write.csv(data.dem, "DEM.csv", row.names = FALSE)

#9. Worldclim monthly----
clim <- ee$ImageCollection('WORLDCLIM/V1/MONTHLY')

data.clim <- ee_extract(
  x = clim,
  y = datasf,
  sf = FALSE
)

data.clim.wide <- left_join(dat, data.clim)

data.clim.long <- data.clim.wide %>% 
  pivot_longer(8:ncol(data.clim.wide), names_to="layer", values_to="value") %>% 
  mutate(covdate = str_sub(layer, 1, 3),
         cov = str_sub(layer, 5, 100)) %>% 
  dplyr::select(-layer) %>% 
  pivot_wider(names_from="cov", values_from="value") %>% 
  mutate(covmonth = as.numeric(str_sub(covdate, 2, 3)))

write.csv(data.clim.long, "Wordclim.csv", row.names = FALSE)

#10. ERA monthly----
years <- seq(1981, 2021, 1) 

data.era <- list()
for(i in 1:length(years)){
  
  start<-paste0(years[i], "-01-01")
  end<-paste0(years[i],"-12-31")
  
  era<-ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY")$filterDate(start,end)
  
  data.i <- ee_extract(
    x = era,
    y = datasf,
    sf = FALSE
  )
  
  data.era[[i]] <- data.i %>% 
    mutate(year = years[i])
  
  print(paste0("Completed year ", years[i], " of ", length(years), " years"))
}

data.era.wide <- cbind(dat,
                       do.call(cbind, data.era) %>% 
  dplyr::select(-ID, -Species, -Date, -season, -year))

data.era.long <- data.era.wide %>% 
  pivot_longer(8:ncol(data.era.wide), names_to="layer", values_to="value") %>% 
  mutate(covdate = str_sub(layer, 1, 7),
         cov = str_sub(layer, 9, 100)) %>% 
  dplyr::select(-layer) %>% 
  pivot_wider(names_from="cov", values_from="value") %>% 
  mutate(covyear = as.numeric(str_sub(covdate, 2, 5)),
         covmonth = as.numeric(str_sub(covdate, 6, 7)))

write.csv(data.era.long, "ERA5.csv", row.names = FALSE)

#11. Terraclim monthly----
years <- seq(1958, 2020, 1) 

data.terra <- list()
for(i in 1:length(years)){
  
  start<-paste0(years[i], "-01-01")
  end<-paste0(years[i],"-12-31")
  
  terra<-ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$filterDate(start,end)
  
  data.i <- ee_extract(
    x = terra,
    y = datasf,
    sf = FALSE
  )
  
  data.terra[[i]] <- data.i %>% 
    mutate(year = years[i])
  
  print(paste0("Completed year ", years[i], " of ", length(years), " years"))
}

data.terra.wide <- cbind(dat,
                       do.call(cbind, data.terra) %>% 
                         dplyr::select(-ID, -Species, -Date, -season, -year))

data.terra.long <- data.terra.wide %>% 
  pivot_longer(8:ncol(data.terra.wide), names_to="layer", values_to="value") %>% 
  mutate(covdate = str_sub(layer, 1, 7),
         cov = str_sub(layer, 9, 100)) %>% 
  dplyr::select(-layer) %>% 
  pivot_wider(names_from="cov", values_from="value") %>% 
  mutate(covyear = as.numeric(str_sub(covdate, 2, 5)),
         covmonth = as.numeric(str_sub(covdate, 6, 7)))

write.csv(data.terra.long, "Terraclim.csv", row.names = FALSE)
