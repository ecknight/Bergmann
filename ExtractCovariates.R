library(rgee)
ee_Initialize() 

library(sf)
library(tidyverse)
library(lubridate)
library(data.table)
library(usdm)

#SET UP####

#1. Import data----
dat.raw <- read.csv("CapDfElly.csv")

dat.b <- dat.raw %>% 
  rename(Y = B.Lat, X = B.Long) %>% 
  mutate(season = "Breed") %>% 
  dplyr::select(rowID, Species, X, Y, season)

dat.w <- dat.raw %>% 
  rename(Y = W.Lat, X = W.Long) %>% 
  mutate(season = "Winter") %>% 
  dplyr::select(rowID, Species,  X, Y, season) %>% 
  dplyr::filter(!is.na(X))

dat <- rbind(dat.b, dat.w)

#2. Initialize rgee----
#ee_install()
ee_check()

#3. Visualize----
ggplot(dat) +
  geom_point(aes(x=X, y=Y, colour=Species)) +
  facet_wrap(~season)

#BUFFER COVS####

#1. Write functions----
buffer_points <- function(feature){
  properties <- c("TagID", "year") #CHANGE THESE!!!!!!!!!
  new_geometry <- feature$geometry()$buffer(rad)
  ee$Feature(new_geometry)$copyProperties(feature, properties)
}

#2. Set up loop----
spp <- data.frame(expand.grid(species = unique(dat$Species),
                              season = unique(dat$season)),
                  radius = c(500, 5000, 500, 100, 1000, 100)) %>% 
  mutate(id = paste0(species, "-", season))

for(i in 1:nrow(spp)){
  
  #3. Set buffer radius
  rad <- spp$radius[i]
  
  #4. Filter data----
  dat.i <- dat %>% 
    dplyr::filter(Species==spp$species[i],
                  season==spp$season[i]) %>% 
    mutate(loop = ceiling(row_number()/100))
  
  for(k in 1:max(dat.i$loop)){
    
    #5. Filter data again----
    dat.k <- dat.i %>% 
      dplyr::filter(loop==k)
    
    #5. Create sf object----
    datasf <- st_as_sf(dat.k, coords = c('X','Y'), crs = 4326)
    
    #6. Send data to GEE----
    data <- sf_as_ee(datasf)
    
    #7. Buffer points----
    data.buff <- data$map(buffer_points)
    
    #8. EVI----
    evi <- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_EVI')$select('EVI')$toBands()
    
    image.evi <- evi$reduceRegions(collection=data.buff,
                                   reducer=ee$Reducer$mean(),
                                   scale=30)
    
    task_vector <- ee_table_to_gcs(collection=image.evi,
                                   bucket="lbcu",
                                   fileFormat = "CSV",
                                   fileNamePrefix = spp$id[i])
    task_vector$start()
    ee_monitoring(task_vector, max_attempts=1000)
    ee_gcs_to_local(task = task_vector, dsn=paste0("buffercovs/evi-", spp$id[i], "-", k, ".csv"))
    
    #5. DEM----
    dem <- ee$Image('USGS/GTOPO30')
    
    image.dem <- dem$reduceRegions(collection=data.buff,
                                   reducer=ee$Reducer$mean(),
                                   scale=30)
    
    task_vector <- ee_table_to_gcs(collection=image.dem,
                                   bucket="lbcu",
                                   fileFormat = "CSV",
                                   fileNamePrefix = spp$id[i])
    task_vector$start()
    ee_monitoring(task_vector, max_attempts=1000)
    ee_gcs_to_local(task = task_vector, dsn=paste0("buffercovs/dem-", spp$id[i], "-", k, ".csv"))
    
    #6. Worldclim monthly----
    clim <- ee$ImageCollection('WORLDCLIM/V1/MONTHLY')$toBands()
    
    image.clim <- clim$reduceRegions(collection=data.buff,
                                     reducer=ee$Reducer$mean(),
                                     scale=30)
    
    task_vector <- ee_table_to_gcs(collection=image.clim,
                                   bucket="lbcu",
                                   fileFormat = "CSV",
                                   fileNamePrefix = spp$id[i])
    task_vector$start()
    ee_monitoring(task_vector, max_attempts=1000)
    ee_gcs_to_local(task = task_vector, dsn=paste0("buffercovs/clim-", spp$id[i], "-", k, ".csv"))
    
    #8. Terraclim monthly----
    years <- seq(1958, 2020, 1) 
    
    data.terra <- list()
    for(j in 1:length(years)){
      
      start<-paste0(years[j], "-01-01")
      end<-paste0(years[j],"-12-31")
      
      terra<-ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$filterDate(start,end)$toBands()
      
      image.terra <- terra$reduceRegions(collection=data.buff,
                                         reducer=ee$Reducer$mean(),
                                         scale=30)
      
      task_vector <- ee_table_to_gcs(collection=image.terra,
                                     bucket="lbcu",
                                     fileFormat = "CSV",
                                     fileNamePrefix = spp$id[i])
      task_vector$start()
      ee_monitoring(task_vector, max_attempts=2000)
      ee_gcs_to_local(task = task_vector, dsn=paste0("buffercovs/terra-",years[j],"-", spp$id[i], "-", k, ".csv"))
    }
    
  }
  
  print(paste0("COMPLETED SPECIES ", spp$species[i]))
}

files <- data.frame(filepath = list.files("buffercovs", full.names = TRUE)) %>% 
  separate(filepath, into=c("folder", "file"), sep="/", remove=FALSE) %>% 
  separate(file, into=c("var", "year", "species",  "season", "loop"), sep="-") %>% 
  mutate(loop = ifelse(!var %in% c("era", "terra"), season, loop),
         loop = as.numeric(str_sub(loop, -100, -5)),
         season = ifelse(!var %in% c("era", "terra"), species, season),
         species = ifelse(!var %in% c("era", "terra"), year, species),
         year = ifelse(!var %in% c("era", "terra"), NA, year))

evi.list <- list()
dem.list <- list()
clim.list <- list()
terra.list <- list()
for(i in 1:nrow(spp)){
  
  dat.i <- dat %>% 
    dplyr::filter(Species==spp$species[i],
                  season==spp$season[i]) %>% 
    mutate(loop = ceiling(row_number()/100))
  
  evi.list.list <- list()
  dem.list.list <- list()
  clim.list.list <- list()
  terra.list.list <- list()
  for(k in 1:max(dat.i$loop)){
    
    dat.k <- dat.i %>% 
      dplyr::filter(loop==k)
    
    evi.list.list[[k]] <- dat.k %>% 
      cbind(read.csv(dplyr::filter(files, var=="evi", species==spp$species[i], season==spp$season[i], loop==k)$filepath)) %>% 
      dplyr::select(-system.index, -.geo)
    
    dem.list.list[[k]] <- dat.k %>% 
      cbind(read.csv(dplyr::filter(files, var=="dem", species==spp$species[i], season==spp$season[i], loop==k)$filepath)) %>% 
      dplyr::select(-system.index, -.geo)
    
    clim.list.list[[k]] <- dat.k %>% 
      cbind(read.csv(dplyr::filter(files, var=="clim", species==spp$species[i], season==spp$season[i], loop==k)$filepath)) %>% 
      dplyr::select(-system.index, -.geo)
    
    terra.files <- dplyr::filter(files, var=="terra", species==spp$species[i], season==spp$season[i], loop==k)
    terra.list.list.list <- list()
    for(j in 1:nrow(terra.files)){
      terra.wide <- dat.k %>% 
        cbind(read.csv(terra.files$filepath[j])) %>% 
        dplyr::select(-system.index, -.geo)
      terra.list.list[[j]] <- terra.wide %>% 
        pivot_longer(7:ncol(terra.wide), names_to="layer", values_to="value") %>% 
        mutate(covdate = str_sub(layer, 1, 7),
               cov = str_sub(layer, 9, 100)) %>% 
        dplyr::select(-layer) %>% 
        pivot_wider(names_from="cov", values_from="value") %>% 
        mutate(covyear = as.numeric(str_sub(covdate, 2, 5)),
               covmonth = as.numeric(str_sub(covdate, 6, 7)))
    }
    
    terra.list.list[[k]] <- rbindlist(terra.list.list.list)
    
  }
  
  evi.list[[i]] <- rbindlist(evi.list.list)
  dem.list[[i]] <- rbindlist(dem.list.list)
  clim.list[[i]] <- rbindlist(clim.list.list)
  terra.list[[i]] <- rbindlist(terra.list.list)
  
}

evi.out <- rbindlist(evi.list)
dem.out <- rbindlist(dem.list)
clim.out <- rbindlist(clim.list)
terra.out <- rbindlist(terra.list)

write.csv(evi.out, "EVI-buffer.csv", row.names = FALSE)
write.csv(dem.out, "DEM-buffer.csv", row.names = FALSE)
write.csv(clim.out, "Wordclim-buffer.csv", row.names = FALSE)
write.csv(terra.out, "Terraclim-buffer.csv", row.names = FALSE)
