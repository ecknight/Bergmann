#COMMENTS FROM EMAIL THREAD FOR INSTRUCTIONS####

#a) show the descriptive data for the different classes (on a map could be helpful, which I think Gabriel was alluding to in his comments?)

#As for mapping things out (like size isoclines I assume?).. I think this would be helpful, but I probably need to get back to my PhD stuff soon & think I have my plate full getting things finished up here & ready to send to co-authors. Do you want to take the lead on producing this figure Alicia? I can share the final data set! 

#Elly, I love making maps too, but have plenty to do! If you want to play around with this, go for it! For ideas, this is part of Gabriel's comment (including the map) in the manuscript: "illustrate the spatial variation in body-size (which we currently don't show anywhere). I add a figure here of the wintering distribution of the Swedish nightjars as an example and source of inspiration." I was also thinking of trying a simple interpolation of body sizes for each species across their breeding grounds, which I envisioned could look something like the second map.

#Alicia & I are in the same boat! Additional examples found in Younflesh et al (2022) paper Figs 3 or 4, or James paper (attached), page 372 onwards.  Elly, super kind of you to offer to take that one on.. Thanks!

#1. Load libraries----
library(tidyverse)
library(readxl)
library(MuMIn)
library(rgdal)
library(sf)
library(terra)
library(ggmap)
library(gridExtra)

#2. Load data----
dat <- read_excel("capriA.red_02.02.24.xlsx")

#3. Explore----

#Mass
ggplot(dat) + 
  geom_point(aes(x=B.Lat, y=B.Long, colour=Mass.comb)) +
  facet_wrap(~Species, scales="free") +
  scale_colour_viridis_c()

ggplot(dat) + 
  geom_point(aes(x=B.Lat, y=Mass.comb)) +
  geom_smooth(aes(x=B.Lat, y=Mass.comb)) +
  facet_wrap(~Species, scales="free") +
  scale_colour_viridis_c()

ggplot(dat) + 
  geom_point(aes(x=B.Long, y=Mass.comb)) +
  geom_smooth(aes(x=B.Long, y=Mass.comb)) +
  facet_wrap(~Species, scales="free") +
  scale_colour_viridis_c()

ggplot(dat) + 
  geom_point(aes(x=B.Elev, y=Mass.comb)) +
  geom_smooth(aes(x=B.Elev, y=Mass.comb)) +
  facet_wrap(~Species, scales="free") +
  scale_colour_viridis_c()

#Wing
ggplot(dat) + 
  geom_point(aes(x=B.Lat, y=B.Long, colour=Wing.comb)) +
  facet_wrap(~Species, scales="free") +
  scale_colour_viridis_c()

ggplot(dat) + 
  geom_point(aes(x=B.Lat, y=Wing.comb, colour=Sex)) +
  geom_smooth(aes(x=B.Lat, y=Wing.comb)) +
  facet_wrap(~Species, scales="free") +
  scale_colour_viridis_d()

ggplot(dat) + 
  geom_point(aes(x=B.Long, y=Wing.comb)) +
  geom_smooth(aes(x=B.Long, y=Wing.comb)) +
  facet_wrap(~Species, scales="free") +
  scale_colour_viridis_c()

ggplot(dat) + 
  geom_point(aes(x=B.Elev, y=Wing.comb)) +
  geom_smooth(aes(x=B.Elev, y=Wing.comb)) +
  facet_wrap(~Species, scales="free") +
  scale_colour_viridis_c()

#4. Model the relationships----

#Models to use from top AIC models from: https://docs.google.com/document/d/1ZDd5Dt5ksrhN5hejxO80DMz-BCfcnsjE78bHAtYMKDg/edit

#CONI mass
#Lat + Lon + Time since sunset² + Sex
dat.coni.mass <- dplyr::filter(dat, Species=="Nighthawk", !is.na(Mass.comb), !is.na(tsss.comb))
m.coni.mass <- lm(Mass.comb ~ B.Lat + B.Long + Sex + poly(tsss.comb, 2), data=dat.coni.mass)
summary(m.coni.mass)

#CONI wing
#Lat + Sex
dat.coni.wing <- dplyr::filter(dat, Species=="Nighthawk", !is.na(Wing.comb), !is.na(tsss.comb))
m.coni.wing <- lm(Wing.comb ~ B.Lat + Sex, data=dat.coni.wing)
summary(m.coni.wing)

#EUNI mass
#Age + Lat + Time since sunset² + Sex
dat.euni.mass <- dplyr::filter(dat, Species=="Nightjar", !is.na(Mass.comb), !is.na(tsss.comb))
m.euni.mass <- lm(Mass.comb ~ B.Lat + Sex + Age + poly(tsss.comb, 2), data=dat.euni.mass)
summary(m.euni.mass)

#EUNI wing
#Age + Lat + Sex
dat.euni.wing <- dplyr::filter(dat, Species=="Nightjar", !is.na(Wing.comb), !is.na(tsss.comb))
m.euni.wing <- lm(Wing.comb ~ B.Lat + Sex + Age, data=dat.euni.wing)
summary(m.euni.wing)

#EWPW mass
#Age + Lat + Lon + Time since sunset² + Sex
dat.ewpw.mass <- dplyr::filter(dat, Species=="Whip-poor-will", !is.na(Mass.comb), !is.na(tsss.comb))
m.ewpw.mass <- lm(Mass.comb ~ B.Lat + B.Long + Sex + Age + poly(tsss.comb, 2), data=dat.ewpw.mass)
summary(m.ewpw.mass)

#EWPW wing
#Age + Lat + Lon + Sex
dat.ewpw.wing <- dplyr::filter(dat, Species=="Whip-poor-will", !is.na(Wing.comb), !is.na(tsss.comb))
m.ewpw.wing <- lm(Wing.comb ~ B.Lat + B.Long + Sex + Age, data=dat.ewpw.wing)
summary(m.ewpw.wing)

#5. Get the DEMs----

#ONLY RUN THIS ONCE TO DOWNLOAD
# library(rgee)
# ee_Initialize()
# 
# img <- ee$Image("NOAA/NGDC/ETOPO1")$select(c("bedrock"))
# 
# geo.coni <- ee$Geometry$Rectangle(
#   coords = c(-140, 5, -55, 68),
#   proj = "EPSG:4326",
#   geodesic = FALSE
# )
# 
# img.coni <- ee_as_rast(
#   image = img,
#   region =  geo.coni,
#   via = "drive",
#   lazy = TRUE
# )
#GO GET RASTERS FROM GOOGLE DRIVE

elev.coni <- rast("dems/ETOPO1_coni.tif")

#5. Get the ranges & rasterize----

ranges <- read_sf("ranges/BOTW_9-3_Caprimulgids.shp")

#CONI
coni <- ranges %>% 
  dplyr::filter(SCINAME=="Chordeiles minor",
                SEASONA==2) %>% 
  vect()
r <- rast(ext(coni), resolution=0.1, crs=crs(coni))
r.coni <- rasterize(x=coni, y=r, field="SCINAME") %>% 
  as.data.frame(xy=TRUE) %>% 
  mutate(Age = "Unk",
         Sex = "M",
         tsss.comb = mean(dat.coni.mass$tsss.comb)) %>% 
  rename(B.Lat = y, B.Long = x)

#EWPW
ewpw <- ranges %>% 
  dplyr::filter(SCINAME=="Caprimulgus vociferus",
                SEASONA==2) %>% 
  vect()
r <- rast(ext(ewpw), resolution=0.1, crs=crs(ewpw))
r.ewpw <- rasterize(x=ewpw, y=r, field="SCINAME") %>% 
  as.data.frame(xy=TRUE) %>% 
  mutate(Age = "Unk",
         Sex = "M",
         tsss.comb = mean(dat.ewpw.mass$tsss.comb)) %>% 
  rename(B.Lat = y, B.Long = x)

#EUNI
euni <- ranges %>% 
  dplyr::filter(SCINAME=="Caprimulgus europaeus",
                SEASONA==2) %>% 
  vect()
r <- rast(ext(euni), resolution=0.1, crs=crs(euni))
r.euni <- rasterize(x=euni, y=r, field="SCINAME") %>% 
  as.data.frame(xy=TRUE) %>% 
  mutate(Age = "Unk",
         Sex = "M",
         tsss.comb = mean(dat.euni.mass$tsss.comb)) %>% 
  rename(B.Lat = y, B.Long = x)

#6. Make predictions----
pred.coni.mass <- data.frame(val = predict(m.coni.mass, newdata=r.coni)) %>% 
  cbind(r.coni)

pred.coni.wing <- data.frame(val = predict(m.coni.wing, newdata=r.coni)) %>% 
  cbind(r.coni)

pred.euni.mass <- data.frame(val = predict(m.euni.mass, newdata=r.euni)) %>% 
  cbind(r.euni)

pred.euni.wing <- data.frame(val = predict(m.euni.wing, newdata=r.euni)) %>% 
  cbind(r.euni)

pred.ewpw.mass <- data.frame(val = predict(m.ewpw.mass, newdata=r.ewpw)) %>% 
  cbind(r.ewpw)

pred.ewpw.wing <- data.frame(val = predict(m.ewpw.wing, newdata=r.ewpw)) %>% 
  cbind(r.ewpw)

#7. Aggregate sampling data----
sample.coni.mass <- dat.coni.mass %>% 
  mutate(latr = round(B.Lat),
         lonr = round(B.Long)) %>% 
  group_by(latr, lonr) %>% 
  summarize(samples = n()) %>% 
  ungroup()

sample.euni.mass <- dat.euni.mass %>% 
  mutate(latr = round(B.Lat),
         lonr = round(B.Long)) %>% 
  group_by(latr, lonr) %>% 
  summarize(samples = n()) %>% 
  ungroup()

sample.ewpw.mass <- dat.ewpw.mass %>% 
  mutate(latr = round(B.Lat),
         lonr = round(B.Long)) %>% 
  group_by(latr, lonr) %>% 
  summarize(samples = n()) %>% 
  ungroup()

#7. Get basemap data----

# nam <- map_data("world", region=c("Canada", "USA", "Mexico", "Guatemala", "Panama", "Honduras", "Nicaragua", "Costa Rica", "Dominican Republic", "Jamaica", "Haiti", "Cuba"))

world <- map_data("world")
lake <- map_data("lakes")

#8. Set map theme----
map.theme <- theme_nothing() +
  theme(text=element_text(size=12, family="Arial"),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) +
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1))

#9. Plot----

#Wing
plot.coni.wing <- ggplot(pred.coni.wing) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray80", colour = "gray80", size=0.3) +
  geom_polygon(data=lake, aes(x=long, y=lat, group=group), fill="white", colour = "white", size=0.3) +
  geom_tile(aes(x=B.Long, y=B.Lat, fill=val)) +
  geom_jitter(data=dat.coni.wing, aes(x=B.Long, y=B.Lat), pch=21, size=3, alpha = 0.5) +
  coord_sf(xlim=c(-140, -55), ylim=c(5, 68), expand = FALSE, crs=4326) +
  scale_fill_viridis_c(name = "Wing length (mm)") +
  map.theme +
  xlab("") +
  ylab("") + 
  theme(legend.position = "bottom") +
  ggtitle("Nighthawk")
plot.coni.wing

plot.ewpw.wing <- ggplot(pred.ewpw.wing) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray80", colour = "gray80", size=0.3) +
  geom_polygon(data=lake, aes(x=long, y=lat, group=group), fill="white", colour = "white", size=0.3) +
  geom_tile(aes(x=B.Long, y=B.Lat, fill=val)) +
  geom_jitter(data=dat.ewpw.wing, aes(x=B.Long, y=B.Lat), pch=21, size=3, alpha = 0.5) +
  coord_sf(xlim=c(-110, -62), ylim=c(29, 57), expand = FALSE, crs=4326) +
  scale_fill_viridis_c(name = "Wing length (mm)") +
  map.theme +
  xlab("") +
  ylab("") + 
  theme(legend.position = "bottom") +
  ggtitle("Whip-poor-will")
plot.ewpw.wing

plot.euni.wing <- ggplot(pred.euni.wing) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray80", colour = "gray80", size=0.3) +
  geom_polygon(data=lake, aes(x=long, y=lat, group=group), fill="white", colour = "white", size=0.3) +
  geom_tile(aes(x=B.Long, y=B.Lat, fill=val)) +
  geom_jitter(data=dat.euni.wing, aes(x=B.Long, y=B.Lat), pch=21, size=3, alpha = 0.5) +
  coord_sf(xlim=c(-15, 125), ylim=c(22, 68), expand = FALSE, crs=4326) +
  scale_fill_viridis_c(name = "Wing length (mm)") +
  map.theme +
  xlab("") +
  ylab("") + 
  theme(legend.position = "bottom") +
  ggtitle("Nightjar")
plot.euni.wing

#Mass
plot.coni.mass <- ggplot(pred.coni.mass) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray80", colour = "gray80", size=0.3) +
  geom_polygon(data=lake, aes(x=long, y=lat, group=group), fill="white", colour = "white", size=0.3) +
  geom_tile(aes(x=B.Long, y=B.Lat, fill=val)) +
  geom_point(data=sample.coni.mass, aes(x=lonr, y=latr, size=samples+10), pch=21, colour="black", fill="white", alpha = 0.7) +
  coord_sf(xlim=c(-140, -55), ylim=c(5, 68), expand = FALSE, crs=4326) +
  scale_fill_viridis_c(name = "Mass (g)", limits=c(50, 85)) +
  scale_size(limits=c(1, 777), name="Sample size") +
  map.theme +
  xlab("") +
  ylab("") + 
  theme(legend.position = "none") +
  ggtitle("Nighthawk")
plot.coni.mass

plot.euni.mass <- ggplot(pred.euni.mass) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray80", colour = "gray80", size=0.3) +
  geom_polygon(data=lake, aes(x=long, y=lat, group=group), fill="white", colour = "white", size=0.3) +
  geom_tile(aes(x=B.Long, y=B.Lat, fill=val)) +
  geom_point(data=sample.euni.mass, aes(x=lonr, y=latr, size=samples+10), pch=21, colour="black", fill="white", alpha = 1) +
  coord_sf(xlim=c(-15, 70), ylim=c(12, 75), expand = FALSE, crs=4326) +
  scale_fill_viridis_c(name = "Mass (g)", limits=c(50, 85)) +
  scale_size(limits=c(1, 777), name="Sample size") +
  map.theme +
  xlab("") +
  ylab("") + 
  theme(legend.position = "none") +
  ggtitle("Nightjar")
plot.euni.mass

plot.ewpw.mass <- ggplot(pred.ewpw.mass) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray80", colour = "gray80", size=0.3) +
  geom_polygon(data=lake, aes(x=long, y=lat, group=group), fill="white", colour = "white", size=0.3) +
  geom_tile(aes(x=B.Long, y=B.Lat, fill=val)) +
  geom_point(data=sample.ewpw.mass, aes(x=lonr, y=latr, size=samples+10), pch=21, colour="black", fill="white", alpha = 1) +
  coord_sf(xlim=c(-110, -62), ylim=c(24, 60), expand = FALSE, crs=4326) +
  scale_fill_viridis_c(name = "Mass (g)", limits=c(50, 85)) +
  scale_size(limits=c(1, 777), name="Sample size") +
  map.theme +
  xlab("") +
  ylab("") + 
  theme(legend.position = "none") +
  ggtitle("Whip-poor-will")
plot.ewpw.mass

plot.legend.mass <- ggplot(pred.ewpw.mass) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray80", colour = "gray80", size=0.3) +
  geom_polygon(data=lake, aes(x=long, y=lat, group=group), fill="white", colour = "white", size=0.3) +
  geom_tile(aes(x=B.Long, y=B.Lat, fill=val)) +
  geom_point(data=sample.euni.mass, aes(x=lonr, y=latr, size=samples+10), pch=21, colour="black", fill="white", alpha = 1) +
  coord_sf(xlim=c(-110, -62), ylim=c(29, 57), expand = FALSE, crs=4326) +
  scale_fill_viridis_c(name = "Mass (g)", limits=c(50, 85)) +
  scale_size(limits=c(1,777), name="Sample size") +
  map.theme +
  xlab("") +
  ylab("") + 
  theme(legend.position = "bottom")

legend.mass <- cowplot::get_legend(plot.legend.mass)

#10. Put mass figure together----
plot.mass <- grid.arrange(plot.coni.mass, plot.euni.mass, plot.ewpw.mass, legend.mass,
             heights = c(4,1),
             widths = c(4,4,4),
             layout_matrix = rbind(c(1,2,3),
                                   c(4,4,4)))

#11. Save----
ggsave(plot.mass, filename="figs/Isocline_mass.jpeg", width = 12, height = 5)
