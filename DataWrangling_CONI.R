library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)

#1. Read in data----
band <- read_excel("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/tbl_band.xlsx")

all <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/CONIMCP_CleanDataAll.csv")  %>% 
  mutate(Winter2 = case_when(PinpointID %in% c(81, 439, 443, 490, 825, 826, 828) & Season2=="Winter2" ~ 2,
                             PinpointID %in% c(81, 439, 443, 490, 825, 826, 828) & Season2=="Winter" ~ 3,
                             !PinpointID %in% c(81, 439, 443, 490, 825, 826, 828) & Season2=="Winter" ~ 1,
                             is.na(Season2) ~ 0)) %>% 
  dplyr::filter(PinpointID!=829)

pop <- read_excel("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/tbl_population.xlsx") %>% 
  dplyr::select(Population, Region, Country)

df.raw <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/roosting_habitat/Data/CONIMCP_CleanDataAll_Habitat.csv")

#2. Wrangle bird metadata----
birds <- all %>% 
  dplyr::filter(Winter==1,
                !is.na(Winter2)) %>% 
  group_by(PinpointID, Winter2, Year, BandDate, Population, BandLat, BandLong) %>% 
  summarize(WintLat = mean(Lat),
         WintLong = mean(Long)) %>% 
  ungroup() %>% 
  left_join(band) %>% 
  left_join(pop) %>% 
  rename(BandTime=Time,
         SiteName=Region,
         BandNumber=Band,
         WingChord=Wing,
         TailLength=Tail,
         B.Lat = BandLat,
         B.Long=BandLong,
         W.Lat=WintLat,
         W.Long=WintLong,
         TagID=PinpointID) %>% 
  mutate(Project="CONI MCP",
         Recap="N",
         Species="CONI",
         CP=NA,
         BP=NA,
         WingFlat="N",
         Tarsus=NA,
         BandTime = str_sub(as.character(BandTime), -8, -1)) %>% 
  mutate(Wint.Loc = case_when(Winter2==1 ~ 1,
                              Winter2==2 ~ 1,
                              Winter2==3 ~ 2)) %>% 
  dplyr::select(Project, Year, BandDate, Recap, BandTime, Species, SiteName, Country, BandNumber, TagID, Age, Sex, CP, BP, Fat, WingChord, WingFlat, TailLength, Tarsus, Mass, B.Lat, B.Long, W.Lat, W.Long, Wint.Loc, Comments) %>% 
#  dplyr::filter(WingChord > 0) %>% 
  mutate(Fat = ifelse(Fat < 0, NA, Fat),
         TailLength = ifelse(TailLength < 0, NA, TailLength))

#3. Filter out migrations with zero points----
df.n <- df.raw %>% 
  dplyr::filter(PinpointID %in% birds$TagID) %>% 
  arrange(PinpointID, DateTime) %>% 
  dplyr::filter(Season %in% c("SpringMig", "FallMig")) %>% 
  group_by(PinpointID, Season) %>% 
  mutate(n=n()) %>% 
  group_by(PinpointID) %>% 
  ungroup() %>% 
  left_join(df.raw)  %>% 
  rename(TagID = PinpointID) %>% 
  mutate(DateTime = ymd_hms(DateTime),
         doy = yday(DateTime))

table(df.n$TagID, df.n$Season)

#4. ID birds with > 1 wintering ground----
df.wint <- birds %>% 
  dplyr::filter(Wint.Loc==2) %>% 
  dplyr::select(TagID)

#5. Put points together for each bird----
df.spring <- birds %>% 
  dplyr::filter(!(TagID %in% df.wint$TagID & Wint.Loc==1)) %>% 
  dplyr::select(TagID, B.Lat, B.Long, W.Lat, W.Long) %>% 
  pivot_longer(cols=c(B.Lat, W.Lat), names_to = "Season1", values_to="Lat") %>% 
  pivot_longer(cols=c(B.Long, W.Long), names_to = "Season2", values_to="Long") %>% 
  dplyr::filter((Season1=="B.Lat" & Season2=="B.Long")|
                  Season1=="W.Lat" & Season2=="W.Long") %>% 
  mutate(Season = case_when(Season1=="B.Lat" ~ "Breed",
                            Season1=="W.Lat" ~ "Winter"),
         doy = case_when(Season=="Breed" ~ 200,
                         Season=="Winter" ~ 1),
         n = NA) %>% 
  dplyr::select(TagID, Season, doy, Lat, Long, n) %>% 
  inner_join(df.n %>% 
               dplyr::filter(Season=="SpringMig") %>% 
               dplyr::select(TagID) %>% 
               unique()) %>% 
  rbind(df.n %>% 
          dplyr::filter(Season=="SpringMig") %>% 
          dplyr::select(TagID, Season, doy, Lat, Long, n)) %>% 
  arrange(TagID, doy) 

df.fall <- birds %>% 
  dplyr::filter(!(TagID %in% df.wint$TagID & Wint.Loc==2)) %>% 
  dplyr::select(TagID, B.Lat, B.Long, W.Lat, W.Long) %>% 
  pivot_longer(cols=c(B.Lat, W.Lat), names_to = "Season1", values_to="Lat") %>% 
  pivot_longer(cols=c(B.Long, W.Long), names_to = "Season2", values_to="Long") %>% 
  dplyr::filter((Season1=="B.Lat" & Season2=="B.Long")|
                  Season1=="W.Lat" & Season2=="W.Long") %>% 
  mutate(Season = case_when(Season1=="B.Lat" ~ "Breed",
                            Season1=="W.Lat" ~ "Winter"),
         doy = case_when(Season=="Breed" ~ 1,
                         Season=="Winter" ~ 365),
         n = NA) %>% 
  dplyr::select(TagID, Season, doy, Lat, Long, n) %>% 
  inner_join(df.n %>% 
               dplyr::filter(Season=="FallMig") %>% 
               dplyr::select(TagID) %>% 
               unique()) %>% 
  rbind(df.n %>% 
          dplyr::filter(Season=="FallMig") %>% 
          dplyr::select(TagID, Season, doy, Lat, Long, n)) %>% 
  arrange(TagID, doy) 

#6. Loop to calculate distances between all consecutive points----
id.all <- unique(df.spring$TagID)
ID.MD <- mig.pts <- dist <- vector("list", length = length(id.all))
for(i in 1:length(id.all)){
  ID.MD[[i]] <- subset(df.spring, TagID==id.all[i])
  dist[[i]] <- data.frame(rep(id.all[i], nrow(ID.MD[[i]]) - 1),
                          distHaversine(cbind(ID.MD[[i]]$Long, ID.MD[[i]]$Lat)))
}

dist.df.all.spring <- dplyr::bind_rows(dist, .id = "column_label")[,c(2:3)]
colnames(dist.df.all.spring)[1:2] <- c("TagID","dist")
head(dist.df.all.spring)

id.all <- unique(df.fall$TagID)
ID.MD <- mig.pts <- dist <- vector("list", length = length(id.all))
for(i in 1:length(id.all)){
  ID.MD[[i]] <- subset(df.fall, TagID==id.all[i])
  dist[[i]] <- data.frame(rep(id.all[i], nrow(ID.MD[[i]]) - 1),
                          distHaversine(cbind(ID.MD[[i]]$Long, ID.MD[[i]]$Lat)))
}

dist.df.all.fall <- dplyr::bind_rows(dist, .id = "column_label")[,c(2:3)]
colnames(dist.df.all.fall)[1:2] <- c("TagID","dist")
head(dist.df.all.fall)

#7. Summarize per bird----
dist.df.spring <- data.frame(dist.df.all.spring) %>% 
  group_by(TagID) %>% 
  summarize(dist.km = sum(dist)/1000) %>% 
  left_join(df.spring %>% 
              dplyr::filter(n > 0) %>% 
              dplyr::select(TagID, n) %>% 
              unique()) %>% 
  rename(Dist.Spring = dist.km, N.Spring = n)

dist.df.fall <- data.frame(dist.df.all.fall) %>% 
  group_by(TagID) %>% 
  summarize(dist.km = sum(dist)/1000) %>% 
  left_join(df.fall %>% 
              dplyr::filter(n > 0) %>% 
              dplyr::select(TagID, n) %>% 
              unique()) %>% 
  rename(Dist.Fall = dist.km, N.Fall = n)

#8. Extract departure dates----
dates.df <- all %>% 
  dplyr::filter(PinpointID %in% df.n$TagID,
                Type!="Band") %>% 
  group_by(PinpointID, Season2) %>% 
  summarize(mind = min(doy),
            maxd = max(doy)) %>% 
  ungroup() %>% 
  rename(Season=Season2) %>% 
  pivot_longer(cols=mind:maxd, names_to="val", values_to="doy") %>% 
  dplyr::filter(!(Season=="Breed1" & val=="mind"),
                !(Season=="Breed2" & val=="maxd"),
                !(Season=="Winter" & val=="mind" & doy < 250),
                !(Season=="Winter" & val=="maxd" & doy > 200))

dates.n <- table(dates.df$PinpointID, dates.df$Season) %>% 
  data.frame() %>% 
  rename(PinpointID=Var1, Season=Var2) %>% 
  dplyr::filter(Freq>0)

fall.dep <- dates.n %>% 
  dplyr::filter(Season %in% c("Breed1", "FallMig")) %>% 
  group_by(PinpointID) %>% 
  summarize(n=n()) %>% 
  dplyr::filter(n==2) %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID))) %>% 
  left_join(dates.df) %>% 
  dplyr::filter((Season=="Breed1" & val=="maxd") | (Season=="FallMig" & val=="mind")) %>% 
  group_by(PinpointID) %>% 
  summarize(B.dep = mean(doy)) %>% 
  ungroup()

fall.arr <- dates.n %>% 
  dplyr::filter(Season %in% c("FallMig", "Winter")) %>% 
  group_by(PinpointID) %>% 
  summarize(n=n()) %>% 
  dplyr::filter(n==2) %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID))) %>% 
  left_join(dates.df) %>% 
  dplyr::filter((Season=="FallMig" & val=="maxd") | (Season=="Winter" & val=="mind")) %>% 
  group_by(PinpointID) %>% 
  summarize(W.arr = mean(doy)) %>% 
  ungroup()

spring.dep <- dates.n %>% 
  dplyr::filter(Season %in% c("Winter", "SpringMig")) %>% 
  group_by(PinpointID) %>% 
  summarize(n=n()) %>% 
  dplyr::filter(n==2) %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID))) %>% 
  left_join(dates.df) %>% 
  dplyr::filter((Season=="Winter" & val=="maxd") | (Season=="SpringMig" & val=="mind")) %>% 
  group_by(PinpointID) %>% 
  summarize(W.dep = mean(doy)) %>% 
  ungroup()

spring.arr <- dates.n %>% 
  dplyr::filter(Season %in% c("SpringMig", "Breed2")) %>% 
  group_by(PinpointID) %>% 
  summarize(n=n()) %>% 
  dplyr::filter(n==2) %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID))) %>% 
  left_join(dates.df) %>% 
  dplyr::filter((Season=="SpringMig" & val=="maxd") | (Season=="Breed2" & val=="mind")) %>% 
  group_by(PinpointID) %>% 
  summarize(B.arr = mean(doy)) %>% 
  ungroup()

dates.all <- full_join(fall.dep, fall.arr) %>% 
  full_join(spring.dep) %>% 
  full_join(spring.arr) %>% 
  rename(TagID = PinpointID)

#9. Put two seasons together and join back to metadata----
dist.df <- full_join(dist.df.spring, dist.df.fall) %>% 
  right_join(birds) %>% 
  left_join(dates.all) %>% 
  dplyr::select(Project, Year, BandDate, Recap, BandTime, Species, SiteName, Country, BandNumber, TagID, Age, Sex, CP, BP, Fat, WingChord, WingFlat, TailLength, Tarsus, Mass, B.Lat, B.Long, W.Lat, W.Long, Wint.Loc, Dist.Fall, N.Fall, Dist.Spring, N.Spring, B.dep, W.arr, W.dep, B.arr, Comments)

write.csv(dist.df, "/Users/ellyknight/Documents/UoA/Projects/Projects/Morphometrics/DataSheet_CONI_updated.csv", row.names = FALSE)

#10. Figure out which photos to pull----
photoid <- dist.df %>% 
  dplyr::select(BandNumber, Year, Sex, Age, Fat, Mass) %>% 
  left_join(band %>% 
              mutate(Fat = ifelse(Fat==-99, NA, Fat),
                     Mass = ifelse(Mass==-99, NA, Mass)) %>% 
              rename(BandNumber = Band) %>% 
              dplyr::select(BandNumber, Population, ID, DataID, Year, Sex, Age, Fat, Mass)) %>% 
  unique() %>% 
  dplyr::filter(Year > 2016)

photoid <- dist.df %>% 
  dplyr::filter(Year > 2016) %>% 
  arrange(SiteName, BandNumber)

bandid <- band %>% 
  dplyr::filter(ArgosID > 0, Year > 2016) %>% 
  dplyr::select(Population, Band, DataID, ArgosID, PinpointID) %>% 
  arrange(Population, PinpointID)
