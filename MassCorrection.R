library(tidyverse)
library(lme4)
library(lubridate)
library(suncalc)

#1. Read in data----
band <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/Morphometrics/DataSheet_CONI_Breeding.csv") %>% 
  mutate(Population = as.factor(SiteName),
         DateTime = ymd_hms(paste(as.character(BandDate), BandTime), tz="America/Edmonton"),
         BandDate = as.Date(BandDate),
         year = year(DateTime), 
         jday = yday(DateTime)) %>% 
  filter(Mass > 0,
         !is.na(Mass),
         BandNumber!="Unbanded",
         !is.na(DateTime)) %>% 
  rename(date=BandDate, lat=B.Lat, lon=B.Long)

#2. Get time relative to sunset----
band$sunset <- suncalc::getSunlightTimes(data=band, tz="America/Edmonton")$sunset
band$tssr <- as.numeric(difftime(band$DateTime, band$sunset), units="hours")

use <- band %>% 
  mutate(tssr = as.numeric(ifelse(tssr < -12, tssr+24, tssr))) %>% 
  dplyr::filter(tssr > -5, 
                tssr < 8)
hist(use$tssr)

#3. Look at recaptures----
recap <- use %>% 
  mutate(year = year(DateTime)) %>% 
  group_by(BandNumber, year) %>% 
  summarize(captures = n()) %>% 
  ungroup() %>% 
  dplyr::filter(captures > 1) %>% 
  left_join(use)

#4. Visualize----
ggplot(use) +
  geom_point(aes(x=tssr, y=Mass)) +
  geom_smooth(aes(x=tssr, y=Mass)) +
  ylim(c(45, 100))

ggplot(use) +
  geom_point(aes(x=tssr, y=Mass, colour=factor(round(lat, -1)))) +
  geom_smooth(aes(x=tssr, y=Mass, colour=factor(round(lat, -1)))) +
  ylim(c(45, 100))

ggplot(use) +
  geom_point(aes(x=jday, y=Mass, colour=factor(round(lat, -1)))) +
  geom_smooth(aes(x=jday, y=Mass, colour=factor(round(lat, -1))), method="lm") +
  ylim(c(45, 100)) +
  facet_wrap(~year)

ggplot(recap) +
  geom_point(aes(x=tssr, y=Mass, colour=BandNumber)) +
  geom_line(aes(x=tssr, y=Mass, colour=BandNumber)) +
  geom_smooth(aes(x=tssr, y=Mass))

#5. Ok try with RE for individual----
mod1 <- lmer(Mass ~ tssr + (1|Population/BandNumber), data=use)
mod2 <- lmer(Mass ~ poly(tssr, 2) + (1|Population/BandNumber), data=use)
AIC(mod1, mod2)

#6. Try predicting----
newdat <- data.frame(expand.grid(tssr = seq(round(min(use$tssr), 1), round(max(use$tssr), 1), 0.1),
#                                 jday = seq(min(use$jday), max(use$jday), 1),
                                 Population = unique(use$Population)))
pred <- data.frame(pred.re = predict(mod2, newdat, re.form = ~(1|Population)),
                   pred = predict(mod2, newdat, re.form = ~0)) %>% 
  cbind(newdat)

ggplot() +
  geom_point(aes(x=tssr, y=Mass, colour=Population), data=use) +
  geom_line(aes(x=tssr, y=pred.re, colour=Population), data=pred) +
  geom_line(aes(x=tssr, y=pred), data=pred, colour="black", lwd=2)

#7. Predict to new data----
out <- use %>% 
  mutate(CorrectedMass = predict(mod2, use))

#8. Visualize----
ggplot(out) +
  geom_point(aes(x=Mass, y=CorrectedMass, colour=tssr)) +
  scale_colour_viridis_c()

write.csv(out, "/Users/ellyknight/Documents/UoA/Projects/Projects/Morphometrics/DataSheet_CONI_Breeding_MassCorrection.csv", row.names = FALSE)
