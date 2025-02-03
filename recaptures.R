library(tidyverse)
library(readxl)
library(lme4)

dat <- read_excel("CONI_MCPv4_band.xlsx")

recap <- dat |> 
  dplyr::filter(Wing!=-99) |> 
  group_by(Band) |> 
  mutate(captures = n()) |> 
  ungroup() |> 
  dplyr::filter(captures > 1,
                Band != "Unbanded") |> 
  mutate(Date = str_sub(Date, 1, 10),
         Time = str_sub(Time, 12, 100),
         Wing = as.numeric(Wing),
         Year = as.numeric(str_sub(Date, 1, 4))) |> 
  dplyr::select(Band, Year, Date, Time, Sex, Lat, Long, Wing, Tail, Mass, BanderID) |> 
  rename(Observer = BanderID)

write.csv(recap, "CONI_MCPv4_recaptures.csv", row.names = FALSE)

mmod <- lmer(Wing ~ Observer + Year + (1|Band), data=recap)
mnull <- lmer(Wing ~ 1 + (1|Band), data=recap)
summary(mmod)
AIC(mmod, mnull)

mod <- lm(Wing ~ Observer + Year, data=recap)
summary(mod)

mod2 <- lm(Wing ~ Observer + Band, data=recap)

summary(mod2)

set.seed(1234)
recap_wide <- recap |> 
  dplyr::select(Band, Wing, Observer, Year) |> 
  group_by(Band) |> 
  sample_n(2) |> 
  mutate(Capture = row_number()) |> 
  ungroup() |> 
  pivot_wider(names_from=Capture, values_from=c(Wing, Observer, Year)) |> 
  mutate(Diff = abs(Wing_1 - Wing_2)) 

ggplot(recap_wide) +
  geom_point(aes(x=Wing_1, y=Wing_2))  +
  geom_smooth(aes(x=Wing_1, y=Wing_2))

cor(recap_wide$Wing_1, recap_wide$Wing_2)

t.test(recap_wide$Wing_1, recap_wide$Wing_2, paired = TRUE)
