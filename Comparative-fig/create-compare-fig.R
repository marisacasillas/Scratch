library(tidyverse)
library(jtools)

aclew.data <- read_csv("aclew-cds-byrec.csv")
comparison.data <- read_csv("comparison_studies.csv")

aclew.by.3mo <- aclew.data %>%
  group_by(age_3mo, group_corpNE) %>%
  summarize(
   tds_mph = mean(mean.mph) 
  ) %>%
  mutate(
    Reference = "Bunce et al. (in prep)",
    Site = group_corpNE,
    Type = case_when(
      Site == "Tseltal" | Site == "Yeli_Dnye" ~ "Rural, non-WEIRD",
      TRUE ~ "Urban")) %>%
  rename(AgeMonths = age_3mo)

all.comparison.data <- comparison.data %>%
  select(-shape) %>%
  full_join(aclew.by.3mo) %>%
  mutate(
    Site = ifelse(Site == "US", "NA_English", Site),
    Type = as.factor(Type),
    Site = as.factor(Site))
all.comparison.data$Type <- factor(all.comparison.data$Type,
  levels = c("Urban", "Rural, non-WEIRD"))
all.comparison.data$Site <- factor(all.comparison.data$Site,
  levels = c("Yucatec", "Tseltal", "Tsimane", "Yeli_Dnye",
    "MozambiqueRur", "MozambiqueUrb", "Arg_Spanish", "Dutch",
    "UK_English", "NA_English"))
all.comparison.data$Site <- factor(all.comparison.data$Site,
  labels = c("Yucatec", "Tseltal", "Tsimane", "Yélî Dnye",
    "Mozambique (rural)", "Mozambique (urban)", "Arg. Spanish", "Dutch",
    "UK English", "NA English"))

comparison.data.shapes <- c(15, 19)
comparison.data.colors <- c("chartreuse4", "chartreuse2", "chartreuse",
  "springgreen", "green", "tan", "tan2", "tan4", "tomato1", "tomato4")


# TDS min/hr comparison
tdsmph.comparison <- ggplot(all.comparison.data,
  aes(x = AgeMonths, y = tds_mph, color = Site, shape = Type)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_shape_manual(values=comparison.data.shapes) +
  scale_color_manual(values=comparison.data.colors) +
  ylab("All CDS (min/hr)") + xlab("Child age (mo)")	+
  scale_y_continuous(limits=c(0,20),
    breaks=seq(0,20,5)) +
  scale_x_continuous(limits=c(0,38),
    breaks=seq(0,38,6)) +
  coord_cartesian(ylim=c(0,20),xlim=c(0,38)) +
  theme_apa() +
  guides(size = FALSE, shape = FALSE)
tdsmph.comparison
