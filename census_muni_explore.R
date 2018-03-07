source("_fns.R")
library(ggplot2)
library(ggthemes)
library(forcats)
library(geofacet)
library(dplyr)

if (!"census" %in% ls())
  load("data/census.Rdata")

load("data/geo/state_muni_codes.Rdata")

rendabr <- filter(census$RENDABR, !is.na(state_code))

# roll up across municipality
muni_inc <- rendabr %>%
  group_by(muni_code, state_code, year) %>%
  summarise(
    mean_inc = sum(house_inc) / sum(pop),
    prop_4mw = sum(pop_4mw) / sum(pop),
    pop = sum(pop)) %>%
  left_join(state_codes) %>%
  ungroup() %>%
  mutate(state_name = fct_reorder(state_name, mean_inc))

ggplot(muni_inc, aes(state_name, log10(mean_inc))) +
  geom_violin() +
  coord_flip()

muni_inc10 <- muni_inc %>%
  filter(year == 2010) %>%
  mutate(
    state_name = fct_reorder(state_name, mean_inc, .desc = TRUE),
    region_name = fct_reorder(region_name, mean_inc))

ggplot(muni_inc10, aes(state_name, mean_inc, color = region_name)) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip() +
  scale_color_tableau(guide = FALSE) +
  facet_grid(region_name ~ ., scales = "free_y", space = "free_y") +
  theme_bw()

ggplot(filter(muni_inc, year == 2010), aes(state_name, prop_4mw)) +
  geom_boxplot() +
  coord_flip()

plot(sort(filter(muni_inc, year == 2010)$prop_4mw))
hist(filter(muni_inc, year == 2010)$prop_4mw, breaks = 50)

## TODO: look at municipality mean income vs. population density
##---------------------------------------------------------



