# setwd("~/Desktop/brazil_geo")

source("_fns.R")
library(ggplot2)
library(ggthemes)

load("data/geo/state_muni_codes.Rdata")
load("data/geo/state_muni_shapefiles.Rdata")
load("data/artifacts/muni_summaries.Rdata")

muni_codes$muni_code <- substr(muni_codes$muni_code, 1, 6)
muni_codes$muni_code <- as.integer(muni_codes$muni_code)

if (!"census" %in% ls())
  load("data/census.Rdata")
cdat <- filter(census$RENDABR, !is.na(state_code))

ggplot(brthwt_muni_region, aes(rank2, mean_bwt, ymin = q1, ymax = q3, color = region_name)) +
  geom_linerange(color = "gray", alpha = 0.2) +
  geom_point() +
  theme_bw() +
  coord_flip()

ggplot(brthwt_muni_region, aes(rank, mean_bwt, color = region_name)) +
  # geom_linerange(aes(ymin = q1, ymax = q3), color = "gray", alpha = 0.2) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ 1, se = FALSE) +
  theme_bw() +
  scale_color_tableau(guide = FALSE) +
  coord_flip() +
  facet_wrap(~ region_name, scales = "free_y", ncol = 1, strip.position = "left") +
  labs(y = "Mean Birthweight", x = "Municipality Rank (within region)")

##
##---------------------------------------------------------

tmp <- brthwt_muni_ga_year %>%
  # filter(gest_weeks == "32-36 weeks")
  filter(gest_weeks == "37-41 weeks")

tmp <- tmp %>%
  # filter(n >= 10) %>%
  filter(n >= 30) %>%
  group_by(m_muni_code) %>%
  mutate(nn = n()) %>%
  filter(nn == 15) %>%
  dplyr::select(-gest_weeks, -sd_bwt, -q1, -q3, -nn)

tmp <- left_join(tmp, muni_codes, by = c(m_muni_code = "muni_code"))
tmp <- left_join(tmp, state_codes)

cdat2 <- cdat %>%
  filter(year == 2010) %>%
  group_by(muni_code) %>%
  summarise(
    mean_inc_2010 = sum(house_inc) / sum(pop),
    prop_4mw_2010 = sum(pop_4mw) / sum(pop),
    pop_2010 = sum(pop))

tmp <- left_join(tmp, cdat2, by = c(m_muni_code = "muni_code"))

tmp <- filter(tmp, !is.na(muni_name))

tmp <- tmp %>%
  mutate(
    pct_low_bwt = 100 * prop_low_bwt,
    n_log = log10(n),
    pop_2010_log = log10(pop_2010)) %>%
  dplyr::select(-prop_low_bwt, -n, -pop_2010)

library(MASS)
get_slope <- function(x, y) {
  a <- rlm(y ~ x)
  unname(coef(a)[2])
}

# aa <- filter(tmp, muni_name == "Terezópolis de Goiás")
# x <- aa$birth_year
# y <- aa$pct_low_bwt
# get_slope(x, y)

tmp <- tmp %>%
  ungroup() %>%
  group_by(m_muni_code) %>%
  mutate(slope = get_slope(birth_year, pct_low_bwt))

library(trelliscopejs)

# filter(tmp, muni_name == "Terezópolis de Goiás")$slope
# filter(tmp, abs(slope + 0.0932) < 0.01)

bb <- rbind(head(tmp, 300), tail(tmp, 300))

ggplot(tmp, aes(birth_year, pct_low_bwt)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_line(stat = "smooth", method = rlm, color = "blue", size = 1, alpha = 0.5) +
  theme_bw() +
  labs(y = "Percent Low Birth Weight", x = "Year") +
  # facet_wrap(~ state_name + muni_name) +
  facet_trelliscope(~ state_name + muni_name,
    nrow = 2, ncol = 4, width = 400, height = 400,
    name = "pct_low_bwt_muni",
    desc = "percent low birth weight yearly by municipality",
    path = "trelliscope/pct_low_bwt_muni")


## plot slope of pct low birth weight vs. income
##---------------------------------------------------------

get_slope <- function(x, y) {
  a <- MASS::rlm(y ~ x)
  unname(coef(a)[2])
}

tmp2 <- brthwt_muni_ga_year %>%
  filter(gest_weeks == "37-41 weeks") %>%
  filter(n >= 30) %>%
  group_by(m_muni_code) %>%
  mutate(nn = n()) %>%
  filter(nn == 15) %>%
  dplyr::select(-gest_weeks, -sd_bwt, -q1, -q3, -nn) %>%
  summarise(
    pct_low_bwt_mean = 100 * mean(prop_low_bwt),
    slope = get_slope(birth_year, prop_low_bwt * 100),
    n_mean = mean(n)) %>%
  left_join(muni_codes, by = c(m_muni_code = "muni_code")) %>%
  left_join(state_codes) %>%
  filter(!is.na(muni_name))

cdat00 <- cdat %>%
  filter(year == 2000) %>%
  group_by(muni_code) %>%
  summarise(
    mean_inc_2000 = sum(house_inc) / sum(pop),
    prop_4mw_2000 = sum(pop_4mw) / sum(pop),
    pop_2000 = sum(pop))

cdat10 <- cdat %>%
  filter(year == 2010) %>%
  group_by(muni_code) %>%
  summarise(
    mean_inc_2010 = sum(house_inc) / sum(pop),
    prop_4mw_2010 = sum(pop_4mw) / sum(pop),
    pop_2010 = sum(pop))

cdatc <- left_join(cdat00, cdat10)

cdatc$mean_inc_diff <- cdatc$mean_inc_2010 - cdatc$mean_inc_2000

tmp2 <- left_join(tmp2, cdatc, by = c(m_muni_code = "muni_code"))

ggplot(filter(tmp2, mean_inc_diff > -1000), aes(mean_inc_diff, slope)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ state_name)
  # facet_wrap(~ region_name)

ggplot(tmp2, aes(mean_inc_2010, slope)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ state_name)
  # facet_wrap(~ region_name)




##
##---------------------------------------------------------

library(leaflet)

Sys.setenv(MAPBOX_TOKEN = "pk.eyJ1IjoicmhhZmVuIiwiYSI6ImNpdnY5M25oaDAwc24yb281cnFoY3g2YTYifQ.aSlJqMyxuFCtaP6euwu-QA")

pal <- colorFactor(palette = scale_color_tableau()$palette(5),
  levels = levels(brthwt_muni_region$region_code))
labels <- sprintf(
  "<strong>Region: %s</strong><br/>State: %s",
  br_shp$REGIAO, br_shp$ESTADO
) %>% lapply(htmltools::HTML)

leaflet(br_shp) %>%
  setView(
    sum(range(sp::coordinates(br_shp)[, 1])) / 2,
    sum(range(sp::coordinates(br_shp)[, 2])) / 2,
    4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(REGIAO),
    weight = 1,
    opacity = 1,
    color = ~pal(REGIAO),
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~REGIAO, opacity = 0.7, title = "Region",
    position = "bottomright")


# rbokeh::widget2gist('
pal <- colorNumeric("viridis", domain = range(muni_shps$mean_bwt, na.rm = TRUE))
pal(muni_shps$mean_bwt)

labels <- sprintf(
  "<strong>%s</strong><br/>State: %s<br/>Mean birth weight: %g<br/>Number of births: %g",
  muni_shps$NOME, muni_shps$state_name, muni_shps$mean_bwt, muni_shps$n
) %>% lapply(htmltools::HTML)

p <- leaflet(muni_shps) %>%
  setView(
    sum(range(sp::coordinates(muni_shps)[, 1])) / 2,
    sum(range(sp::coordinates(muni_shps)[, 2])) / 2,
    4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(mean_bwt),
    weight = 1,
    opacity = 1,
    color = ~pal(mean_bwt),
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addPolylines(
    data = br_shp,
    weight = 2,
    color = "black"
  ) %>%
  addLegend(pal = pal, values = ~mean_bwt, opacity = 0.7, title = "Birth Weight (g)",
    position = "bottomright")
# ', "Brazil Birth Weight by Municipality")

htmlwidgets::saveWidget(p,
  file = file.path(getwd(), "writeup/maps/muni_brthwt.html"), selfcontained = FALSE)
