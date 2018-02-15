source("_fns.R")
library(ggplot2)
library(ggthemes)

if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

load("data/geo/state_muni_codes.Rdata")
load("data/geo/state_muni_shapefiles.Rdata")

brthwt_muni <- snsc %>%
  group_by(m_muni_code) %>%
  summarise(
    meanbwt = mean(brthwt_g, na.rm = TRUE),
    n = length(which(!is.na(brthwt_g))),
    q1 = quantile(brthwt_g, 0.25, na.rm = TRUE),
    q3 = quantile(brthwt_g, 0.75, na.rm = TRUE)) %>%
  filter(!is.na(m_muni_code) & !is.nan(meanbwt)) %>%
  arrange(meanbwt)

muni_codes$code2 <- as.integer(substr(muni_codes$code, 1, 6))

brthwt_muni_region <- brthwt_muni %>%
  left_join(muni_codes, by = c(m_muni_code = "code2")) %>%
  left_join(rename(state_codes, state_name = name), by = c(state_code = "code")) %>%
  filter(!is.na(code)) %>%
  select(-code) %>%
  group_by(region_code) %>%
  mutate(rank = order(meanbwt), region_mnbwt = mean(meanbwt)) %>%
  arrange(state_code, rank) %>%
  ungroup() %>%
  arrange(meanbwt)

brthwt_muni_region$rank2 <- seq_along(brthwt_muni_region$meanbwt)
brthwt_muni_region$region_code <- forcats::fct_reorder(brthwt_muni_region$region_code, brthwt_muni_region$region_mnbwt, mean)

muni_shps <- do.call(rbind, muni_shp)
muni_shps$code <- as.integer(substr(muni_shps$GEOCODIGO, 1, 6))
muni_shps@data <- left_join(muni_shps@data, brthwt_muni, by = c(code = "m_muni_code"))
muni_shps@data <- left_join(muni_shps@data, state_codes, by = c(UF = "code"))

save(brthwt_muni, brthwt_muni_region, muni_shps, file = "data/artifacts/muni_summaries.Rdata")

load("data/artifacts/muni_summaries.Rdata")
