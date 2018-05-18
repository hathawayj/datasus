source("_fns.R")
library(dplyr)
library(forcats)

if (!"census" %in% ls())
  load("data/census.Rdata")

load("data/geo/state_muni_codes.Rdata")
load("data/artifacts/muni_summaries.Rdata")

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

muni_inc2 <- filter(muni_inc, year %in% c(2000, 2010))

## merge 2000/2010 municipality birth weight data with census
##---------------------------------------------------------

brthwt_muni_year2 <- brthwt_muni_year %>%
  filter(birth_year %in% c(2001, 2011)) %>%
  mutate(year = birth_year - 1)

brthwt_inc <- left_join(muni_inc2, brthwt_muni_year2,
  by = c(muni_code = "m_muni_code", year = "year"))

# filter(brthwt_inc, n >= 100)
# plot(sort(log10(brthwt_inc$n)))
# nrow(filter(brthwt_inc, n >= 20)) / nrow(brthwt_inc)

## merge 2000/2010 municipality and ga birth weight data with census
##---------------------------------------------------------

tmp <- brthwt_muni_ga_year %>%
  filter(birth_year %in% c(2001, 2011)) %>%
  mutate(year = birth_year - 1) %>%
  filter(!is.na(gest_weeks) & !is.na(mean_bwt))
brthwt_muni_ga_year2 <- filter(tmp, n >= 2)
# nrow(brthwt_muni_ga_year2) / nrow(tmp)

brthwt_inc_ga <- left_join(
  brthwt_muni_ga_year2,
  muni_inc2,
  by = c(m_muni_code = "muni_code", year = "year"))

## bin 2000 and 2010 municipalities by income
##---------------------------------------------------------

# breaks <- quantile(muni_inc2$mean_inc, c(seq(0, 1, length = 8)))
breaks <- c(0, 200, 400, 800, 1500, 3500)
lbls <- paste0("R$ ", breaks[1:5], "-", breaks[2:6])

hist(muni_inc2$mean_inc)
abline(v = breaks, col = "red")

muni_inc2 <- muni_inc2 %>%
  mutate(income_bin = cut(mean_inc, breaks, labels = lbls))

ga_deliv_year_inc <- snsc %>%
  mutate(
    gest_weeks = fct_collapse(gest_weeks,
   `Less than 27 weeks` = c("22-27 weeks", "Less than 22 weeks"),
   `More than 37 weeks` = c("37-41 weeks", "42 weeks and more"),
   `28-31 weeks` = "28-31 weeks",
   `32-36 weeks` = "32-36 weeks")) %>%
  left_join(filter(muni_inc2, year == 2010), by = c(m_muni_code = "muni_code")) %>%
  group_by(income_bin, gest_weeks, deliv_type, birth_year) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = 2)) %>%
  filter(!is.na(gest_weeks), !is.na(deliv_type)) %>%
  ungroup() %>%
  group_by(income_bin, birth_year) %>%
  mutate(perc = n / sum(n, na.rm = T), pse = sqrt(perc * (1 - perc) / n))

##
##---------------------------------------------------------

# load("data/artifacts/muni_census_ga.Rdata")
save(brthwt_inc, brthwt_inc_ga, ga_deliv_year_inc, file = "data/artifacts/muni_census_ga.Rdata")
