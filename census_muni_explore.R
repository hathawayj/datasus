source("_fns.R")
library(ggplot2)
library(ggthemes)
library(forcats)
library(geofacet)

if (!"census" %in% ls())
  load("data/census.Rdata")

load("data/geo/state_muni_codes.Rdata")

d <- filter(census$RENDABR, !is.na(state_code))

# roll up across municipality
muni_inc <- d %>%
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

##
##---------------------------------------------------------

load("data/artifacts/muni_summaries.Rdata")

brthwt_muni_year2 <- brthwt_muni_year %>%
  filter(birth_year %in% c(2001, 2011)) %>%
  mutate(year = birth_year - 1)

muni_inc2 <- filter(muni_inc, year %in% c(2000, 2010))

brthwt_inc <- left_join(muni_inc2, brthwt_muni_year2, by = c(muni_code = "m_muni_code", year = "year"))

filter(brthwt_inc, n >= 100)

plot(sort(log10(brthwt_inc$n)))


ggplot(filter(brthwt_inc, n >= 100), aes(mean_inc, mean_bwt)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_x_log10() +
  facet_wrap(~ year) +
  theme_bw()

ggplot(filter(brthwt_inc, n >= 100), aes(prop_4mw, prop_low_bwt)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~ year) +
  theme_bw()

##
##---------------------------------------------------------

brthwt_muni_deliv_year2 <- brthwt_muni_deliv_year %>%
  filter(birth_year %in% c(2001, 2011)) %>%
  mutate(year = birth_year - 1) %>%
  filter(!is.na(deliv_type) & n >= 100)

brthwt_inc2 <- left_join(brthwt_muni_deliv_year2, muni_inc2, by = c(m_muni_code = "muni_code", year = "year"))

ggplot(brthwt_inc2, aes(mean_inc, mean_bwt)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_x_log10() +
  facet_wrap(~ year) +
  theme_bw()

ggplot(brthwt_inc2, aes(prop_4mw, prop_low_bwt)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_grid(deliv_type ~ year) +
  theme_bw()

##
##---------------------------------------------------------

brthwt_muni_ga_year2 <- brthwt_muni_ga_year %>%
  filter(birth_year %in% c(2001, 2011)) %>%
  mutate(year = birth_year - 1) %>%
  filter(!is.na(gest_weeks) & n >= 100)

brthwt_inc_ga <- left_join(
  brthwt_muni_ga_year2,
  muni_inc2,
  by = c(m_muni_code = "muni_code", year = "year"))

save(brthwt_inc_ga, file = "data/artifacts/muni_census_ga.Rdata")

ggplot(brthwt_inc_ga, aes(mean_inc, mean_bwt)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", span = 1, se = FALSE) +
  # scale_x_log10() +
  facet_grid(year ~ gest_weeks) +
  theme_bw() +
  labs(
    title = "Mean birthweight vs. mean income for every municipality / gestational age at birth combination",
    x = "Mean monthly income (R$)",
    y = "Mean birth weight (g)")

ggplot(brthwt_inc_ga, aes(prop_4mw, prop_low_bwt)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(year ~ gest_weeks) +
  theme_bw() +
  labs(
    title = "Poverty vs. low birth weight for every municipality / gestational age at birth combination",
    x = "Proportion with household income < 1/4 minimum wage",
    y = "Proportion with low birth weight (g)")


filter(brthwt_inc_ga, prop_low_bwt > 0.95 & prop_4mw < 0.05 & gest_weeks == "22-27 weeks")


tmp <- filter(snsc, gest_weeks == "22-27 weeks" & birth_year == 2011 & m_muni_code == 410690)
table(tmp$brthwt_g < 1800)
