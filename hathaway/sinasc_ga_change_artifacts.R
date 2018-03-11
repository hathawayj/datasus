library(tidyverse)


#(cl <- detectCores()-1)
#cluster <- create_cluster(cores = cl)


if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

snsc <- snsc %>% as.tibble()

muni_detail <- readRDS("data/geo/state_muni_latlong_elev_pop2017.Rds") %>%
  select(ibge7_code, ibge6_code, muni_name, state_code, state_name, region_code, region_name) %>%
  rename(birth_muni_code = ibge6_code ,birth_state_code = state_code )

load("data/artifacts/muni_census_ga.Rdata") # brthwt_inc and brthwt_inc_ga in the Rdata file

########################################################################
#  Is the birth weight relationship of the middle 4 gestational age groups similar across municipalities?
# Use income by municipality as well.
########################################################################

gest_state_muni <- snsc %>%
  filter(!gest_weeks %in% c("Less than 22 weeks", "42 weeks and more"), !is.na(gest_weeks)) %>%
  group_by(gest_weeks, m_state_code, m_muni_code) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gest_weeks_number = case_when(gest_weeks == "22-27 weeks" ~ 24,
                                       gest_weeks == "28 to 31 weeks" ~ 29,
                                       gest_weeks == "32-36 weeks" ~ 34,
                                       gest_weeks == "37-41 weeks" ~ 40)) 

muni_nlarge <- gest_state_muni %>%
  group_by(m_muni_code) %>%
  summarise(min_n = min(n)) %>%
  filter(min_n > 25) %>%
  .$m_muni_code


### Calculate Change between groups  ####


gest_change <- gest_state_muni %>%
  arrange(m_state_code, m_muni_code, gest_weeks) %>%
  select(gest_weeks, m_state_code, m_muni_code, birth_mean) %>%
  spread(key = gest_weeks, value = birth_mean) %>%
  mutate(`changeto_28 to 31 weeks`  = `28 to 31 weeks` - `22-27 weeks` ,
         `changeto_32-36 weeks` = `32-36 weeks` - `28 to 31 weeks`,
         `changeto_37-41 weeks` = `37-41 weeks` - `32-36 weeks`) %>%
  select(m_state_code, m_muni_code,`changeto_28 to 31 weeks`, `changeto_32-36 weeks`, `changeto_37-41 weeks`)

gest_minn <- gest_state_muni %>%
  group_by(m_state_code, m_muni_code) %>%
  summarise(min_n_ga = min(n), n = sum(n)) 
  
gest_change <- gest_change %>%
  left_join(gest_minn) %>%
  rename(muni_code = m_muni_code)
  

## Calculate Slopes ##

muni_slopes <- gest_state_muni %>%
  split(.$m_muni_code) %>%
  #  .[1:2] %>%
  map(~ lm(birth_mean ~ gest_weeks_number, data = .)) %>%
  map("coefficients") %>%
  map("gest_weeks_number") %>%
  do.call("rbind", .) 

# https://www.babycenter.com/average-fetal-length-weight-chart
muni_slopes <- muni_slopes %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as.tibble() %>%
  rename(muni_code = rowname, slope = V1) %>%
  mutate(muni_code = as.integer(muni_code))

ga_change <- brthwt_inc %>%
  filter(year == 2010) %>%
  select(-n) %>%
  left_join(muni_slopes) %>%
  left_join(gest_change)


save(ga_change, file = "data/artifacts/muni_ga_slope_change.Rdata")


