library(tidyverse)

#(cl <- detectCores()-1)
#cluster <- create_cluster(cores = cl)


if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")

snsc <- snsc %>% as.tibble()

muni_detail <- readRDS("data/geo/state_muni_latlong_elev_pop2017.Rds") %>%
  select(ibge7_code, ibge6_code, muni_name, state_code, state_name, region_code, region_name) %>%
  rename(birth_muni_code = ibge6_code ,birth_state_code = state_code )

# snsc <- snsc 
#   left_join(muni_detail)


########################################################################
#  Find the relationship between hospital and state for birth types by gestational age.
########################################################################

# Note that count at hospital includes all NAs for other categories.  
# When calculating percentages for the othe groups NAs were removed so counts would be a little different.
  # we have assumed that NAs are missing at random...

hosp <- snsc %>%
  filter(birth_place %in% c("Hospital", "Other Health Estab")) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_year) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = TRUE)) %>%
    ungroup() 

hosp_race <- snsc %>%
  filter(birth_place %in% c("Hospital", "Other Health Estab"), !is.na(race)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_year, race) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_year) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(race, perc, fill = 0)
  
hosp_deliv <- snsc %>%
  filter(birth_place %in% c("Hospital", "Other Health Estab"), !is.na(deliv_type)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_year, deliv_type) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_year) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(deliv_type, perc, fill = 0)

hosp_ga <- snsc %>%
  filter(birth_place %in% c("Hospital", "Other Health Estab"), !is.na(gest_weeks)) %>%
  mutate(gest_weeks = fct_collapse(gest_weeks,
                                   `Less than 27 weeks` = c("22-27 weeks", "Less than 22 weeks"),
                                   `More than 37 weeks` = c("37-41 weeks", "42 weeks and more"),
                                   `28-31 weeks` = "28 to 31 weeks",
                                   `32-36 weeks` = "32-36 weeks")) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_year, gest_weeks) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, birth_year) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(gest_weeks, perc, fill = 0)

hosp_year <- hosp %>%
  left_join(hosp_deliv) %>%
  left_join(hosp_race) %>%
  left_join(hosp_ga) %>%
  left_join(muni_detail)


hosp <- snsc %>%
  filter(birth_place %in% c("Hospital", "Other Health Estab")) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code) %>%
  summarise(n = n(), birth_mean = mean(brthwt_g, na.rm = TRUE)) %>%
  ungroup() 

hosp_race <- snsc %>%
  filter(birth_place %in% c("Hospital", "Other Health Estab"), !is.na(race)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, race) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(race, perc, fill = 0)

hosp_deliv <- snsc %>%
  filter(birth_place %in% c("Hospital", "Other Health Estab"), !is.na(deliv_type)) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, deliv_type) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(deliv_type, perc, fill = 0)

hosp_ga <- snsc %>%
  filter(birth_place %in% c("Hospital", "Other Health Estab"), !is.na(gest_weeks)) %>%
  mutate(gest_weeks = fct_collapse(gest_weeks,
                                   `Less than 27 weeks` = c("22-27 weeks", "Less than 22 weeks"),
                                   `More than 37 weeks` = c("37-41 weeks", "42 weeks and more"),
                                   `28-31 weeks` = "28 to 31 weeks",
                                   `32-36 weeks` = "32-36 weeks")) %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code, gest_weeks) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(birth_state_code, birth_muni_code, health_estbl_code) %>%
  mutate(perc = n/sum(n)) %>%
  select(-n) %>%
  spread(gest_weeks, perc, fill = 0)

hosp <- hosp %>%
  left_join(hosp_deliv) %>%
  left_join(hosp_race) %>%
  left_join(hosp_ga) %>%
  left_join(muni_detail)

save(hosp_year, hosp, file = "data/artifacts/hospital.Rdata")
