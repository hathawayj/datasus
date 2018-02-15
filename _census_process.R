
source("_fns.R")
source("_sinasc_dict.R")
load("data/geo/state_muni_codes.Rdata")

ff <- list.files("data/datasus/IBGE/censo", full.names = TRUE, pattern = "00|10")

a <- foreign::read.dbf(ff[1])

