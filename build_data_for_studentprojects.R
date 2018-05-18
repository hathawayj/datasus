#' ### Build data for M335, M488, and Data Science Society
#' 
#' This script will read in the big data and save it out as chunks to store on github or data.world
#' 
#' 
# devtools::install_github("datadotworld/data.world-r", build_vignettes = TRUE)
# cat("DW_PAT=JUNKLETTERS\n",
#     file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)
devtools::install_github("karthik/rdrop2")

library(tidyverse)
library(fs)
library(rdrop2)



token <- drop_auth()
write_rds(token, "../db_token.rds")

drop_dir("data")

drop_create("data/brazil")


years <- unique(snsc$birth_year)

for (i in rev(seq_along(years))){
  
  iyear <- years[i]
  ydat <- snsc %>%
    filter(birth_year == iyear)

  ifile_name <- str_c("Brazil_Births_", iyear, "_")
  temp_file <- file_temp(ifile_name, ext = ".csv")
  write_csv(ydat, path = temp_file)
  print(str_c("saved csv file to temp folder - ", temp_file))
  drop_upload(file = temp_file, path = "data/brazil", mute = FALSE)
  #dwapi::upload_data_frame(dataset = "https://data.world/byuidss/brazil-births", data_frame = ydat,
  #                         file_name = str_c("Brazil_Births_", iyear, ".csv"))
}






#' `object.size(iris)` returns an object size that is 3/4 to 1/2 that size on data.world

muni_detail <- readRDS("data/geo/state_muni_latlong_elev_pop2017.Rds") %>%
  select(ibge7_code, ibge6_code, muni_name, state_code, state_name, region_code, region_name)


dwapi::upload_data_frame(dataset = "https://data.world/byuidss/brazil-births", data_frame = muni_detail, file_name = "municipality_details.csv")


if (!"snsc" %in% ls())
  load("data/snsc_all.Rdata")
snsc <- snsc %>% as.tibble()

i = 15



# library(data.world) #library(dwapi) is loaded by default
# data.world::set_config(save_config(auth_token = Sys.getenv("DW_PAT")))
# 
# years <- unique(snsc$birth_year)
# for (i in rev(seq_along(years))){
#   iyear <- years[i]
#   
#   ydat <- snsc %>%
#     filter(birth_year == iyear)
#   print("megabytes")
#   print(object.size(ydat)/1000000)
#   dwapi::upload_data_frame(dataset = "https://data.world/byuidss/brazil-births", data_frame = ydat, 
#                            file_name = str_c("Brazil_Births_", iyear, ".csv"))
# }
# 
# 


dwapi::upload_data_frame(dataset = "https://data.world/byuidss/brazil-births", data_frame = ydat, 
                         file_name = str_c("Brazil_Births", iyear, ".csv"))



# Examples
# dwapi::upload_data_frame(dataset = "https://data.world/byuidss/brazil-births", data_frame = iris, file_name = "iris-1.csv")
# vignette("quickstart", package = "data.world")#

dwapi::upload_data_frame(dataset = "https://data.world/byuidss/brazil-births", data_frame = hosp, file_name = "hospit_11_15.csv")

load("data/artifacts/hospital_snsc20112015.Rdata") 

write_csv(hosp, path = "data/artifacts/hospital_detail.csv")

files_path <- dir_ls("/Users/jhathaway/odrive/Amazon Cloud Drive/data/brazil/byyear")

for (i in seq_along(files_path)){
  ifile <- read_rds(files_path[i])
  write_csv(ifile, path = path_ext_set(files_path[i], "csv"))
  print(i)
}



