library(tidyverse)
library(ggthemes)
library(fs)
library(ggmaps)
library(webshot)
#https://github.com/ropensci/geonames
library(geonames)
library(osfr)

# Load Data 
load(file = "data/artifacts/hospital_snsc20112015.Rdata") # dat, hosp, state_delivtype

cnes <- read_csv("data/artifacts/CNES_Hospital_Information_2016.csv", locale = locale(encoding = 'ISO-8859-1')) %>%
  mutate(health_estbl_code = parse_character(CNES))

hosp %>% 
  filter(birth_year == "2015") %>%
  group_by(health_estbl_code) %>%
  summarise(birth_state_code = birth_state_code[1],
            birth_muni_code = birth_muni_code[1], 
            )
  
  
  left_join(cnes)






table(hosp$birth_place)







#' @title Get lat, long, and elevation
#' @description For a specified location name get the latitude, longitude, and elevation. Requires the libraries ggmap and geonames.
#' @param x is a location
#' @param image_folder defaults to NULL, If it specificies a folder then
#' @param image_name defaults to NULL. If it is not a specificed then the default image name from the 'webshot' package isused. 
#' @author J. Hathaway
#' @return A data.frame with lat, long, and elevation.
#' @export 

spat_details <- function(x, image_folder = NULL, image_name = NULL){
  
    location <- ggmap::geocode(x)
   
    map_location <- paste0('https://www.google.com/maps/@?api=1&map_action=map&center=', location$lat, ',', location$lon, '&zoom=19&basemap=satellite')
    
    if (!is.null(image_folder)) {
      if (is.null(image_name)) {
        webshot(map_location)
      } else {
        webshot(map_location, file = file.path(image_folder, image_name))
      }
        
    }
  
    height <- GNsrtm3(lat = location$lat, lng = location$lon) # returns meter height
  location$elev_m <- height$srtm3
  location$elev_f <- location$elev_m * 3.28084
  location$map_url <- map_location
  print(x)
  print(map_location)
  location[c("lat", "lon", "elev_m", "elev_f", "map_url")]
}




cnes <- cnes %>%
  mutate(Number = Number %>% str_replace_all("S/N", ""),
    search_address = paste(paste(Name, Address, Number, sep = ","), Municipality, State, sep = "-" ),
    search_address = search_address %>% str_replace_all(",-", ","))


spat_details(cnes$search_address[1], image_folder = "hathaway/hospital_images", "hospital.png")
spat_details(cnes$search_address[4])

spatial_cnes <- cnes %>%
  slice(1:2000) %>%
  split(.$CNES) %>%
  map(~spat_details(.x$search_address))

