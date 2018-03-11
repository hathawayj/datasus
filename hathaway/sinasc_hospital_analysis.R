library(tidyverse)
library(ggthemes)
library(geofacet)
library(trelliscopejs)


load("data/artifacts/hospital.Rdata")

###### by year  ######

n200_hosp <- hosp %>%
  filter(n > 200, !is.na(health_estbl_code)) %>%
  .$health_estbl_code


hosp_year %>%
  filter(health_estbl_code %in% n200_hosp) %>%
  mutate(state_name = fct_reorder(state_name, Cesarean)) %>%
  ggplot(aes(x= birth_year, y = Cesarean, color = region_name)) +
  facet_wrap(~state_name, scales = "free_y") +
  geom_point(aes(size = n)) +
  geom_line(aes(group = health_estbl_code)) +
  theme_bw() 

hosp_year %>%
#filter(health_estbl_code %in% c("2001071", "5701929")) %>%
  filter(health_estbl_code %in% n200_hosp) %>%
  ggplot(aes(x= birth_year, y = Cesarean)) +
    geom_point(aes(y = Vaginal, color = "V")) +
    geom_line(aes(y = Vaginal, color = "V"), show.legend = FALSE) +
    geom_point(aes(y = Cesarean, color = "Cn")) +
    geom_line(aes(y = Cesarean, color = "Cn"), show.legend = FALSE) +
    scale_color_manual(values = c(V = "#75bbfd", Cn = "#a83c09") ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(linetype = NA, size = 4))) +
    labs(x = "Birth year", y = "Proportion", color = "Del. type") +
#    facet_wrap(~health_estbl_code + state_name)
    facet_trelliscope(~health_estbl_code + state_name + birth_muni_code, as_plotly = TRUE, width = 650, path = "../share/docs/trelliscope", nrow = 3, ncol = 3)








