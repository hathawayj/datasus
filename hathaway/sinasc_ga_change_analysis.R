library(tidyverse)
library(ggthemes)
library(geofacet)
library(trelliscopejs)


load(file = "data/artifacts/muni_ga_slope_change.Rdata") #ga_change


ga_change %>%
  filter(min_n_ga > 25) %>%
  select(state_code, year, state_name, region_name, `changeto_28 to 31 weeks`, `changeto_32-36 weeks`, `changeto_37-41 weeks`) %>%
  gather(key = change, value = value, -state_code, - year, -state_name, - region_name) %>%
  ggplot(aes(x = fct_reorder(state_name, value), y = value)) +
  geom_jitter(width = .25) +
  facet_wrap(~change) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

# 
# muni_summary %>%
#   filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
#   ggplot(aes(x = fct_reorder(state_name, slope, median), y = slope)) +
#   geom_boxplot() +
#   geom_jitter(width = .25, alpha = .5) +
#   facet_grid(~region_name, scales = "free_x", space = "free_x") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#   labs(x = "State", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)")
# 
# ggsave("hathaway/results/slope_state_region.png", width = 12, height = 6)
# 
# muni_summary %>%
#   filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
#   ggplot(aes(x = mean_inc, y = slope)) +
#   geom_point(aes(color = region_name), alpha = .5) +
#   facet_wrap(~region_name, nrow = 1) +
#   theme_bw() +
#   geom_smooth(se = FALSE) + 
#   labs(x = "Municipality-level average monthly household income (R$)", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)") +
#   theme(legend.position = "none") +
#   scale_color_tableau()
# 
# ggsave("hathaway/results/slope_income_region.png", width = 12, height = 6)
# 
# 
# muni_summary %>%
#   filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
#   ggplot(aes(x = mean_inc, y = slope)) +
#   geom_point(aes(color = region_name), alpha = .5) +
#   theme_bw() +
#   geom_smooth(se = FALSE) + 
#   labs(x = "Municipality-level average monthly household income (R$)", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)",
#        color = "Region") +
#   scale_color_tableau()
# 
# ggsave("hathaway/results/slope_income.png", width = 12, height = 6)
# 
# 
# muni_summary %>%
#   filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
#   ggplot(aes(x = prop_4mw, y = slope)) +
#   geom_point(aes(color = region_name), alpha = .5) +
#   theme_bw() +
#   geom_smooth(method = "lm", se = FALSE) + 
#   facet_wrap(~region_name, nrow = 1) +
#   labs(x = "Percentage with household income less than 1/4 minimum wage", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)",
#        color = "Region") +
#   scale_color_tableau()
# 
# ggsave("hathaway/results/slope_prop4mw.png", width = 12, height = 6)
# 
# 
# muni_summary %>%
#   filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
#   ggplot(aes(x = prop_low_bwt, y = slope)) +
#   geom_point(aes(color = region_name), alpha = .5) +
#   theme_bw() +
#   geom_smooth(method = "lm", se = FALSE) + 
#   facet_wrap(~region_name, nrow = 1) +
#   labs(x = "Percentage of children born with low birthweight", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)",
#        color = "Region") +
#   scale_color_tableau()
# 
# ggsave("hathaway/results/slope_proplowbw.png", width = 12, height = 6)
# 
# 
# ### Need Ryan's Grid
# muni_summary %>%
#   filter(!state_code == "NA", muni_code %in% muni_nlarge) %>%
#   ggplot(aes(x = mean_inc, y = slope)) +
#   geom_point(aes(color = region_name), alpha = .5) +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_bw() +
#   labs(x = "Municipality-level average monthly household income (R$)", y = "Slope: grams per week growth\nusing middle four gestational groups (24, 29, 34, 40)",
#        color = "Region") +
#   theme(legend.position = "none") +
#   scale_color_tableau() +
#   facet_geo(~ state_code, grid = "br_grid1", label = "name") 
# 
