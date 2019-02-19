library(ggplot2)
library(dplyr)
library(raadtools)
library(tabularaster)
#lats <- graticule::graticule(seq(-180, 180, by = 10), 
#                             seq(-87.5, -22.5, by = 5), tiles = TRUE)

zones_ll <- sf::st_as_sf(aceecostats::aes_zone_ll) %>% #mutate(Zone = case_when(Zone == "High-Latitude" ~ "Polar", 
  #                   Zone == "Mid-Latitude" ~ "Subantarctic", 
  #                 Zone == "Continent" ~ "Polar")) %>% 
  group_by(SectorName) %>% 
  summarize() %>% ungroup() %>% sf::st_cast()


fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- orsifronts::orsifronts$front[fronts$object_]
fronts$sector <- zones_ll$SectorName[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2))

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf"), 
                                                    y = -500 - c(0, 200, 400, 600, 800)), "front")
# 
# ggplot(depth %>% dplyr::filter(depth_hi < -1)) + 
#   geom_line(aes(latlevel, depth_mean)) + 
#   geom_ribbon(aes(latlevel, ymax = depth_hi, 
#                   ymin = depth_lo), alpha = 0.5) + 
#   geom_segment(data = front, lwd = 2, aes(front_lo, y, xend = front_hi, yend = y, col = front)) + 
#   geom_point(data = front, aes(front_median, y)) + 
#   facet_wrap(~sector, ncol = 1)


