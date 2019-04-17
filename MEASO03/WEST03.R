library(ggplot2)
library(dplyr)
library(raadtools)
library(tabularaster)
library(measoshapes)
library(future)
library(furrr)


# In order to represent the ocean state at each latitude in a sector, we choose
# February (Feb) as the characteristic month in each year that gives us greatest
# coverage of most ocean data (less sea ice).  We describe attributes relating
# to ocean fronts (sea surface height [SSH, m above mean sea surface 1993-2012],
# sea surface temperature [SST, oC]), water movement (geostrophic speed [m/s]),
# and surface mixing (wind strength [m/s]) .  To calculate these, we undertake
# the following steps for each variable:
#
# average the variable in a cell over February in each of the last 10 years,
# determine the median of these annual February averages to represent the ocean
# state in a cell, and take the median from the cells at that latitude
# (variation represented by the 20th and 80th depth percentiles)

rebuild <- FALSE
if (rebuild) {
  dates <- windfiles() %>% dplyr::filter(date > as.POSIXct("2009-03-01"), 
                                        format(date, "%m") == "02") %>%  mutate(Y = format(date, "%Y")) 
future::plan(multiprocess)  
U <- brick(furrr::future_map(dates$date, ~readwind(.x, lon180 = FALSE, uonly = TRUE, xylim = extent(0, 360,-80,-35))))


GRID <- rotate(calc(U, fun = median, na.rm = TRUE))




library(sf)
measo_regions03_ll <- dplyr::inner_join(measoshapes::measo_regions03_ll, measo_names, "name")

cn <- cellnumbers(GRID, measo_regions03_ll)
latbreaks <- seq(-80.5, -19.5, by = 1)
cn$lon <- xFromCell(GRID, cn$cell_)
cn$lat <- yFromCell(GRID, cn$cell_)
cn$zone <- measo_regions03_ll$zone[cn$object_]
cn$sector <- measo_regions03_ll$sector[cn$object_]
cn <- dplyr::filter(cn, !is.na(zone))
cn$sector <- ordered(cn$sector, c("WestPacific", "EastPacific", "WestAtlantic", "EastAtlantic", 
                                 "CentralIndian", "EastIndian"))
cn$zone <- ordered(cn$zone, c("Antarctic", "Subantarctic", "Northern"))


cn$west <- GRID[cn$cell_]



bigtab <- cn  %>% dplyr::filter(!is.na(zone), !is.na(west)) %>% 
  group_by(
    latitude = cut(lat, latbreaks),
    
    sector, zone
  ) %>%
  summarize(
    west_median = mean(west),
    west_hi = quantile(west, 0.80),
    west_lo  = quantile(west, 0.20)
  ) %>% ungroup()
bigtab$latitude <- (head(latbreaks, -1)+0.5)[bigtab$latitude]

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- ordered(orsifronts::orsifronts$front[fronts$object_], 
                        c("sbdy", "saccf", "pf", "saf", "stf"))
fronts$sector <- measo_regions03_ll$sector[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(measo_regions03_ll), NA), "Spatial"))]
fronts$west <- raster::extract(GRID, cbind(fronts$x_, fronts$y_), method = "bilinear")
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            west = median(west, na.rm = TRUE))



front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")),  "front")
front <- front %>% dplyr::filter(!is.na(sector))
front$front <- ordered(front$front, c("sbdy", "saccf", "pf", "saf", "stf"))

saveRDS(bigtab, file = "MEASO03/west.rds")
saveRDS(front, file = "MEASO03/fronts_west.rds")

}

west <- readRDS("MEASO03/west.rds")
front <- readRDS("MEASO03/fronts_west.rds")

ggplot(west) + #%>% dplyr::filter(depth_hi < -1, zone == "Antarctic")) + 
  geom_line(aes(latitude, west_median, group = zone)) + 
  geom_ribbon(aes(latitude, ymax = west_hi, 
                  ymin = west_lo, fill = zone, group = zone), alpha = 0.5) + 
  facet_wrap(~ordered(sector, c("WestAtlantic", "CentralIndian", "EastIndian", "WestPacific", "EastPacific")), ncol = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, west, xend = front_hi, yend = west, col = front)) + 
  geom_point(data = front, aes(front_median, west))  + 
  ggtitle("Westerly wind strength (m-s)", 
          "west")
ggsave("MEASO03/west03.png")
  
  
 
