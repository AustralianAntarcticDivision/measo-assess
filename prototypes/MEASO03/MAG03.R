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
  dates <- currentsfiles() %>% dplyr::filter(date > as.POSIXct("2009-03-01"), 
                                        format(date, "%m") == "02") %>%  mutate(Y = format(date, "%Y")) %>% split(.$Y)
future::plan(multiprocess)  
U <- brick(furrr::future_map(dates, ~calc(readcurr(.x$date, lon180 = FALSE, uonly = TRUE, xylim = extent(0, 360,-80,-35)), 
                                      fun = median, na.rm = TRUE)))

V <- brick(furrr::future_map(dates, ~calc(readcurr(.x$date, lon180 = FALSE, vonly = TRUE, xylim = extent(0, 360,-80,-35)), 
                                          fun = median, na.rm = TRUE)))


GRID <- rotate(calc(sqrt(U*U + V*V), fun = median, na.rm = TRUE))




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


cn$mag <- GRID[cn$cell_]



bigtab <- cn  %>% dplyr::filter(!is.na(zone), !is.na(mag)) %>% 
  group_by(
    latitude = cut(lat, latbreaks),
    
    sector, zone
  ) %>%
  summarize(
    mag_median = mean(mag),
    mag_hi = quantile(mag, 0.80),
    mag_lo  = quantile(mag, 0.20)
  ) %>% ungroup()
bigtab$latitude <- (head(latbreaks, -1)+0.5)[bigtab$latitude]

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- ordered(orsifronts::orsifronts$front[fronts$object_], 
                        c("sbdy", "saccf", "pf", "saf", "stf"))
fronts$sector <- measo_regions03_ll$sector[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(measo_regions03_ll), NA), "Spatial"))]
fronts$mag <- raster::extract(GRID, cbind(fronts$x_, fronts$y_), method = "bilinear")
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            mag = median(mag, na.rm = TRUE))



front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")),  "front")
front <- front %>% dplyr::filter(!is.na(sector))
front$front <- ordered(front$front, c("sbdy", "saccf", "pf", "saf", "stf"))

saveRDS(bigtab, file = "MEASO03/mag.rds")
saveRDS(front, file = "MEASO03/fronts_mag.rds")

}

mag <- readRDS("MEASO03/mag.rds")
front <- readRDS("MEASO03/fronts_mag.rds")

ggplot(mag) + #%>% dplyr::filter(depth_hi < -1, zone == "Antarctic")) + 
  geom_line(aes(latitude, mag_median, group = zone)) + 
  geom_ribbon(aes(latitude, ymax = mag_hi, 
                  ymin = mag_lo, fill = zone, group = zone), alpha = 0.5) + 
  facet_wrap(~ordered(sector, c("WestAtlantic", "CentralIndian", "EastIndian", "WestPacific", "EastPacific")), ncol = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, mag, xend = front_hi, yend = mag, col = front)) + 
  geom_point(data = front, aes(front_median, mag))  + 
  ggtitle("Water movement (m-s)", 
          "MAG")
ggsave("MEASO03/MAG03.png")
  
  
 
