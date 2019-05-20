library(ggplot2)
library(dplyr)
library(raadtools)
library(tabularaster)
library(measoshapes)

rebuild <- FALSE
if (rebuild) {
topo <- readtopo("etopo2", xylim = extent(-180, 180,-80,-35))
library(sf)
measo_regions03_ll <- dplyr::inner_join(measo_regions03_ll, measo_names, "name")

cn <- cellnumbers(topo, measo_regions03_ll)
## lookup ice shelf
coast <- SOmap::SOmap_data$ADD_coastline_med
surf <- c("ice_shelf", "land", "ice_shelf", "ice_shelf")
coast_r <- fasterize::fasterize(sf::st_as_sf(coast), raster(coast, res = 1000), field = "gridcode")
cn$type <- surf[raster::extract(coast_r, rgdal::project(xyFromCell(topo, cn$cell_), raster::projection(coast_r)), 
                           method = "simple")]
latbreaks <- seq(-80.5, -19.5, by = 1)
cn$lon <- xFromCell(topo, cn$cell_)
cn$lat <- yFromCell(topo, cn$cell_)
cn$zone <- measo_regions03_ll$zone[cn$object_]
cn$sector <- measo_regions03_ll$sector[cn$object_]
cn <- dplyr::filter(cn, !is.na(zone))
cn$sector <- ordered(cn$sector, c("WestPacific", "EastPacific", "WestAtlantic", "EastAtlantic", 
                                 "CentralIndian", "EastIndian"))
cn$zone <- ordered(cn$zone, c("Antarctic", "Subantarctic", "Northern"))
cn$depth <- raster::extract(topo, cn$cell_)
cn$type[cn$depth >= 0 & is.na(cn$type)] <- "land"
cn$type[cn$depth < 0 & cn$depth > -2000 & is.na(cn$type)] <- "topo_shelf"
cn$type[cn$depth < -2000 & is.na(cn$type)] <- "deep"

depth <- cn  %>% dplyr::filter(depth < 0) %>% 
  group_by(
    latitude = cut(lat, latbreaks),
    
    sector, zone
  ) %>%
  summarize(
    depth_mean = mean(depth),
    depth_hi = quantile(depth, 0.80),
    depth_lo  = quantile(depth, 0.20)
  ) %>% ungroup()
depth$latitude <- (head(latbreaks, -1)+0.5)[depth$latitude]

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- ordered(orsifronts::orsifronts$front[fronts$object_], 
                        c("sbdy", "saccf", "pf", "saf", "stf"))
fronts$sector <- measo_regions03_ll$sector[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(measo_regions03_ll), NA), "Spatial"))]
fronts$depth <- raster::extract(topo, cbind(fronts$x_, fronts$y_), method = "bilinear")
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            depth = median(depth, na.rm = TRUE))

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")),  "front")
front <- front %>% dplyr::filter(!is.na(sector))
front$front <- ordered(front$front, c("sbdy", "saccf", "pf", "saf", "stf"))

saveRDS(depth, file = "MEASO03/geography.rds")
saveRDS(fronts, file = "MEASO03/fronts_geography.rds")

}
ggplot(depth ) + #%>% dplyr::filter(depth_hi < -1, zone == "Antarctic")) + 
  geom_line(aes(latitude, depth_mean, group = zone)) + 
  geom_ribbon(aes(latitude, ymax = depth_hi, 
                  ymin = depth_lo, fill = zone, group = zone), alpha = 0.5) + 
  facet_wrap(~ordered(sector, c("WestAtlantic", "CentralIndian", "EastIndian", "WestPacific", "EastPacific")), ncol = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, depth, xend = front_hi, yend = depth, col = front)) + 
  geom_point(data = front, aes(front_median, depth))  + 
  ggtitle("Geography (m)", 
          "Etopo2 bathymetry")
ggsave("MEASO03/Geography03.png")
  
  
 
