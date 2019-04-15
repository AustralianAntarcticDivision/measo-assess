library(ggplot2)
library(dplyr)
library(raadtools)
library(tabularaster)
library(measoshapes)
# dir.create("shp")
# setwd("shp")
# sf::st_write(zones_ll, "zones_ll.shp")
# sf::st_write(zones, "zones.shp")
# setwd("..")
topo <- readtopo("etopo2", xylim = extent(-180, 180,-80,-35))
library(sf)
measo_regions02_ll <- dplyr::inner_join(measo_regions02_ll, measo_names, "name")

cn <- cellnumbers(topo, measo_regions02_ll)
## lookup ice shelf
coast <- SOmap::SOmap_data$ADD_coastline_med
surf <- c("ice_shelf", "land", "ice_shelf", "ice_shelf")
coast_r <- fasterize::fasterize(sf::st_as_sf(coast), raster(coast, res = 1000), field = "gridcode")
cn$type <- surf[raster::extract(coast_r, rgdal::project(xyFromCell(topo, cn$cell_), raster::projection(coast_r)), 
                           method = "simple")]
latbreaks <- seq(-80.5, -19.5, by = 1)
cn$lon <- xFromCell(topo, cn$cell_)
cn$lat <- yFromCell(topo, cn$cell_)
cn$zone <- measo_regions02_ll$zone[cn$object_]
cn$sector <- measo_regions02_ll$sector[cn$object_]
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
fronts$sector <- measo_regions02_ll$sector[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(measo_regions02_ll), NA), "Spatial"))]
fronts$depth <- raster::extract(topo, cbind(fronts$x_, fronts$y_), method = "bilinear")
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            depth = median(depth, na.rm = TRUE))

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")),  "front")
front <- front %>% dplyr::filter(!is.na(sector))


ggplot(depth ) + #%>% dplyr::filter(depth_hi < -1, zone == "Antarctic")) + 
  geom_line(aes(latitude, depth_mean, group = zone)) + 
  geom_ribbon(aes(latitude, ymax = depth_hi, 
                  ymin = depth_lo, fill = zone, group = zone), alpha = 0.5) + 
  facet_wrap(~sector, ncol = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, depth, xend = front_hi, yend = depth, col = front)) + 
  geom_point(data = front, aes(front_median, depth)) 
  
  
 

# 
# 
# 
# 
# library(sf)
# library(dplyr)
# aes_bathy1 <- aceecostats::aes_region_ll
# aes_bathy1 <- sf::as_Spatial(st_cast(sf::st_as_sf(aes_bathy1), "POLYGON"))
# aes_bathy1$id <- 1:nrow(aes_bathy1)
# #plot(aes_bathy1)
# #raster::click(aes_bathy1, n = 5)$id
# aes_bathy1$BathyClass[c(71, 74, 122, 155, 204)] <- "Continent"
# 
# 
# rhumb_width <- function(x) {
#   height <- res(x)[1] * 1852 * 60
#   x[] <- cos(yFromCell(x, 1:ncell(x)) * pi/180) * height
#   x
# }
# 
# aes_bathy <- sf::st_as_sf(aes_bathy1) %>% group_by(BathyClass, SectorName, Zone) %>% 
#   summarize() %>% st_cast()
# 
# 
# aes_bathy$ID <- 1:nrow(aes_bathy)
# rr <- raster(extent(-180, 180, -79.5, -29.5), res = c(.5, .5), crs = "+init=epsg:4326")
# r <- fasterize::fasterize(aes_bathy, 
#                           rr, 
#                           field = "ID")
# 
# topo <- readRDS("gebco_14.rds")
# if (FALSE) {
#   topo <- raadtools::readtopo("gebco_14", xylim = extent(-180, 180, -80, -29))
#   topo <- readAll(topo)
#   saveRDS(topo, "gebco_14.rds", compress = FALSE)
# }
# uni <- sf::st_cast(sf::st_union(aes_bathy), "POLYGON")[2]  ## avoid a tiny part of Antarctica
# nohole <- st_polygon(list(uni[[1]][[1]]))
# ## anything in continent and NA is "Continent"
# continent <- fasterize::fasterize(aes_bathy %>% dplyr::filter(BathyClass == "Continent"), 
#                                   rr)
# 
# ## zap out the continental-land
# mask <- fasterize::fasterize(sf::st_sf(geometry = sf::st_sfc(nohole), id = 1), topo, field = "id")
# lons <- c(-180, -115, -60, 55, 145, 180)
# tab <- tabularaster::as_tibble(r, index = TRUE) %>% 
#   dplyr::mutate(latitude = raster::yFromCell(r, cellindex),
#                 longitude = raster::xFromCell(r, cellindex),
#                 #SectorName = aes_bathy$SectorName[cellvalue], 
#                 #BathyClass = aes_bathy$BathyClass[cellvalue], 
#                 gebco = raster::extract(topo * mask, xyFromCell(r, cellindex)),
#                 bathy = c("Deep", "Bank/Plateau", "Island")[cut(gebco, c(-1e4, -2000, 0, 1e4))],
#                 continent =  !is.na(continent[cellindex]),
#                 width = rhumb_width(rr)[cellindex], 
#                 sector = c("WestPacific", "EastPacific", "Atlantic", "Indian", "WestPacific")[cut(longitude, lons)])
# 
# tab$bathy[tab$continent] <- "Continent"
# 
# tab$bathy <- ordered(tab$bathy, c("Deep", "Bank/Plateau", "Island", "Continent"))
# saveRDS(tab, "grid_tab.rds")
# 
# 
# sec_class <- tab %>% 
#   group_by(latitude, sector, bathy) %>% 
#   summarize(width_km = sum(width/1000, na.rm = TRUE)) %>% 
#   arrange(latitude) %>% dplyr::filter(!is.na(bathy)) %>% ungroup()
# 
# sec_class$dodge <- as.integer(factor(sec_class$bathy)) * 0.2
# ggplot(depth %>% dplyr::filter(depth_hi < -1)) + 
#   geom_line(aes(latitude, depth_mean)) + 
#   geom_ribbon(aes(latitude, ymax = depth_hi, 
#                   ymin = depth_lo), alpha = 0.5) + 
#   #facet_wrap(~sector, ncol = 1)
# 
#   geom_segment(data = front, lwd = 2, aes(front_lo, depth, xend = front_hi, yend = depth, col = front)) + 
#   geom_point(data = front, aes(front_median, depth)) + 
#   
#   
#   facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) + 
#   ## just Islands (scale is relative)
#   geom_segment(data = sec_class  %>% dplyr::filter(!bathy == "Deep") %>% mutate(latitude = as.integer((latitude %/% 1) * 1)) %>% 
#              group_by(latitude, bathy, sector, dodge) %>% summarize(width_km = mean(width_km)), 
#            aes(x = latitude + dodge, xend = latitude+dodge, 
#                y = -5400, yend = -5400 + width_km * 2.5 ,  
#               
#                group = bathy, colour = bathy), size = 1.5, alpha = 0.85)  + 
#   ggtitle("Bathymetry (grey) and relative distance over topography")
# 
# ggsave("Geography.png")
# 
# 
