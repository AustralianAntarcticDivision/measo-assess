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

par(mfrow = c(4, 1))
for (asect in unique(front$sector)) {
dd <- depth %>% dplyr::filter(sector == asect) %>% dplyr::select(latitude, depth_mean) 
plot(dd$latitude, dd$depth_mean, type = "l")
ad <- max(dd$depth_mean)
x <- dplyr::filter(front, sector == asect)
for (i in seq_len(nrow(x))) {
  text(x$front_mean[i], (ad - 300 * i) + 130, lab = x$front[i])
  points(x$front_mean[i], ad - 300 * i)
  segments(x$front_lo[i], ad - 300 * i, x$front_hi[i], ad - 300 * i)
  title(asect)
}

}



#db <- dplyr::src_sqlite("/perm_storage/home/acebulk/data/habitat_assessment.sqlite3")
if (!file.exists("depth.rds")) {
  topo <- readtopo("etopo2", xylim = extent(-180, 180,-80,-20))
  
  cn <- cellnumbers(topo, sf::st_as_sf(zones_ll))
  latbreaks <- seq(-80.5, -19.5, by = 1)
  cn$lon <- xFromCell(topo, cn$cell_)
  cn$lat <- yFromCell(topo, cn$cell_)
  cn$zone <- zones_ll$Zone[cn$object_]
  cn$sector <- zones_ll$SectorName[cn$object_]
  cn$depth <- raster::extract(topo, cn$cell_)
  depth <- cn %>% dplyr::filter(depth < 0) %>%
    group_by(
           latitude = cut(lat, latbreaks),
      
 sector
    ) %>%
    summarize(
      depth_mean = mean(depth),
      depth_hi = quantile(depth, 0.80),
      depth_lo  = quantile(depth, 0.20)
    ) %>% ungroup()
  depth$latlevel <- (head(latbreaks, -1)+0.5)[depth$latitude]
  saveRDS(depth, "depth.rds")
}

fronts <- orsifronts::orsifronts

depth <- readRDS("depth.rds")

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf"), 
                                     y = -100 - c(0, 200, 400, 600, 800)), "front")

ggplot(depth %>% dplyr::filter(depth_hi < -1)) + 
  geom_line(aes(latlevel, depth_mean)) + 
  geom_ribbon(aes(latlevel, ymax = depth_hi, 
                  ymin = depth_lo), alpha = 0.5) + 
  geom_segment(data = front, lwd = 2, aes(front_lo, y, xend = front_hi, yend = y, col = front)) + 
  geom_point(data = front, aes(front_median, y)) + 
  facet_wrap(~sector, ncol = 1)


  



library(raadtools)
cfiles <- raadtools::oc_sochla_files() %>% 
  filter(date >= ISOdate(2017, 1, 1, 0, 0, 0), 
         format(date, "%m") %in% c("10", "11", "12"))

chla <- readchla(cfiles$date, xylim = extent(-180, 180, -80, -30))

cn <- cellnumbers(chla, sf::st_as_sf(zones_ll))

cn$lon <- xFromCell(topo, cn$cell_)
cn$lat <- yFromCell(topo, cn$cell_)
cn$zone <- zones_ll$Zone[cn$object_]
cn$sector <- zones_ll$SectorName[cn$object_]
cn$depth <- raster::extract(topo, cn$cell_)
depth <- cn %>% sample_n(1e6) %>%
  group_by(
    latitude = round(lat, digits = 0),
    
    sector, zone
  ) %>%
  summarize(
    depth_mean = mean(depth),
    depth_hi = quantile(depth, 0.80),
    depth_lo  = quantile(depth, 0.20)
  )



if (!file.exists("ice.rds")) {
  library(raadtools)
  icf <- raadfiles::nsidc_monthly_files() %>% 
    dplyr::filter(date >= ISOdatetime(2009, 1, 1, 0, 0, 0, tz = "UTC")) %>% 
    dplyr::filter(format(date, "%m") %in% c("06", "07", "08"))
  
  iceconc <- mean( raadtools::readice_monthly(icf$date))
  
  
  cn <- cellnumbers(iceconc[[1]], sf::st_as_sf(spTransform(lats, projection(iceconc))))
  
  cn$x <- xFromCell(iceconc, cn$cell_)
  cn$y <- yFromCell(iceconc, cn$cell_)
  cn[c("lon", "lat")] <- rgdal::project(as.matrix(cn[c("x", "y")]), projection(iceconc), inv = TRUE)
  
  i <- 1
  #l <- vector("list", nlayers(iceconc))
  # for (i in seq_along(l)) {
  #   cn$iceconc <- raster::extract(iceconc[[i]], cn$cell_)
  
  ice <- cn %>% 
    group_by(latitude = (lat %/% 1) * 1, longitude = forcats::as_factor(as.character((lon %/% 90) * 90))) %>% 
    summarize(ice = mean(iceconc, na.rm = TRUE))
  #}
  saveRDS(ice, "ice.rds")
}



if (!file.exists("sst.rds")) {
  
  sstf <- raadfiles::oisst_monthly_files() %>% 
    dplyr::filter(date >= ISOdatetime(2009, 1, 1, 0, 0, 0, tz = "UTC")) %>% 
    dplyr::filter(format(date, "%m") %in% c("06", "07", "08"))
  
  sstm <- raadtools::readsst(sstf$date, time.resolution = "monthly")
  
  cn <- cellnumbers(sstm[[1]], sf::st_as_sf(lats))
  
  cn$lon <- xFromCell(sstm, cn$cell_)
  cn$lat <- yFromCell(sstm, cn$cell_)
  
  
  l <- vector("list", nlayers(sstm))
  for (i in seq_along(l)) {
    cn$sst<- raster::extract(sstm[[i]], cn$cell_)
    
    l[[i]] <- cn %>% 
      group_by(latitude = lat, longitude  = forcats::as_factor(as.character((lon %/% 90) * 90))) %>% 
      summarize(sst_mean = mean(sst, na.rm = TRUE), 
                sst_hi = quantile(sst, 0.65, na.rm = TRUE), 
                sst_lo = quantile(sst, 0.35, na.rm = TRUE))
  }
  sst <- bind_rows(l, .id = "month") %>% group_by(month, latitude, longitude) %>% summarize_all(mean) %>% dplyr::filter(!is.na(sst_mean))
  
  saveRDS(sst, "sst.rds")
}



sst <- readRDS("sst.rds")

ggplot(sst %>% dplyr::filter(sst_hi < Inf)) + 
  geom_line(aes(latitude, -sst_mean, group = longitude, col = longitude)) + 
  geom_ribbon(aes(latitude, ymax = -sst_hi, ymin = -sst_lo, group = longitude, fill = longitude), alpha = 0.5) + NULL

scale_y_log10()







ice <- readRDS("ice.rds")



ggplot(ice, aes(latitude, ice, group = longitude, col = longitude)) + 
  geom_line()

ggplot(cn %>% mutate(latitude = lat, longitude = forcats::as_factor(as.character((lon %/% 90) * 90))) %>% sample_n(10000), 
       aes(latitude, iceconc, group = longitude, col = longitude)) + 
  geom_point(pch = ".") + 
  geom_smooth()

cn$g <- forcats::as_factor(as.character((cn$lon %/% 90) * 90))
ggplot(cn, aes(lat, sst, group = g, col = g)) + 
  geom_point(pch = ".") + geom_smooth(method = "lm")

#plats <- spTransform(lats, commonprojections$polar)
plot(lats)
lats$lat <- as.integer(round(rgdal::project(coordinates(lats), projection(lats), inv = TRUE)[,2]))
plot(lats)
text(lats, lab = lats$lat, cex = 0.7)
