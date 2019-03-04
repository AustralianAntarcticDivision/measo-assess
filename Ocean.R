library(raadtools)
library(dplyr)
f <- sshfiles(ssha = FALSE) %>% dplyr::filter(date > (Sys.time() - 10 * 365.25 * 24 * 3600), 
                                              format(date, "%m") == "02")

if (FALSE) {
  ssh <- readssh(f$date, xylim = extent(0, 360, -80, -29), lon180 = FALSE)
  medssh <- calc(ssh, median, na.rm = TRUE)
  saveRDS(readAll(medssh), "feb_10yr_medssh.rds")
  
}
if (FALSE) {
  ssha <- readssh(f$date, ssha = TRUE, xylim = extent(0, 360, -80, -29), lon180 = FALSE)
  medssha <- calc(ssha, median, na.rm = TRUE)
  saveRDS(readAll(medssha), "feb_10yr_medssha.rds")
  
}
if (FALSE) {
   u <- readcurr(f$date, xylim = extent(0, 360, -80, -29), uonly = TRUE, lon180 = FALSE)
  # medu <- calc(u, median, na.rm = TRUE)
  # saveRDS(medu, "feb_10yr_medu.rds")
  # 
   v <- readcurr(f$date, xylim = extent(0, 360, -80, -29), vonly = TRUE, lon180 = FALSE)
  # medv <- calc(v, median, na.rm = TRUE)
  # saveRDS(medv, "feb_10yr_medv.rds")
  
  mag <- sqrt(u*u + v * v)
  medmag <- calc(mag, median, na.rm = TRUE)
  saveRDS(readAll(medmag), "feb_10yr_medmag.rds")

}
if (FALSE) {
  sst <- readsst(f$date, xylim = extent(0, 360, -80, -29), lon180 = FALSE)
  medsst <- calc(sst, median, na.rm = TRUE)
  saveRDS(readAll(medsst), "feb_10yr_medsst.rds")
}
if (FALSE) {
  wf <- windfiles()
  west <- readwind(f$date, xylim = extent(0, 360, -80, -29), lon180 = FALSE, inputfiles = wf, uonly = TRUE)
  medwest <- calc(west, median, na.rm = TRUE)
  saveRDS(readAll(medwest), "feb_10yr_medwest.rds")
}

ssh <- rotate(readRDS("feb_10yr_medssh.rds"))
ssha <- rotate(readRDS("feb_10yr_medssha.rds"))
mag <- rotate(readRDS("feb_10yr_medmag.rds"))
sst <- rotate(readRDS("feb_10yr_medsst.rds"))
west <- rotate(readRDS("feb_10yr_medwest.rds"))




# prm <- function(x, m) projectRaster(x, m)
# b <- brick(purrr::map(list(ssh, sst, mag, west), prm, m = m))
# plot(b, col = viridis::viridis(200))
# 


tab <- readRDS("grid_tab.rds")
r <- raster(extent(-180, 180, -79.5, -29.5), nrow = 100, ncol = 720, crs = "+proj=longlat +datum=WGS84 +no_defs")
tab$ssh <- extract(ssh, cbind(tab$longitude, tab$latitude), method = "bilinear")
tab$ssha <- extract(ssha, cbind(tab$longitude, tab$latitude), method = "bilinear")
tab$mag <- extract(mag, cbind(tab$longitude, tab$latitude), method = "bilinear")

tab$sst <- extract(sst, cbind(tab$longitude, tab$latitude), method = "bilinear")
tab$west <- extract(west, cbind(tab$longitude, tab$latitude), method = "bilinear")




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
                                                    y = 0.17 - c(0, 2, 4, 6, 8)/58), "front")


latbreaks <- seq(-80.5, -19.5, by = 1)
ssh1 <- tab %>% dplyr::filter(!is.na(ssha)) %>% group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(ssha_median = median(ssha, na.rm = TRUE), 
            ssha_lo = quantile(ssha, 0.2, na.rm = TRUE), 
            ssha_hi = quantile(ssha, 0.8, na.rm = TRUE))
ssh1$latitude <- (head(latbreaks, -1)+0.5)[ssh1$latitude]

ggplot(ssh1) + 
  geom_point(aes(latitude, ssha_median), pch=".") + 
  geom_ribbon(aes(latitude, ymin = ssha_lo, ymax = ssha_hi), alpha = 0.5) + 
  geom_segment(data = front, lwd = 2, aes(front_lo, y, xend = front_hi, yend = y, col = front)) + 
  geom_point(data = front, aes(front_median, y)) + 
  
  facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) + 
  ylim(c(0, 0.2)) + 
  ggtitle("Sea level anomaly (m)", "Sea surface height above mean sea surface 1993-2012 period")
ggsave("SSH.png")




mag1 <- tab %>% dplyr::filter(!is.na(mag)) %>% group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(mag_median = median(mag, na.rm = TRUE), 
            mag_lo = quantile(mag, 0.2, na.rm = TRUE), 
            mag_hi = quantile(mag, 0.8, na.rm = TRUE))
mag1$latitude <- (head(latbreaks, -1)+0.5)[mag1$latitude]

ggplot(mag1) + 
  geom_point(aes(latitude, mag_median), pch=".") + 
  geom_ribbon(aes(latitude, ymin = mag_lo, ymax = mag_hi), alpha = 0.5) + 
  geom_segment(data = front, lwd = 2, aes(front_lo, y, xend = front_hi, yend = y, col = front)) + 
  geom_point(data = front, aes(front_median, y)) + 
  
  facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) + 
  ylim(c(0, 0.5)) + 
  ggtitle("Absolute geostrophic velocity (m/s)", "Sea Surface Height measured by Altimetry and derived variables")
ggsave("EKE.png")



fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- orsifronts::orsifronts$front[fronts$object_]
fronts$sector <- zones_ll$SectorName[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2))

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf"), 
                                                    y = 10), "front")


temp1 <- tab %>% dplyr::filter(!is.na(sst)) %>% group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(sst_median = median(sst, na.rm = TRUE), 
            sst_lo = quantile(sst, 0.2, na.rm = TRUE), 
            sst_hi = quantile(sst, 0.8, na.rm = TRUE))
temp1$latitude <- (head(latbreaks, -1)+0.5)[temp1$latitude]

ggplot(temp1) + 
  geom_point(aes(latitude, sst_median), pch=".") + 
  geom_ribbon(aes(latitude, ymin = sst_lo, ymax = sst_hi), alpha = 0.5) + 
  geom_segment(data = front, lwd = 2, aes(front_lo, y, xend = front_hi, yend = y, col = front)) + 
  geom_point(data = front, aes(front_median, y)) + 
  
  facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) + 
  ggtitle("Surface Temperature (C)", "Daily-OI-V2, Final, Data \n(Ship, Buoy, AVHRR: NOAA19, METOP, NCEP-ice)")
ggsave("SST.png")


fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- orsifronts::orsifronts$front[fronts$object_]
fronts$sector <- zones_ll$SectorName[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2))

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf"), 
                                                    y = 10), "front")


west1 <- tab %>% dplyr::filter(!is.na(west)) %>% group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(west_median = median(west, na.rm = TRUE), 
            west_lo = quantile(west, 0.2, na.rm = TRUE), 
            west_hi = quantile(west, 0.8, na.rm = TRUE))
west1$latitude <- (head(latbreaks, -1)+0.5)[west1$latitude]

ggplot(west1) + 
  geom_point(aes(latitude, west_median), pch=".") + 
  geom_ribbon(aes(latitude, ymin = west_lo, ymax = west_hi), alpha = 0.5) + 
  geom_segment(data = front, lwd = 2, aes(front_lo, y, xend = front_hi, yend = y, col = front)) + 
  geom_point(data = front, aes(front_median, y)) + 
  
  facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) + 
  ggtitle("Westerly wind strength (m/s)", "NCEP/DOE AMIP-II Reanalysis, \n6-Hourly Forecast of U-wind at 10 m")
ggsave("WEST.png")
