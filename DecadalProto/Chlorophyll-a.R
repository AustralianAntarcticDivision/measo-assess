library(raadtools)
library(dplyr)
library(palr)
chlf <- raadtools::ocfiles(time.resolution = "monthly", 
                          product = "MODISA", varname = "CHL", type = "L3m") %>% 
  dplyr::mutate(season = case_when(format(date, "%m") %in% c("10", "11", "12") ~ "Oct-Dec",
                                   format(date, "%m") %in% c("01", "02", "03") ~ "Jan-Mar")) %>% 
  dplyr::filter(!is.na(season)) %>% dplyr::filter(date > (Sys.time()  - 10 * 365.25 * 24 * 3600))
  
ex <-  extent(-180, 180, -80, -29)
if (FALSE) {
  chl <- brick(purrr::map(split(chlf$fullname, chlf$season)[unique(chlf$season)], 
                  ~calc(brick(lapply(.x, function(file) raster::crop(raster(file), ex))), mean, na.rm = TRUE)))

  chl <- readAll(chl)
  saveRDS(chl, "chl.rds")
}

chl <- readRDS("chl.rds")
#pal <- palr::chlPal(palette = TRUE, alpha = 0.5)
#plot(chl, col = pal$cols, breaks = pal$breaks, legend = FALSE, nr = 2)


fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- orsifronts::orsifronts$front[fronts$object_]
fronts$sector <- zones_ll$SectorName[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
fronts$chl <- raster::extract(chl[[1]], cbind(fronts$x_, fronts$y_), method = "bilinear")
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            chl = median(chl, na.rm = TRUE))

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf"), 
                                                    y = 2 - c(0, 0.2, 0.4, 0.6, 0.8)), "front")
                                             

tab <- readRDS("grid_tab.rds")
tab$chl <- raster::extract(aggregate(chl[[1]], fact = 4, fun = mean, na.rm = TRUE), cbind(tab$longitude, tab$latitude), method = "bilinear")
latbreaks <- seq(-80.5, -19.5, by = 1)
chl1 <- tab %>% dplyr::filter(!is.na(chl)) %>% group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(chl_median = median(chl, na.rm = TRUE), 
            chl_lo = quantile(chl, 0.2, na.rm = TRUE), 
            chl_hi = quantile(chl, 0.8, na.rm = TRUE))
chl1$latitude <- (head(latbreaks, -1)+0.5)[chl1$latitude]

library(ggplot2)
ggplot(chl1) + 
  geom_point(aes(latitude, chl_median), pch=".") + 
  geom_ribbon(aes(latitude, ymin = chl_lo, ymax = chl_hi), alpha = 0.5) + 
  geom_segment(data = front, lwd = 2, aes(front_lo, chl, xend = front_hi, yend = chl, col = front)) + 
  geom_point(data = front, aes(front_median, chl)) + 
  
  facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) + 
 # ylim(c(0, 0.2)) + 
  ggtitle("Chlorophyll-a (mg/m-3)", "Chlorophyll Concentration, OCI Algorithm October-December 2009-2018")

ggsave("CHL_Oct-Dec.png")


tab <- readRDS("grid_tab.rds")
tab$chl <- raster::extract(aggregate(chl[[2]], fact = 4, fun = mean, na.rm = TRUE), cbind(tab$longitude, tab$latitude), method = "bilinear")
latbreaks <- seq(-80.5, -19.5, by = 1)
chl2 <- tab %>% dplyr::filter(!is.na(chl)) %>% group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(chl_median = median(chl, na.rm = TRUE), 
            chl_lo = quantile(chl, 0.2, na.rm = TRUE), 
            chl_hi = quantile(chl, 0.8, na.rm = TRUE))
chl2$latitude <- (head(latbreaks, -1)+0.5)[chl2$latitude]

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- orsifronts::orsifronts$front[fronts$object_]
fronts$sector <- zones_ll$SectorName[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
fronts$chl <- raster::extract(chl[[2]], cbind(fronts$x_, fronts$y_), method = "bilinear")
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            chl = median(chl, na.rm = TRUE))

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf"), 
                                                    y = 2 - c(0, 0.2, 0.4, 0.6, 0.8)), "front")


library(ggplot2)
ggplot(chl2) + 
  geom_point(aes(latitude, chl_median), pch=".") + 
  geom_ribbon(aes(latitude, ymin = chl_lo, ymax = chl_hi), alpha = 0.5) + 
  geom_segment(data = front, lwd = 2, aes(front_lo, chl, xend = front_hi, yend = chl, col = front)) + 
  geom_point(data = front, aes(front_median, chl)) + 
  
  facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) + 
  # ylim(c(0, 0.2)) + 
  ggtitle("Chlorophyll-a (mg/m-3)", "Chlorophyll Concentration, OCI Algorithm January-March 2010-2019")
ggsave("CHL_Jan-Mar.png")
