library(raster)
dp <- "/rdsi/PRIVATE/raad/data_local/aad.gov.au/iceseason"

advance70 <- readRDS(file.path(dp, "ice_70_advance.rds"))
retreat70 <- readRDS(file.path(dp, "ice_70_retreat.rds"))
advance95 <- readRDS(file.path(dp, "ice_95_advance.rds"))
retreat95 <- readRDS(file.path(dp, "ice_95_retreat.rds"))


nl <- nlayers(advance70)

advance70 <- subset(advance70, (nl - 9):nl)
retreat70 <- subset(retreat70, (nl - 9):nl)
advance95 <- subset(advance95, (nl - 9):nl)
retreat95 <- subset(retreat95, (nl - 9):nl)
adv_70 <- calc(advance70, median, na.rm = TRUE)
ret_70 <- calc(retreat70, median, na.rm = TRUE)
day_70 <- calc(retreat70 - advance70, median, na.rm = TRUE)

adv_95 <- calc(advance95, median, na.rm = TRUE)
ret_95 <- calc(retreat95, median, na.rm = TRUE)
day_95 <- calc(retreat95 - advance95, median, na.rm = TRUE)

prj <- projection(advance70)
tab <- readRDS("grid_tab.rds")
tab$advance70 <- raster::extract(aggregate(adv_70, fact = 4, fun = mean, na.rm = TRUE), 
                                 reproj::reproj(cbind(tab$longitude, tab$latitude), prj, 
                                                source = "+proj=longlat +datum=WGS84")[, 1:2, drop = FALSE]
                                , method = "bilinear")

tab$advance95 <- raster::extract(aggregate(adv_95, fact = 4, fun = mean, na.rm = TRUE), 
                                 reproj::reproj(cbind(tab$longitude, tab$latitude), prj, 
                                                source = "+proj=longlat +datum=WGS84")[, 1:2, drop = FALSE]
                                 , method = "bilinear")

tab$retreat70 <- raster::extract(aggregate(ret_70, fact = 4, fun = mean, na.rm = TRUE), 
                                 reproj::reproj(cbind(tab$longitude, tab$latitude), prj, 
                                                source = "+proj=longlat +datum=WGS84")[, 1:2, drop = FALSE]
                                 , method = "bilinear")

tab$retreat95 <- raster::extract(aggregate(ret_95, fact = 4, fun = mean, na.rm = TRUE), 
                                 reproj::reproj(cbind(tab$longitude, tab$latitude), prj, 
                                                source = "+proj=longlat +datum=WGS84")[, 1:2, drop = FALSE]
                                 , method = "bilinear")


latbreaks <- seq(-80.5, -19.5, by = 1)
adv_tab<- tab %>% 
  #dplyr::filter(!is.na(advance70)) %>%
  group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(advance70 = median(advance70, na.rm = TRUE), 
            advance95 = median(advance95, na.rm = TRUE), 
            retreat70 = median(retreat70, na.rm = TRUE), 
            retreat95 = median(retreat95, na.rm = TRUE))

adv_tab$latitude <- (head(latbreaks, -1)+0.5)[adv_tab$latitude]
adv70_tab <-  tidyr::gather(adv_tab, "sic70", "value", advance70, advance95)
adv95_tab <-  tidyr::gather(adv_tab, "sic90", "value", advance70, advance95)

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- orsifronts::orsifronts$front[fronts$object_]

fronts$day <- raster::extract(day_70, cbind(fronts$x_, fronts$y_), method = "bilinear")
fronts$sector <- zones_ll$SectorName[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            day = median(day, na.rm = TRUE))

front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf"), 
                                                    y = 360 - c(0, 80, 160, 240, 320)), "front")

library(ggplot2)
ggplot(adv_tab) + 
 geom_segment(aes(latitude, advance70, xend = latitude, yend = retreat70), lwd = 3) + 
  geom_segment(aes(latitude, advance95, xend = latitude, yend = retreat95), lwd = 2, col = "firebrick") + 
  
  facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, day, xend = front_hi, yend = day, col = front)) + 
  geom_point(data = front, aes(front_median, day)) +
  ylab("sea ice season") + 
   xlim(c(-80, -30)) + 
   ggtitle("Zonal-median sea ice season", "Black: 70% concentration, Red: 95% concentration")
     ggsave("ICE-season.png")
   
   
   
