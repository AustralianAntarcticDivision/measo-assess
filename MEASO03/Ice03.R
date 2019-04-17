
####################################################################################################
library(raster)
library(dplyr)

## updated seasons was calculated in MEASO02/Ice02.R
if (rebuild) {
read_season <- function(thresh = "15", dp = "/rdsi/PRIVATE/raad/data_local/aad.gov.au/iceseason") {
  stopifnot(is.character(thresh))
  stopifnot(nchar(thresh) == 2)
  files <- fs::dir_ls(dp, glob = "*ice*.rds")
  outfiles <- grep(thresh, files, value = TRUE)
  stopifnot(length(outfiles) == 3)
  keep10 <- function(x) {
    l <- nlayers(x)
    subset(x, (l-9):l)
  }
  retreat <- keep10(readRDS(grep("retreat", outfiles, value = TRUE)))
  advance <- keep10(readRDS(grep("advance", outfiles, value = TRUE)))
  daycount <- keep10(readRDS(grep("daycount", outfiles, value = TRUE)))
  adv <- calc(advance, median, na.rm = TRUE)
  ret <- calc(retreat, median, na.rm = TRUE)
  day <- calc(daycount, median, na.rm = TRUE)
  brick(list(advance = adv, retreat = ret, daycount = day))
}

season05 <- read_season("05")
season15 <- read_season("15")
season25 <- read_season("25")
season75 <- read_season("75")
season95 <- read_season("95")



prj <- projection(season05)
tab <- readRDS("prototypes/DecadalProto/grid_tab.rds")
tab$sector <- NULL
tabxy <- cbind(tab$longitude, tab$latitude)

tab$sector <- zones_ll$sector[ over(SpatialPoints(tabxy), 
                                       as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
tab <- tab %>% dplyr::filter(!is.na(sector))

tab$sector <- ordered(tab$sector, c("WestPacific", "EastPacific", "WestAtlantic", "CentralIndian", "EastIndian"))
tabxy <- reproj::reproj(cbind(tab$longitude, tab$latitude), target = projection(season05), 
                        source = projection(zones_ll))[,1:2, drop = FALSE]

tab[c("advance05", "retreat05")] <- raster::extract(season05, 
                                                    tabxy, 
                                                    method = "bilinear")
tab[c("advance15", "retreat15")] <- raster::extract(season15, 
                                                    tabxy, 
                                                    method = "bilinear")
tab[c("advance25", "retreat25")] <- raster::extract(season25, 
                                                    tabxy, 
                                                    method = "bilinear")
tab[c("advance75", "retreat75")] <- raster::extract(season75, 
                                                    tabxy, 
                                                    method = "bilinear")
tab[c("advance95", "retreat95")] <- raster::extract(season95, 
                                                    tabxy, 
                                                    method = "bilinear")

latbreaks <- seq(-80.5, -19.5, by = 1)
adv_tab<- tab %>% 
  #dplyr::filter(!is.na(advance70)) %>%
  group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(advance15 = median(advance15, na.rm = TRUE), 
            advance25 = median(advance25, na.rm = TRUE), 
            advance75 = median(advance75, na.rm = TRUE), 
            advance95 = median(advance95, na.rm = TRUE), 
            retreat15 = median(retreat15, na.rm = TRUE), 
            retreat25 = median(retreat25, na.rm = TRUE),            
            retreat75 = median(retreat75, na.rm = TRUE), 
            retreat95 = median(retreat95, na.rm = TRUE))

#idx <- which((adv_tab$retreat05 - adv_tab$advance05) < 0)
#if (length(idx) > 0) adv_tab$retreat05[idx] <- NA; adv_tab$advance05[idx] <- NA
idx <- which((adv_tab$retreat15 - adv_tab$advance15) < 0)
if (length(idx) > 0) adv_tab$retreat15[idx] <- NA; adv_tab$advance15[idx] <- NA
idx <- which((adv_tab$retreat25 - adv_tab$advance25) < 0)
if (length(idx) > 0) adv_tab$retreat25[idx] <- NA; adv_tab$advance25[idx] <- NA
idx <- which((adv_tab$retreat75 - adv_tab$advance75) < 0)
if (length(idx) > 0) adv_tab$retreat75[idx] <- NA; adv_tab$advance75[idx] <- NA
idx <- which((adv_tab$retreat95 - adv_tab$advance95) < 0)
if (length(idx) > 0) adv_tab$retreat95[idx] <- NA; adv_tab$advance95[idx] <- NA

adv_tab$latitude <- (head(latbreaks, -1)+0.5)[adv_tab$latitude]

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- orsifronts::orsifronts$front[fronts$object_]
zones_ll <- measoshapes::measo_regions03_ll %>% inner_join(measoshapes::measo_names, "name")

fronts$day <- raster::extract(season75[["daycount"]], 
                              reproj::reproj(cbind(fronts$x_, fronts$y_), prj, source = "+init=epsg:4326")[,1:2, drop = F], method = "bilinear")
fronts$day[!fronts$day > 0] <- NA
fronts$sector <- zones_ll$sector[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), 
                                           as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
front <- fronts %>% group_by(sector, front) %>% 
  
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            day = median(day, na.rm = TRUE))

front$day[is.na(front$day)] <- 0
front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")), 
                                             "front")
front <- front %>% dplyr::filter(!is.na(sector))
front$front <- ordered(front$front, c("sbdy", "saccf", "pf", "saf", "stf"))
saveRDS(adv_tab, file = "MEASO03/ice.rds")
saveRDS(front, file = "MEASO03/fronts_ice.rds")

}

adv_tab <- readRDS("MEASO03/ice.rds")
front <- readRDS("MEASO03/fronts_ice.rds")
library(ggplot2)
ggplot(adv_tab) + 
  geom_segment(aes(latitude, advance15, xend = latitude, yend = retreat15), lwd = 5) + 
  #geom_segment(aes(latitude, advance25, xend = latitude, yend = retreat25), lwd = 4, col = "purple2") + 
  
  geom_segment(aes(latitude, advance75, xend = latitude, yend = retreat75), lwd = 3, col = "#21908C") + 
  geom_segment(aes(latitude, advance95, xend = latitude, yend = retreat95), lwd = 2, col = "yellow3") + 
  
  facet_wrap(~ordered(sector, c("WestAtlantic", "CentralIndian", "EastIndian", "WestPacific", "EastPacific")), ncol = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, day, xend = front_hi, yend = day, col = front)) + 
  geom_point(data = front, aes(front_median, day)) +
  ylab("sea ice season") + 
  xlim(c(-80, -30)) + 
  ggtitle("Zonal-median sea ice season (2009-2019)", "Black: 15%,  Green: 75%, Yellow: 95% concentration")

ggsave("MEASO03/Ice03.png")



