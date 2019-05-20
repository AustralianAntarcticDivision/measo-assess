## Production

library(raadtools)
library(sf)
library(measoshapes)
measo_regions03_ll <- dplyr::inner_join(measoshapes::measo_regions03_ll, measo_names, "name")
ext <- extent(-180, 180,-80,-35)
#grid <- raster(ext, res = 1, crs = "+init=epsg:4326")
library(dplyr)
## overall
files <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m") 
files$month <- format(files$date, "%b")
read_chl0 <- function(file) {
  raster::crop(raster::raster(file, varname = "chlor_a"), ext, snap = "out")
}
read_mld0 <- memoise::memoise(readmld)
cn <- tabularaster::cellnumbers(read_chl0(files$fullname[1]), measo_regions03_ll)
cn$area <- raster::area(read_chl0(files$fullname[1]))[cn$cell_] * 1e6
library(furrr)
plan(multiprocess)
energyfun <- function(ff) {  
  chl <- read_chl0(ff$fullname[1])
  mld <- read_mld0()[[ff$month[1]]]
  dplyr::mutate(cn, energy = raster::values(chl * projectRaster(mld, chl[[1]])) * area) %>% group_by(object_) %>% 
    summarize(energy_abundance = mean(energy, na.rm = TRUE), 
              energy_hi = quantile(energy, 0.95, na.rm = TRUE), 
              energy_lo = quantile(energy, 0.05, na.rm = TRUE)) %>% 
    mutate(date = ff$date[1])
}
pea1 <- future_map_dfr(purrr::transpose(files), energyfun)
pea1$zone <- measo_regions03_ll$zone[as.integer(pea1$object_)]
pea1$sector <- measo_regions03_ll$sector[as.integer(pea1$object_)]
saveRDS(pea1, file = "PEA1.rds")
library(dplyr)
SECTORS <- c("EastPacific", "WestAtlantic", "CentralIndian", "EastIndian", "WestPacific")
pea1 <-readRDS("PEA1.rds") %>%  dplyr::filter(!is.na(sector))
pea1$date <- pea1$date + as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
library(ggplot2)
ggplot(pea1 %>% dplyr::filter(zone == "Antarctic"), aes(date, energy_abundance, ymin = energy_lo, ymax = energy_hi, group = zone)) + 
  geom_line() + geom_ribbon(alpha = 0.5) + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(" Overall energy available (abundance of Chlorophyll a mg) \nover time by sector within the Antarctic Zone") + 
  scale_y_log10()
ggsave("pea_za_1.png")

ggplot(pea1 %>% dplyr::filter(zone == "Antarctic"), aes(date, energy_abundance, ymin = energy_lo, ymax = energy_hi, group = zone)) + 
 geom_line() + geom_ribbon(alpha = 0.5) + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(" Overall energy available (log abundance of Chlorophyll a mg) \nover time by sector within the Antarctic Zone") + 
  scale_y_log10()
ggsave("pea_za_1_log.png")

ggplot(pea1 %>% dplyr::filter(zone == "Subantarctic"), aes(date, energy_abundance, group = zone)) + 
  geom_line() + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(" Overall energy available (abundance of Chlorophyll a mg) \nover time by sector within the Subantarctic Zone")
ggsave("pea_za_2.png")


ggplot(pea1 %>% dplyr::filter(zone == "Subantarctic"), aes(date, energy_abundance, group = zone)) + 
  geom_line() + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(" Overall energy available (log abundance of Chlorophyll a mg) \nover time by sector within the Subantarctic Zone")
ggsave("pea_za_2_log.png")


ggplot(pea1 %>% dplyr::filter(zone == "Northern"), aes(date, energy_abundance, group = zone)) + 
  geom_line() + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(" Overall energy available (abundance of Chlorophyll a mg) \nover time by sector within the Northern Zone")
ggsave("pea_za_3.png")


ggplot(pea1 %>% dplyr::filter(zone == "Northern"), aes(date, energy_abundance, group = zone)) + 
  geom_line() + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(" Overall energy available (log abundance of Chlorophyll a mg) \nover time by sector within the Northern Zone")
ggsave("pea_za_3_log.png")


## PEA2
GRID <- calc(raster::brick(l), mean, na.rm = TRUE)

cn <- cellnumbers(GRID, measo_regions03_ll)
latbreaks <- seq(-80.5, -19.5, by = 1)
cn$lon <- xFromCell(GRID, cn$cell_)
cn$lat <- yFromCell(GRID, cn$cell_)
cn$zone <- measo_regions03_ll$zone[cn$object_]
cn$sector <- measo_regions03_ll$sector[cn$object_]
cn <- dplyr::filter(cn, !is.na(zone))
SECTORS <- c("EastPacific", "WestAtlantic", "CentralIndian", "EastIndian", "WestPacific")
cn$sector <- ordered(cn$sector, SECTORS)
cn$zone <- ordered(cn$zone, c("Antarctic", "Subantarctic", "Northern"))

cn$energy <- GRID[cn$cell_]



bigtab <- cn  %>% dplyr::filter(!is.na(zone), !is.na(energy)) %>% 
  group_by(
    latitude = cut(lat, latbreaks),
    
    sector, zone
  ) %>%
  summarize(
    energy_median = mean(energy),
    energy_hi = quantile(energy, 0.95),
    energy_lo  = quantile(energy, 0.05)
  ) %>% ungroup()
bigtab$latitude <- (head(latbreaks, -1)+0.5)[bigtab$latitude]

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- ordered(orsifronts::orsifronts$front[fronts$object_], 
                        c("sbdy", "saccf", "pf", "saf", "stf"))
fronts$sector <- measo_regions03_ll$sector[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(measo_regions03_ll), NA), "Spatial"))]
fronts$energy <- raster::extract(GRID, cbind(fronts$x_, fronts$y_), method = "bilinear")
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            energy = median(energy, na.rm = TRUE))



front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")),  "front")
front <- front %>% dplyr::filter(!is.na(sector))
front$front <- ordered(front$front, c("sbdy", "saccf", "pf", "saf", "stf"))


library(ggplot2)
ggplot(bigtab %>% dplyr::filter(zone == "Antarctic")) + 
  geom_line(aes(latitude, energy_median, group = zone)) + 
  geom_ribbon(aes(latitude, ymax = energy_hi, 
                  ymin = energy_lo, fill = zone, group = zone), alpha = 0.5) + 
  facet_wrap(~ordered(sector, SECTORS), nrow = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, energy, xend = front_hi, yend = energy, col = front)) + 
  geom_point(data = front, aes(front_median, energy))  + 
  ggtitle("Energy available", 
          "mean chlorophyll-a g.m-3 x volume")





## attempt 2 at ## pea_za_1
cn$area <- area(GRID)[cn$cell_]
files <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m") 
files$year <- format(files$date, "%Y")
yfiles <- files %>% group_by(year) %>% mutate(n = n()) %>% dplyr::filter(n == 12) %>% ungroup()

library(furrr)
plan(multiprocess)
l <- future_map(split(yfiles, yfiles$year)[unique(yfiles$year)], function(ff) {  
  chl <- raster::brick(raster::stack(lapply(ff$fullname, function(a) raster::crop(raster::raster(a, varname = "chlor_a"), ext))))
  mld <- readmld()
  calc(chl, mean, na.rm = TRUE) * projectRaster(mld, chl[[1]]) * area(chl[[1]])
})




## pea_za_2

cn$ebreaks <- cut(cn$energy, c(pretty(cn$energy), max(cn$energy)))
cn %>% dplyr::filter(!is.na(energy)) %>% 
  group_by( sector, zone, ebreaks) %>% summarize(area = sum(area))
range(cn$energy)
