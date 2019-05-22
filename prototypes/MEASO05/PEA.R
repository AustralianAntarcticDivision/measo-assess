## Production

library(raadtools)
library(sf)
library(measoshapes)
library(dplyr)
library(ggplot2)

measo_regions_ll <- dplyr::inner_join(measoshapes::measo_regions05_ll, 
                                      measo_names, "name")
ext <- extent(-180, 180,-80,-35)

files <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m") 
files$month <- format(files$date, "%b")
read_chl0 <- function(file) {
  raster::crop(raster::raster(file, varname = "chlor_a"), ext, snap = "out")
}
read_mld0 <- memoise::memoise(readmld)
cn <- tabularaster::cellnumbers(read_chl0(files$fullname[1]), measo_regions_ll)
cn$area <- raster::area(read_chl0(files$fullname[1]))[cn$cell_] * 1e6

SECTORS <- c("EastPacific", "WestAtlantic", "CentralIndian", "EastIndian", "WestPacific")
pea1 <-readRDS("PEA_energy_chl_mld.rds") %>%  dplyr::filter(!is.na(sector))
pea1$date <- pea1$date + as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
pea1$energy <- pea1$energy_abundance

figures <- list()
SECTORS <- c("EastPacific", "Atlantic", "CentralIndian", "EastIndian", "WestPacific")
Zones <- c("Antarctic", "Subantarctic", "Northern")
subfolder <- "prototypes/MEASO05"
figheight <- 7
figwidth <- 7
figdpi <- 144

#ggsave("pea_za_1.png")
labels <- c("pea_za_1", "pea_zs_1", "pea_zn_1")

for (ifig in seq_along(labels)) {
  label <- labels[ifig]
  Zone <- Zones[ifig]
  gg <-
  ggplot(pea1 %>% dplyr::filter(zone == Zone), 
       aes(date, energy, ymin = energy_lo, ymax = energy_hi, group = zone)) + 
  geom_line() + geom_ribbon(alpha = 0.5) + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(glue::glue("Median over decade of the mean energy available per month (abundance of Chlorophyll a mg) by latitude by sector within the {Zone} Zone")) 


figures[[label]] <- gg
ggsave(file.path(subfolder, sprintf("%s.png", label)), 
       plot = gg, width = figwidth, height = figheight, dpi = figdpi)

}

labels <- c("pea_za_2", "pea_zs_2", "pea_zn_2")
for (ifig in seq_along(labels)) {
  label <- labels[ifig]
  Zone <- Zones[ifig]
  
gg <- ggplot(pea1 %>% dplyr::filter(zone == Zone), aes(date, energy_abundance, group = zone)) + 
  geom_line() + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(glue::glue(" Overall energy available (abundance of Chlorophyll a mg) \nover time by sector within the {Zone} Zone"))

figures[[label]] <- gg
ggsave(file.path(subfolder, sprintf("%s.png", label)), 
       plot = gg, width = figwidth, height = figheight, dpi = figdpi)

}


labels <- c("pea_za_3", "pea_zs_3", "pea_zn_3")
for (ifig in seq_along(labels)) {
  label <- labels[ifig]
  Zone <- Zones[ifig]
  
  gg <- ggplot(pea1 %>% dplyr::filter(zone == Zone), aes(date, energy_abundance, group = zone)) + 
    geom_line() + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
    ggtitle(glue::glue(" Area (km2) by median average Chl a for each decade (austral year 1999-2009; 2009-2019) for each sector within the {Zone} Zone"))
  
  figures[[label]] <- gg
  ggsave(file.path(subfolder, sprintf("%s.png", label)), 
         plot = gg, width = figwidth, height = figheight, dpi = figdpi)
  
}



## TO BE DETERMINED classes
labels <- c("pea_za_4", "pea_zs_4", "pea_zn_4")
for (ifig in seq_along(labels)) {
  label <- labels[ifig]
  Zone <- Zones[ifig]
  
#  gg <- ggplot(pea1 %>% dplyr::filter(zone == Zone), 
#               aes(date, area, group = zone)) + 
#    geom_line() + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
#    ggtitle(glue::glue("Area (km2) over time of each category of median average Chl a {Zone} Zone"))
  
 # figures[[label]] <- gg
#  ggsave(file.path(subfolder, sprintf("%s.png", label)), 
#         plot = gg, width = figwidth, height = figheight, dpi = figdpi)
  
}



saveRDS(figures, "PEA_figures.rds")







# ## PEA2
# GRID <- calc(raster::brick(l), mean, na.rm = TRUE)
# 
# cn <- cellnumbers(GRID, measo_regions03_ll)
# latbreaks <- seq(-80.5, -19.5, by = 1)
# cn$lon <- xFromCell(GRID, cn$cell_)
# cn$lat <- yFromCell(GRID, cn$cell_)
# cn$zone <- measo_regions03_ll$zone[cn$object_]
# cn$sector <- measo_regions03_ll$sector[cn$object_]
# cn <- dplyr::filter(cn, !is.na(zone))
# SECTORS <- c("EastPacific", "WestAtlantic", "CentralIndian", "EastIndian", "WestPacific")
# cn$sector <- ordered(cn$sector, SECTORS)
# cn$zone <- ordered(cn$zone, c("Antarctic", "Subantarctic", "Northern"))
# 
# cn$energy <- GRID[cn$cell_]
# 
# 
# 
# bigtab <- cn  %>% dplyr::filter(!is.na(zone), !is.na(energy)) %>% 
#   group_by(
#     latitude = cut(lat, latbreaks),
#     
#     sector, zone
#   ) %>%
#   summarize(
#     energy_median = mean(energy),
#     energy_hi = quantile(energy, 0.95),
#     energy_lo  = quantile(energy, 0.05)
#   ) %>% ungroup()
# bigtab$latitude <- (head(latbreaks, -1)+0.5)[bigtab$latitude]
# 
# fronts <- spbabel::sptable(orsifronts::orsifronts)
# fronts$front <- ordered(orsifronts::orsifronts$front[fronts$object_], 
#                         c("sbdy", "saccf", "pf", "saf", "stf"))
# fronts$sector <- measo_regions03_ll$sector[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(measo_regions03_ll), NA), "Spatial"))]
# fronts$energy <- raster::extract(GRID, cbind(fronts$x_, fronts$y_), method = "bilinear")
# front <- fronts %>% group_by(sector, front) %>% 
#   summarize(front_median = median(y_), 
#             front_hi = quantile(y_, 0.8), 
#             front_lo = quantile(y_, 0.2), 
#             energy = median(energy, na.rm = TRUE))
# 
# 
# 
# front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")),  "front")
# front <- front %>% dplyr::filter(!is.na(sector))
# front$front <- ordered(front$front, c("sbdy", "saccf", "pf", "saf", "stf"))
# 
# 
# library(ggplot2)
# ggplot(bigtab %>% dplyr::filter(zone == "Antarctic")) + 
#   geom_line(aes(latitude, energy_median, group = zone)) + 
#   geom_ribbon(aes(latitude, ymax = energy_hi, 
#                   ymin = energy_lo, fill = zone, group = zone), alpha = 0.5) + 
#   facet_wrap(~ordered(sector, SECTORS), nrow = 1) +
#   geom_segment(data = front, lwd = 2, aes(front_lo, energy, xend = front_hi, yend = energy, col = front)) + 
#   geom_point(data = front, aes(front_median, energy))  + 
#   ggtitle("Energy available", 
#           "mean chlorophyll-a g.m-3 x volume")
# 
# 
# 
# 
# 
# ## attempt 2 at ## pea_za_1
# cn$area <- area(GRID)[cn$cell_]
# files <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m") 
# files$year <- format(files$date, "%Y")
# yfiles <- files %>% group_by(year) %>% mutate(n = n()) %>% dplyr::filter(n == 12) %>% ungroup()
# 
# library(furrr)
# plan(multiprocess)
# l <- future_map(split(yfiles, yfiles$year)[unique(yfiles$year)], function(ff) {  
#   chl <- raster::brick(raster::stack(lapply(ff$fullname, function(a) raster::crop(raster::raster(a, varname = "chlor_a"), ext))))
#   mld <- readmld()
#   calc(chl, mean, na.rm = TRUE) * projectRaster(mld, chl[[1]]) * area(chl[[1]])
# })
# 
# 
# 
# 
# ## pea_za_2
# 
# cn$ebreaks <- cut(cn$energy, c(pretty(cn$energy), max(cn$energy)))
# cn %>% dplyr::filter(!is.na(energy)) %>% 
#   group_by( sector, zone, ebreaks) %>% summarize(area = sum(area))
# range(cn$energy)
