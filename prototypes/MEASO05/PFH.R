library(raadtools)
library(sf)
library(measoshapes)
library(dplyr)
library(ggplot2)
measo_regions_ll <- dplyr::inner_join(measoshapes::measo_regions05_ll, measo_names, "name")
## NOTE: Antarctic on
ext <- extent(-180, 180,-80,-35)
#grid <- raster(ext, res = 1, crs = "+init=epsg:4326")


## overall
files <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m")
#files2 <- ocfiles(time.resolution = "monthly", product = "SeaWiFS", varname = "CHL", type = "L3m")

files$month <- format(files$date, "%b")
read_chl0 <- function(file) {
  raster::crop(raster::raster(file, varname = "chlor_a"), ext, snap = "out")
}
read_mld0 <- memoise::memoise(readmld)
chl <- read_chl0(files$fullname[6])
cn <- tabularaster::cellnumbers(chl, measo_regions_ll)
cn$area <- raster::area(chl)[cn$cell_] * 1e6
ndays <- function(month) setNames(c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), month.abb)[month]


figures <- list()
d <- readRDS("PFH_chla_gt0.5_modis.rds")

SECTORS <- c("EastPacific", "Atlantic", "CentralIndian", "EastIndian", "WestPacific")
Zones <- c("Antarctic", "Subantarctic", "Northern")
subfolder <- "prototypes/MEASO05"
figheight <- 7
figwidth <- 7
figdpi <- 144

# Figure pfh_za_1: Decadal median of status of food habitat (weighted mean
# annual days above food threshold of 0.5 mg.m-3) by latitude in each sector
# within the Antarctic Zone.
#
# Horizontal panel of Summary pfh1 with panel boxes from left to right being
# sectors East Pacific, Atlantic, Indian, East Indian, West Pacific.

d$lat <- yFromCell(chl, d$cellindex)
latbreaks <- seq(-80.5, -19.5, by = 1)
d <- d %>% inner_join(cn, c("cellindex" = "cell_"))
d$sector <- measo_regions_ll$sector[d$object_]
d$zone <- measo_regions_ll$zone[d$object_]

med <- d  %>% group_by(cellindex, lat, sector, zone) %>% 
  summarize(ndays = median(ndays)) %>%  ungroup() %>% 
  group_by(
    latitude = cut(lat, latbreaks),
    
    sector, zone
  ) %>%
  summarize(
    days_mean = mean(ndays),
    days_hi = quantile(ndays, 0.80),
    days_lo  = quantile(ndays, 0.20)
  ) %>% ungroup()
med$latitude <- (head(latbreaks, -1)+0.5)[med$latitude]

labels <- c("pfh_za_1", "pfh_zs_1", "pfh_zn_1")

for (ifig in seq_along(labels)) {
  label <- labels[ifig]
  Zone <- Zones[ifig]
gg <- med %>% dplyr::filter(zone == Zone, !is.na(sector)) %>% 
 ggplot(aes(latitude, days_mean, ymin = days_lo, ymax = days_hi, colour = sector)) + 
  geom_line() + facet_wrap(~ordered(sector, SECTORS), ncol = 1) + 
  ggtitle(glue::glue("Decadal median of status of food habitat \n(weighted mean annual days above food threshold of 0.5 mg.m-3) \nby latitude in each sector within the {Zone} Zone"))


figures[[label]] <- gg
ggsave(file.path(subfolder, sprintf("%s.png", label)), 
       plot = gg, width = figwidth, height = figheight, dpi = figdpi)
}



# Figure pfh_za_2: Overall status of food habitat over time (weighted mean
# annual days above food threshold of 0.5 mg.m-3) by sector within the Antarctic
# Zone.
#
# Horizontal panel of Summary pfh2 with panel boxes from left to right being
# sectors East Pacific, Atlantic, Indian, East Indian, West Pacific.
# 
dd <- d %>% group_by(cellindex, year) %>% 
  summarize(ndays = sum(ndays)) %>% ungroup()

med <- dd %>% inner_join(cn, c("cellindex" = "cell_"))

med$decade <- factor(med$year > 2010, labels = c("2002-2010", "2011-2019"))
med$sector <- measo_regions_ll$sector[med$object_]
med$zone <- measo_regions_ll$zone[med$object_]
med <- med %>% group_by(year, sector, zone) %>% summarize(days = mean(ndays))



labels <- c("pfh_za_2", "pfh_zs_2", "pfh_zn_2")

for (ifig in seq_along(labels)) {
  label <- labels[ifig]
  Zone <- Zones[ifig]
  gg <- ggplot(med %>% dplyr::filter(zone == Zone, !is.na(sector)), aes(year, days)) + geom_point() + geom_line() + 
    facet_wrap(~ordered(sector, SECTORS), ncol = 1) +
    ggtitle(glue::glue("Overall status of food habitat over time \n(weighted mean annual days above food threshold of 0.5 mg.m-3) \nby sector within the {Zone} Zone."))
  
  figures[[label]] <- gg
  ggsave(file.path(subfolder, sprintf("%s.png", label)), 
         plot = gg, width = figwidth, height = figheight, dpi = figdpi)
}



# Figure pfh_za_3: Area (km2) by decadal median status of food habitat (weighted
# mean annual days above food threshold of 0.5 mg.m-3) (austral year 1999-2009;
# 2009-2019) for each sector within the Antarctic Zone.
#
# Horizontal panel of Summary pfh3 with panel boxes from left to right being
# sectors East Pacific, Atlantic, Indian, East Indian, West Pacific.



dd <- d %>% group_by(cellindex, year) %>% summarize(ndays = sum(ndays))
dd <- dd %>% group_by(cellindex) %>% mutate(ndays = median(ndays))

med <- dd %>% ungroup() %>% inner_join(cn, c("cellindex" = "cell_")) %>% 
  group_by(year, object_) %>% summarize(area = sum(area)/1e6, ndays = median(ndays))

med$decade <- factor(med$year > 2010, labels = c("2002-2010", "2011-2019"))
med$sector <- measo_regions_ll$sector[med$object_]
med$zone <- measo_regions_ll$zone[med$object_]


labels <- c("pfh_za_3", "pfh_zs_3", "pfh_zn_3")

for (ifig in seq_along(labels)) {
  label <- labels[ifig]
  Zone <- Zones[ifig]
  gg <-  ggplot(med %>% dplyr::filter(zone == Zone, !is.na(sector)), 
                aes(ndays, weight = area, group = decade, colour = decade)) + geom_density() +
    facet_wrap(~ordered(sector, SECTORS), ncol = 1, scales = "free_y") +
    ylab("Area (km2)") + 
    ggtitle(glue::glue("Area (km2) by decadal median status of food habitat \n(weightedmean annual days above food threshold of 0.5 mg.m-3) \n(austral year 2002-2010; 2011-2019) \nfor each sector within the {Zone} Zone."))
  figures[[label]] <- gg
  ggsave(file.path(subfolder, sprintf("%s.png", label)), 
         plot = gg, width = figwidth, height = figheight, dpi = figdpi)
}




# Figure pfh_za_4: Area (km2) over time of each category of median status of
# food habitat (weighted mean annual days above food threshold of 0.5 mg.m-3)
# (0-100, 101-200, 201-300, >300 days)
#
# Horizontal panel of Summary pfh4 with panel boxes from left to right being
# sectors East Pacific, Atlantic, Indian, East Indian, West Pacific.
# 

dd <- d %>% group_by(cellindex, year) %>% summarize(ndays = sum(ndays))
dd <- dd %>% group_by(cellindex) %>% mutate(ndays = median(ndays)) %>% ungroup()

med <- dd  %>% inner_join(cn, c("cellindex" = "cell_")) %>% 
  mutate(days = cut(ndays, c(0, 100, 200, 300, 366))) 

med$decade <- factor(med$year > 2010, labels = c("2002-2010", "2011-2019"))
med$sector <- measo_regions_ll$sector[med$object_]
med$zone <- measo_regions_ll$zone[med$object_]

med <- med %>%  group_by(decade, days, sector, zone) %>% 
  summarize(area = sum(area)/1e6)



labels <- c("pfh_za_4", "pfh_zs_4", "pfh_zn_4")

for (ifig in seq_along(labels)) {
  label <- labels[ifig]
  Zone <- Zones[ifig]
  gg <-  ggplot(med %>% dplyr::filter(zone == Zone, !is.na(sector)), 
                aes(decade, area, group = days, colour = days)) + geom_point() + geom_line() + 
    facet_wrap(~ordered(sector, SECTORS), ncol = 1) +
    ylab("Area (km2)")  + 
    ggtitle(glue::glue("Area (km2) over time of each category of median status of food habitat \n(weighted mean annual days above food threshold of 0.5 mg.m-3)\nwithin the {Zone} Zone"))
  figures[[label]] <- gg
  ggsave(file.path(subfolder, sprintf("%s.png", label)), 
         plot = gg, width = figwidth, height = figheight, dpi = figdpi)
}

saveRDS(figures, "PFH_figures.rds")



