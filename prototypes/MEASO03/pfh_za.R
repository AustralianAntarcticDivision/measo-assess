##pfh_za_3
# Figure pfh_za_3: Area (km2) by decadal median status of food habitat (weighted
# mean annual days above food threshold of 0.5 mg.m-3) (austral year 1999-2009;
# 2009-2019) for each sector within the Antarctic Zone.
#
# Horizontal panel of Summary pfh3 with panel boxes from left to right being
# sectors East Pacific, Atlantic, Indian, East Indian, West Pacific.


library(raadtools)
library(sf)
library(measoshapes)
measo_regions04_ll <- dplyr::inner_join(measoshapes::measo_regions04_ll, measo_names, "name")
## NOTE: Antarctic on
ext <- extent(-180, 180,-80,-50)
#grid <- raster(ext, res = 1, crs = "+init=epsg:4326")
library(dplyr)

## overall
files <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m")
#files2 <- ocfiles(time.resolution = "monthly", product = "SeaWiFS", varname = "CHL", type = "L3m")

files$month <- format(files$date, "%b")
read_chl0 <- function(file) {
  raster::crop(raster::raster(file, varname = "chlor_a"), ext, snap = "out")
}
read_mld0 <- memoise::memoise(readmld)
chl <- read_chl0(files$fullname[6])
cn <- tabularaster::cellnumbers(chl, measo_regions04_ll)
cn$area <- raster::area(chl)[cn$cell_] * 1e6
ndays <- function(month) setNames(c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), month.abb)[month]

files$year <- as.integer(format(files$date, "%Y"))
fflist <- split(files, files$year)
#plan(multiprocess)
# l <- vector("list", length(fflist))
# for (i in seq_along(fflist)) {
#   x <- furrr::future_map(fflist[[i]]$fullname, ~read_chl0(.x))
#   x <- bind_rows(purrr::map(x, tabularaster::as_tibble), .id = "month")
#   x$month <- format(fflist[[i]]$date, "%b")[as.integer(x$month)]
#   x$ndays <- ndays(x$month)
#   x <- x %>% dplyr::filter(!is.na(cellvalue), cellvalue >= 0.5)
#   l[[i]] <- x
#   #mutate(nday = n() * ndays) %>% distinct(cellindex, nday)
#   print(i)
# }
# 
# for (i in seq_along(l)) {
#   
#   l[[i]]$year <- (2002:2019)[i]
# }
l <- readRDS("src/lndays.rds")

d <- bind_rows(l) ##%>% inner_join(cn, c("cellindex" = "cell_"))
dd <- d %>% group_by(cellindex, year) %>% summarize(ndays = sum(ndays))
dd <- dd %>% group_by(cellindex) %>% mutate(ndays = median(ndays))

med <- dd %>% ungroup() %>% inner_join(cn, c("cellindex" = "cell_")) %>% 
  group_by(year, object_) %>% summarize(area = sum(area)/1e6, ndays = median(ndays))

med$decade <- factor(med$year > 2010, labels = c("2002-2010", "2011-2019"))
med$sector <- measo_regions04_ll$sector[med$object_]
med$zone <- measo_regions04_ll$zone[med$object_]

med <- med %>% dplyr::filter(zone == "Antarctic", !is.na(sector)) %>% ungroup()
library(ggplot2)
SECTORS <- c("EastPacific", "Atlantic", "CentralIndian", "EastIndian", "WestPacific")
ggplot(med, aes(ndays, weight = area, group = decade, colour = decade)) + geom_density() +
  facet_wrap(~ordered(sector, SECTORS), ncol = 1, scales = "free_y") +
  ylab("Area (km2)") + 
  ggtitle("Area (km2) by decadal median status of food habitat \n(weightedmean annual days above food threshold of 0.5 mg.m-3) \n(austral year 2002-2010; 2011-2019) \nfor each sector within the Antarctic Zone.")

