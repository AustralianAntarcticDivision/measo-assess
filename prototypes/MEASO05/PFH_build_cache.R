

library(raadtools)
library(sf)
library(measoshapes)
measo_regions_ll <- dplyr::inner_join(measoshapes::measo_regions05_ll, measo_names, "name")
## NOTE: Antarctic on
ext <- extent(-180, 180,-80,-35)
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
cn <- tabularaster::cellnumbers(chl, measo_regions_ll)
cn$area <- raster::area(chl)[cn$cell_] * 1e6
ndays <- function(month) setNames(c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), month.abb)[month]

files$year <- as.integer(format(files$date, "%Y"))
fflist <- split(files, files$year)
library(furrr)
plan(multicore)

l <- vector("list", length(fflist))
years <- 2002:2019
for (i in seq_along(fflist)) {
  x <- furrr::future_map(fflist[[i]]$fullname, ~read_chl0(.x))
  x <- bind_rows(purrr::map(x, tabularaster::as_tibble), .id = "month")
  x$month <- format(fflist[[i]]$date, "%b")[as.integer(x$month)]
  x$ndays <- ndays(x$month)
  x <- x %>% dplyr::filter(!is.na(cellvalue), cellvalue >= 0.5) %>% 
    mutate(year = years[i])
  l[[i]] <- x
  #mutate(nday = n() * ndays) %>% distinct(cellindex, nday)
  print(i)
}
d <- bind_rows(l)
saveRDS(d, "PFH_chla_gt0.5_modis.rds", compress = FALSE)

