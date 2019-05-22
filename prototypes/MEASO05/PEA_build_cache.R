## Production

library(raadtools)
library(sf)
library(measoshapes)
library(dplyr)
library(ggplot2)

measo_regions_ll <- dplyr::inner_join(measoshapes::measo_regions05_ll, measo_names, "name")
ext <- extent(-180, 180,-80,-35)
## overall
files <- ocfiles(time.resolution = "monthly", product = "MODISA", varname = "CHL", type = "L3m") 
files$month <- format(files$date, "%b")
read_chl0 <- function(file) {
  raster::crop(raster::raster(file, varname = "chlor_a"), ext, snap = "out")
}
read_mld0 <- memoise::memoise(readmld)
cn <- tabularaster::cellnumbers(read_chl0(files$fullname[1]), measo_regions_ll)
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
pea1$zone <- measo_regions_ll$zone[as.integer(pea1$object_)]
pea1$sector <- measo_regions_ll$sector[as.integer(pea1$object_)]
saveRDS(pea1, file = "PEA1.rds")
