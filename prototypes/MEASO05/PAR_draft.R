files <- raadtools::ocfiles(type = "L3m", product = "MODISA", time.resolution = "monthly", varname = "PAR")
library(raadtools)
library(sf)
library(measoshapes)
measo_regions_ll <- dplyr::inner_join(measoshapes::measo_regions05_ll, measo_names, "name")
ext <- extent(-180, 180,-80,-35)
library(dplyr)

files$month <- format(files$date, "%b")
read_par0 <- function(file) {
  raster::crop(raster::raster(file, varname = "par"), ext, snap = "out")
}
p0  <- read_par0(files$fullname[6])

cn <- tabularaster::cellnumbers(p0, measo_regions_ll)
cn$area <- raster::area(p0)[cn$cell_] * 1e6
cn$latitude <- yFromCell(p0, cn$cell_)
cn$latitude <- as.integer(round(cn$latitude))
ndays <- function(month) setNames(c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), month.abb)[month]

files$year <- as.integer(format(files$date, "%Y"))

read_tab <- function(file, date) {
  tab <- cn
  tab$par <- dplyr::pull(read_par0(file) %>% tabularaster::as_tibble(cell = FALSE, value = TRUE))
  tab %>% dplyr::filter(!is.na(par)) %>%  group_by(object_, latitude) %>% 
    summarize(mean_par = mean(par), sum_par = sum(par), 
              par_lo = quantile(par, 0.2), par_hi = quantile(par, 0.8), 
              par_n = n(), area = sum(area)) %>% mutate(date = date)
}

d <- vector("list", nrow(files))
for (i in seq_along(l)) {
  d[[i]] <- read_tab(files$fullname[i], files$date[i])
  if (i %% 10 == 0)  print(i)
}

d <- do.call(rbind, d)

d$decade <- as.integer(d$year) > 2010
dd <- d %>% group_by(decade, object_, latitude) %>% summarize(sum_par = sum(sum_par), 
                                                  par_lo = quantile(sum_par, 0.2), 
                                                  par_hi = quantile(sum_par, 0.8), 
                                                  area = sum(area), n = sum(par_n)) %>% 
  ungroup()
dd$zone <- measo_regions_ll$zone[dd$object_]
dd$sector <- measo_regions_ll$sector[dd$object_]
dd <- dd %>% filter(!is.na(zone), !is.na(sector))
library(ggplot2)
ggplot(dd %>% filter(zone == "Antarctic"), aes(latitude, sum_par, colour = sector, group = decade)) + geom_line() + 
  facet_grid(sector~.)

