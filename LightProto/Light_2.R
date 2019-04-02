vname <- "Kd_490"

pfiles <- raadtools::ocfiles(time.resolution = "monthly", product = "MODISA", varname = toupper(gsub("_", "", vname)), type = "L3m")
library(furrr)
plan(multiprocess)

library(dplyr)
#pfiles$decade <- as.integer(factor(cut(pfiles$date, aceecostats::aes_decades, labels = FALSE)))
pfiles$decade <- rep(1:2, each = nrow(pfiles)/2)
pfiles <- pfiles %>% dplyr::filter(!is.na(decade))


zones_ll <- sf::st_as_sf(aceecostats::aes_zone_ll) %>% 
  group_by(SectorName) %>% 
  summarize() %>% ungroup() %>% sf::st_cast()

zones_ll$sec <- 1:nrow(zones_ll)
grid <- fasterize::fasterize(zones_ll, raster::raster(raster::extent(-180, 180, -70, -50), res = 0.5), field = "sec")



get_latitudes_var <- function(files, latitudes = c(-65, -55), varname) {
  bb <- raster::extent(-180, 180, min(latitudes) - 2, max(latitudes) + 2)
  f <- function(file) {
    rr <- raster::crop(raster::raster(file, varname = varname), bb)
    tabularaster::as_tibble(rr, xy = TRUE) %>% 
      dplyr::filter(round(y) %in% latitudes) %>% 
      dplyr::transmute(cellvalue, lon = x, lat = round(y), area_km2 = raster::area(rr)[cellindex], cellindex)
  }
  furrr::future_map_dfr(files, f, .id = "year")  
}


months <- sprintf("%02i", 1:12)
decs <- c(1, 2)
l <- vector("list", length(months) * length(decs))
cnt <- 0


for (imonth in seq_along(months)) {
  for (j in seq_along(decs)) {
    mfiles <- dplyr::filter(pfiles, format(date, "%m") == months[imonth], decade == decs[j]) 
    tab <- get_latitudes_var(mfiles$fullname, varname = vname)

  tab$sector <- zones_ll$SectorName[raster::extract(grid, cbind(tab$lon, tab$lat))]
    
    cnt <- cnt + 1
    l[[cnt]] <- tab %>% dplyr::filter(!is.na(sector)) %>% 
      group_by(sector,  lat, year) %>% 
      summarize(#q10 = quantile(cellvalue, 0.1, na.rm = TRUE), 
                sum = sum(cellvalue, na.rm = TRUE),
                mean = mean(cellvalue, na.rm = TRUE), 
                median = quantile(cellvalue, 0.5, na.rm = TRUE), 
                n = n()) %>% 
                #q90 = quantile(cellvalue, 0.9, na.rm = TRUE)) %>% 
      ungroup() %>% mutate(month = months[imonth], decade = decs[j])
    
    
    
  }
}

#areas <- tab %>% dplyr::filter(!is.na(sector)) %>% distinct(cellindex, .keep_all = T)  %>%  
#  group_by(sector, lat) %>% summarize(area_m2 = sum(area_km2) * 1e6)

d <- bind_rows(l) 




if (vname == "par") saveRDS(d, "Light2/PAR.rds")
if (vname == "Kd_490") saveRDS(d, "Light2/KD490.rds")
library(ggplot2)
##PAR
d <- readRDS("Light2/PAR.rds")
#d$par <- d$sum
dd <- d %>% group_by(sector, decade, lat, month) %>% summarize(par= mean(sum, na.rm = TRUE), q10 = quantile(sum, .1), 
                                                               q90 = quantile(sum, 0.9)) %>% ungroup()

#aes_cols <- setNames(aceecostats::aes_zone_cols()$col, aceecostats::aes_zone_cols()$name)

ggplot(dd) + geom_bar(aes(x = month, y = par, fill = factor(decade)), position = "dodge", stat = "identity") + 
  geom_errorbar(aes(x = month, ymin= q10, ymax= q90, group = decade), width = 0.2, position = position_dodge(0.5)) +
  facet_grid(sector~lat)  + 
  ggtitle("Photosynthetically Available Radiation\n R. Frouin einstein day^-1")

ggsave("Light2/PAR.png")


d <- readRDS("Light2/KD490.rds")
d$kd490 <- d$sum
dd <- d %>% group_by(sector, decade, lat, month) %>% summarize(kd490 = mean(sum, na.rm = TRUE), q10 = quantile(sum, .1), 
                                                  q90 = quantile(sum, 0.9)) %>% ungroup()

ggplot(dd) + geom_bar(aes(x = month, y = kd490, fill = factor(decade)), position = "dodge", stat = "identity") + 
  geom_errorbar(aes(x = month, ymin= q10, ymax= q90, group = decade), width = 0.2, position = position_dodge(0.5)) +
  facet_grid(sector~lat)  + 
  ggtitle("Diffuse attenuation coefficient at 490 nm,\n KD2 algorithm m^-1")

ggsave("Light2/KD490.png")

# short par[lon,lat]   (Chunking: [44,22])  (Compression: shuffle,level 4)
# long_name: Photosynthetically Available Radiation, R. Frouin
# scale_factor: 0.0020000000949949
# add_offset: 65.5
# units: einstein m^-2 day^-1
# standard_name: surface_downwelling_photosynthetic_photon_flux_in_air


# short Kd_490[lon,lat]   (Chunking: [44,22])  (Compression: shuffle,level 4)
# long_name: Diffuse attenuation coefficient at 490 nm, KD2 algorithm
# scale_factor: 0.000199999994947575
# add_offset: 0
# units: m^-1
# standard_name: diffuse_attenuation_coefficient_of_downwelling_radiative_flux_in_sea_water
# _FillValue: -32767
# valid_min: 50
# valid_max: 30000
# display_scale: log
# display_min: 0.00999999977648258
# display_max: 5
# unsigned byte palette[eightbitcolor,rgb]   (Contiguous storage)  
# 

## AREAS

# A tibble: 10 x 3
# Groups:   sector [5]
# sector        lat `sum(area_km2)`
# <chr>       <dbl>           <dbl>
#   1 NA            -65          26082.
# 2 NA            -55          14152.
# 3 Atlantic      -65         602486.
# 4 Atlantic      -55         838508.
# 5 EastPacific   -65         266033.
# 6 EastPacific   -55         350263.
# 7 Indian        -65         461645.
# 8 Indian        -55         636841.
# 9 WestPacific   -65         521633.
# 10 WestPacific   -55         707601.


