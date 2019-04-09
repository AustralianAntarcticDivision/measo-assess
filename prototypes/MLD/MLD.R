mldf <- "/rdsi/PUBLIC/raad/data/sose.ucsd.edu/SO6/ITER122/bsose_i122_2013to2017_monthly_MLD.nc"

library(tidync)
s55 <- tidync::tidync(mldf) %>% hyper_filter(YC = between(index, 373, 384)) %>% hyper_tibble()
s65 <- tidync::tidync(mldf) %>% hyper_filter(YC = between(index, 250, 265)) %>% hyper_tibble()
ss <- bind_rows(s55, s65)
rm(s55, s65)
ss$date <- as.POSIXct("2012-12-01") + ss$time

ss$latlat <- cut(ss$YC, c(-70, -60, -50), labels = c("65S", "55S"))
zones_ll <- sf::st_as_sf(aceecostats::aes_zone_ll) %>% 
  group_by(SectorName) %>% 
  summarize() %>% ungroup() %>% sf::st_cast()

zones_ll$sec <- 1:nrow(zones_ll)
grid <- fasterize::fasterize(zones_ll, raster::raster(raster::extent(-180, 180, -70, -50), res = 0.5), field = "sec")
ss$lon <- ss$XC
ss$lon[ss$lon> 180] <- ss$lon[ss$lon> 180] - 360
ss$sector <- zones_ll$SectorName[raster::extract(grid, cbind(ss$lon, ss$YC))]

ss <- dplyr::filter(ss, !is.na(sector))
aes_cols <- setNames(aceecostats::aes_zone_cols()$col, aceecostats::aes_zone_cols()$name)
ggplot(ss %>%  group_by(date, sector, latlat) %>% summarize(BLGMLD = mean(BLGMLD)), 
       aes(date, BLGMLD, col = latlat, group = latlat)) + 
  geom_line(pch = ".")+ facet_wrap(~sector)  + 
  ggtitle("MLD by month")

ggsave("MLD/MLD_month.png")



ggplot(ss %>% 
         group_by(date, sector, latlat) %>% summarize(BLGMLD = mean(BLGMLD)), 
       aes(date, BLGMLD, col = sector, group = sector)) + 
  geom_line(pch = ".")+facet_wrap(~latlat, ncol = 1, scales = "free") + 
  scale_color_manual(values = aes_cols) + 
  ggtitle("MLD month, latitude")

ggsave("MLD/MLD_month_latitude.png")


# tidync::tidync(mldf)
# library(dplyr)
# library(raster)
# library(angstroms)
# tunit <- ncmeta::nc_atts(mldf, "time") %>% 
#   dplyr::filter(name == "units") %>% 
#   tidyr::unnest() %>% dplyr::pull(value)
# timenc <- tidync::tidync(mldf) %>% 
#   tidync::activate("D0") %>% tidync::hyper_array(select_var = "time")
# time_rnc <- RNetCDF::utcal.nc(tunit, timenc$time)
# time <- ISOdate(time_rnc[,"year", drop = TRUE], 
#                 time_rnc[,"month", drop = TRUE], 
#                 time_rnc[,"day", drop = TRUE], tz = "UTC")
# mld <- setZ(angstroms::romsdata3d(mldf, varname= "BLGMLD"), time)
# cds <- setValues(subset(mld, 1:2), as.matrix(expand.grid(x = rawdata(mldf, "XC"), 
#                                                          y = rev(rawdata(mldf, "YC")))))
# 
# lon <- cds[[1]]
# lon[cellFromCol(cds, 2160)] <- 361
# lon[cellFromCol(cds, 1)] <- 1
# cds <- brick(lon, cds[[2]])
# library(quadmesh)
# mesh_low <- function(x, coords = NULL, fact = 8, ...) {
#   x <- aggregate(brick(x[[1]], coords), fact = fact, fun = function(x, ...) x[length(x)/2])
#   quadmesh::mesh_plot(x[[1]], coords = subset(x, 2:3), ...)
# }
# mesh_low(mld, cds, crs = "+proj=stere +lat_0=-90", fact = 4)

mesh_low(mld, cds)
