aes_bathy1 <- aceecostats::aes_region_ll
aes_bathy1 <- sf::as_Spatial(st_cast(sf::st_as_sf(aes_bathy1), "POLYGON"))
aes_bathy1$id <- 1:nrow(aes_bathy1)
#plot(aes_bathy1)
#raster::click(aes_bathy1, n = 5)$id
aes_bathy1$BathyClass[c(71, 74, 122, 155, 204)] <- "Continent"

rhumb_width <- function(x) {
 height <- res(x)[1] * 1852 * 60
 x[] <- cos(yFromCell(x, 1:ncell(x)) * pi/180) * height
 x
}
library(sf)
library(dplyr)
aes_bathy <- sf::st_as_sf(aes_bathy1) %>% group_by(BathyClass, SectorName, Zone) %>% 
  summarize() %>% st_cast()


aes_bathy$ID <- 1:nrow(aes_bathy)
rr <- raster(extent(-180, 180, -79.5, -29.5), res = c(.5, .5), crs = "+init=epsg:4326")
r <- fasterize::fasterize(aes_bathy, 
              rr, 
               field = "ID")


tab <- tabularaster::as_tibble(r, index = TRUE) %>% 
  dplyr::filter(!is.na(cellvalue)) %>% 
  dplyr::mutate(latitude = raster::yFromCell(r, cellindex), 
                SectorName = aes_bathy$SectorName[cellvalue], 
                BathyClass = aes_bathy$BathyClass[cellvalue], 
                width = rhumb_width(rr)[cellindex]) %>% 
  group_by(latitude, SectorName, BathyClass) %>% 
  summarize(width = sum(width))


ggplot(tab, aes(latitude, width, col = SectorName)) + 
  geom_point()
 ## need to get area from area(r)
ggplot(tab, aes(latitude, width, group = BathyClass, col = BathyClass)) + geom_path() + facet_wrap(~SectorName)







aes_bathy1 <- aceecostats::aes_region_ll
aes_bathy1 <- sf::as_Spatial(st_cast(sf::st_as_sf(aes_bathy1), "POLYGON"))
aes_bathy1$id <- 1:nrow(aes_bathy1)
#plot(aes_bathy1)
#raster::click(aes_bathy1, n = 5)$id
aes_bathy1$BathyClass[c(71, 74, 122, 155, 204)] <- "Continent"


rhumb_width <- function(x) {
  height <- res(x)[1] * 1852 * 60
  x[] <- cos(yFromCell(x, 1:ncell(x)) * pi/180) * height
  x
}
library(sf)
library(dplyr)
aes_bathy <- sf::st_as_sf(aes_bathy1) %>% group_by(BathyClass, SectorName, Zone) %>% 
  summarize() %>% st_cast()


aes_bathy$ID <- 1:nrow(aes_bathy)
rr <- raster(extent(-180, 180, -79.5, -29.5), res = c(.5, .5), crs = "+init=epsg:4326")
r <- fasterize::fasterize(aes_bathy, 
                          rr, 
                          field = "ID")


topo <- raadtools::readtopo("gebco_14", xylim = extent(-180, 180, -80, -29))
topo <- readAll(topo)

uni <- sf::st_cast(sf::st_union(aes_bathy), "POLYGON")[2]  ## avoid a tiny part of Antarctica
nohole <- st_polygon(list(uni[[1]][[1]]))
## anything in continent and NA is "Continent"
continent <- fasterize::fasterize(aes_bathy %>% dplyr::filter(BathyClass == "Continent"), 
                                  rr)

## zap out the continental-land
mask <- fasterize::fasterize(sf::st_sf(geometry = sf::st_sfc(nohole), id = 1), topo, field = "id")
lons <- c(-180, -115, -60, 55, 145, 180)
tab <- tabularaster::as_tibble(r, index = TRUE) %>% 
  dplyr::mutate(latitude = raster::yFromCell(r, cellindex),
            longitude = raster::xFromCell(r, cellindex),
            #SectorName = aes_bathy$SectorName[cellvalue], 
            #BathyClass = aes_bathy$BathyClass[cellvalue], 
            gebco = raster::extract(topo * mask, xyFromCell(r, cellindex)),
            bathy = c("Deep", "Bank", "Island")[cut(gebco, c(-1e4, -2000, 0, 1e4))],
            continent =  !is.na(continent[cellindex]),
            width = rhumb_width(rr)[cellindex], 
            sector = c("WestPacific", "EastPacific", "Atlantic", "Indian", "WestPacific")[cut(longitude, lons)])

tab$bathy[tab$continent] <- "Continent"

tab$bathy <- ordered(tab$bathy, c("Deep", "Bank", "Island", "Continent"))



a <- tab %>% 
  group_by(latitude, sector, bathy) %>% 
  summarize(width = sum(width, na.rm = TRUE)) %>% 
  arrange(latitude) %>% dplyr::filter(!is.na(bathy))

library(ggplot2)
ggplot(a %>% dplyr::mutate(bathy = levels(bathy)[bathy]), 
       aes(latitude, sqrt(width), group = bathy, colour = bathy)) + 
  geom_path() + facet_wrap(~sector)



