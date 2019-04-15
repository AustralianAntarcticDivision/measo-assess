minlat <- -80
sectors_ll <- tribble(~lon, ~lat, ~zone,
                 -125, minlat,  1, 
                 -125, -35,  1,
                
                 -70, minlat, 2,
                 -64.5, -66, 2,
                 -60, -64, 2, 
                 -55.054477039271191, -63.260598133971456, 2, 
                 -63.799776827053591, -54.721070105901589, 2, 
                 
                 -5, minlat, 3, 
                 -5, -35, 3, 
                 
                 55, minlat, 4, 
                 55, -35, 4, 
                 
                 115, minlat, 5,  
                 115, -35, 5, 
                 
                 158, minlat, 6,
                 158, -75,    6,
                 170, -71.60, 6, 
                 170, -47.50, 6
                 )

library(spbabel)
library(dplyr)
sectors <- sectors_ll %>% 
  transmute(lon, lat, x_ = lon, y_ = lat, branch_ = zone, object_ = zone, order_ = row_number()) %>% 
  sp(crs = "+init=epsg:4326")
sectors <- sf::st_as_sf(sectors)
load("shapes/zones.Rdata")

#plot(sectors)
#plot(zones, add = TRUE)
#maps::map(add = TRUE)
domain <- st_cast(spex::polygonize(raster::raster(raster::extent(-180, 180, minlat, -30), nrows = 1, 
                                                  ncols = 1, 
                                                  crs = st_crs(sectors)$proj4string)), "LINESTRING")

lns <- st_sf(geometry = c(st_geometry(sectors),
                          st_geometry(domain),
                          st_geometry(zones)), crs = 4326)

measo_regions02g <- st_cast(st_polygonize(st_union(lns)))
## drop degenerate regions 
measo_regions02g <- measo_regions02g[st_area(st_set_crs(measo_regions02g, NA)) > 1]

measo_regions02 <- st_sf(geometry = measo_regions02g)
st_coordinates(st_centroid(measo_regions02))[,2]
plot(measo_regions02[st_coordinates(st_centroid(measo_regions02))[,2] < -38, ], 
     col = sample(rainbow(nrow(measo_regions02)-1)))




## order by longitude, then latitude of bottom left corner
ord <- spbabel::sptable(measo_regions02) %>% 
  group_by(object_) %>% 
  arrange(x_, y_) %>% slice(1) %>% ungroup() %>% arrange(x_, y_) %>% pull(object_)

#plot(measo_regions02, reset = FALSE)
#text(st_coordinates(st_centroid(measo_regions02))[ord, ], lab = 1:nrow(measo_regions02))
measo_regions02 <- measo_regions02[ord, ]

measo_regions02$name <- c("WPA", "WPS", "WPN", 
                          NA, ## northern background, 
                          "EPA", "EPS", "EPN", 
                          "WAA", "WAS", "WAN", 
                          "EAA", "EAS", "EAN", 
                          "CIA", "CIS", "CIN", 
                          "EIA", "EIS", "EIN", 
                          "WPA", "WPS", "WPN")
plot(st_geometry(measo_regions02), reset = FALSE, 
     col = rainbow(length(unique(measo_regions02$name)), alpha = 0.4)[factor(measo_regions02$name)], border  = NA)
text(st_coordinates(st_centroid(measo_regions02)), 
     lab = measo_regions02$name)
plot(orsifronts::orsifronts, add = TRUE)
