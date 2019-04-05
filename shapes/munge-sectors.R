load("shapes/zones.Rdata")
load("shapes/sectors.Rdata")
library(sf)

line_extend <- function(x, ylim) {
  for (i in seq_along(x$geometry)) {
    xxs <- x$geometry[[i]][,1]
    yys <- x$geometry[[i]][,2]
    if (length(unique(xxs)) == 1) {
        x$geometry[[i]][,2] <- ylim
    } else {
        ## append new coords because it's not just vertical
        #browser()
        #x$geometry[[i]] <- st_linestring(cbind(xxs[c(1, 1, 2, 2)], 
        #                         c(ylim[1], yys, ylim[2])))
        ## but, don't do the north ...
      ## mesh in the long Chile/Argentina border
      sam <- subset(rnaturalearth::ne_countries(), sovereignt %in% c("Chile", "Argentina"))
      arc <- silicate::ARC(sam)
      #arcs we want (it's broken atm)
      v <- arc$arc_link_vertex %>% filter(arc_ == arc$object_link_arc$arc_[2]) %>% 
        inner_join(arc$vertex) %>% select(x_, y_) #%>% plot(asp = 1)
      
        x$geometry[[i]] <- st_linestring(cbind(c(xxs[c(1, 1, 2)], rev(v$x_)), 
                                               c(ylim[1], yys, rev(v$y_))))
        
    }
  }
  x
}
sectors2 <- line_extend(sectors, c(-85, -30))



attr(sectors2$geometry, "bbox") <- st_bbox(c(xmin = -125, ymin = -85, xmax = 170, ymax = -35))

domain <- st_cast(spex::polygonize(raster::raster(raster::extent(-180, 180, -85, -30), nrows = 1, ncols = 1, crs = st_crs(sectors)$proj4string)), "LINESTRING")

lns <- st_sf(geometry = c(st_geometry(sectors2),
                          st_geometry(domain),
st_geometry(zones)), crs = 4326)

plot(st_cast(st_polygonize(st_union(lns))), col = sample(viridis::viridis(29)), reset = F)
maps::map(add = T)


lns <- st_sf(geometry = c(st_geometry(sectors2), 
                          st_geometry(domain), 
                          st_geometry(zones)), crs = 4326)

g<- c(st_geometry(zones), st_geometry(sectors))


spbabel::sptable(sectors)
