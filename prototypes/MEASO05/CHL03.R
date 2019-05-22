
if (FALSE) {
  chlf <- raadtools::ocfiles(time.resolution = "monthly", 
                             product = "MODISA", varname = "CHL", type = "L3m") %>% 
    dplyr::mutate(season = case_when(format(date, "%m") %in% c("10", "11", "12") ~ "Oct-Dec",
                                     format(date, "%m") %in% c("01", "02", "03") ~ "Jan-Mar")) %>% 
    dplyr::filter(!is.na(season)) %>% dplyr::filter(date > (Sys.time()  - 10 * 365.25 * 24 * 3600))
  
  ex <-  extent(-180, 180, -80, -29)
  chl <- brick(purrr::map(split(chlf$fullname, chlf$season)[unique(chlf$season)], 
                          ~calc(brick(lapply(.x, function(file) raster::crop(raster(file), ex))), mean, na.rm = TRUE)))
  
  chl <- readAll(chl)
  saveRDS(chl, "chl.rds")
}

chl <- readRDS("prototypes/DecadalProto/chl.rds")


library(ggplot2)
library(dplyr)
library(raadtools)
library(tabularaster)
library(measoshapes)
library(future)
library(furrr)


#The average abundance of Chlorophyll a over spring and summer (October to
#March) is used to indicate a cell's relative productivity in a year.  The
#relative productivity for a latitude in a year is a sum of these cell averages
#for all cells on that latitude.  The summary attribute of production at a
#latitude is then represented by the median of the relative productivity for
#that latitude over the last ten years (variation represented by the 20th and
#80th depth percentiles).


rebuild <- FALSE
if (rebuild) {

GRID <-mean(chl, na.rm = TRUE)



library(sf)
measo_regions03_ll <- dplyr::inner_join(measoshapes::measo_regions03_ll, measo_names, "name")

cn <- cellnumbers(GRID, measo_regions03_ll)
latbreaks <- seq(-80.5, -19.5, by = 1)
cn$lon <- xFromCell(GRID, cn$cell_)
cn$lat <- yFromCell(GRID, cn$cell_)
cn$zone <- measo_regions03_ll$zone[cn$object_]
cn$sector <- measo_regions03_ll$sector[cn$object_]
cn <- dplyr::filter(cn, !is.na(zone))
cn$sector <- ordered(cn$sector, c("WestPacific", "EastPacific", "WestAtlantic", "EastAtlantic", 
                                 "CentralIndian", "EastIndian"))
cn$zone <- ordered(cn$zone, c("Antarctic", "Subantarctic", "Northern"))


cn$chla <- GRID[cn$cell_]



bigtab <- cn  %>% dplyr::filter(!is.na(zone), !is.na(chla)) %>% 
  group_by(
    latitude = cut(lat, latbreaks),
    
    sector, zone
  ) %>%
  summarize(
    chla_median = mean(chla),
    chla_hi = quantile(chla, 0.80),
    chla_lo  = quantile(chla, 0.20)
  ) %>% ungroup()
bigtab$latitude <- (head(latbreaks, -1)+0.5)[bigtab$latitude]

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- ordered(orsifronts::orsifronts$front[fronts$object_], 
                        c("sbdy", "saccf", "pf", "saf", "stf"))
fronts$sector <- measo_regions03_ll$sector[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), as(sf::st_set_crs(sf::st_geometry(measo_regions03_ll), NA), "Spatial"))]
fronts$chla <- raster::extract(GRID, cbind(fronts$x_, fronts$y_), method = "bilinear")
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            chla = median(chla, na.rm = TRUE))



front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")),  "front")
front <- front %>% dplyr::filter(!is.na(sector))
front$front <- ordered(front$front, c("sbdy", "saccf", "pf", "saf", "stf"))

saveRDS(bigtab, file = "MEASO03/chla.rds")
saveRDS(front, file = "MEASO03/fronts_chla.rds")

}

chla <- readRDS("MEASO03/chla.rds")
front <- readRDS("MEASO03/fronts_chla.rds")

ggplot(chla) + #%>% dplyr::filter(depth_hi < -1, zone == "Antarctic")) + 
  geom_line(aes(latitude, chla_median, group = zone)) + 
  geom_ribbon(aes(latitude, ymax = chla_hi, 
                  ymin = chla_lo, fill = zone, group = zone), alpha = 0.5) + 
  facet_wrap(~ordered(sector, c("WestAtlantic", "CentralIndian", "EastIndian", "WestPacific", "EastPacific")), ncol = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, chla, xend = front_hi, yend = chla, col = front)) + 
  geom_point(data = front, aes(front_median, chla))  + 
  scale_y_log10() +
  ggtitle("Chlorophyll-a (mg/m3)", 
          "chla (log10)")
ggsave("MEASO03/chla03.png")
  
  
 
