
calc_ice_season <- function(yfile, threshval = 15) {
  hemi <- substr(basename(yfile[1]), 1, 5)
  threshold.value <- threshval
  ndays <- 5
  
  len <- 15
  
  ## north_ice_1979_02_15_1980_02_14.grd"
  ice <- brick(yfile)
  year_n <- nlayers(ice)
  template <- ice[[1]] * 0
  icemat <- raster::values(ice)
  icemat[icemat > 100] <- 0
  
  ## here we need to get the next day for the interpolation . . .
  ##icemat[is.na(icemat)] <- 0
  adv <- numeric(nrow(icemat))
  ret <- numeric(nrow(icemat))
  threshold <- icemat >= threshold.value
  
  
  rsum <- .rowSums(threshold, nrow(threshold), ncol(threshold))
  ## all values less
  alllt <- rsum == 0
  ## all values greater than threshold
  allgt <- rsum == ncol(threshold)
  ## values missing
  ##miss <- icemat[,1] > 100
  ## all the rest
  visit <- which(!alllt & !allgt)
  for (ii in seq_along(visit)) {
    rl <- rle(threshold[visit[ii], ])
    
    ##    annual day of advance is the time when the ice
    ##    concentration in a given pixel first exceeds 15% (taken to
    ##    approximate the ice edge) for at least 5 days
    for (ir in seq_along(rl$lengths)) {
      if (rl$values[ir] & rl$lengths[ir] >= ndays) {
        adv[visit[ii]] <- if (ir == 1) 1L else sum(head(rl$lengths, ir - 1))
        break;
      }
    }
    if (adv[visit[ii]] == 0) adv[visit[ii]] <- NA
    
    ##while day of retreat
    ## ## is the time when concentration remains below 15% until the end
    ## ## of the given sea ice year
    revlengths <- rev(rl$lengths)
    revvals <- rev(rl$values)
    for (ri in seq_along(revlengths)) {
      if (revvals[ri]) {
        ret[visit[ii]] <- if (ri == 1) length(year_n) else sum(revlengths[ri:length(revlengths)])
        break;
      }
    }
    if (ret[visit[ii]] == 0) ret[visit[ii]] <- NA
  }
  adv[alllt] <- NA
  ## adv[miss] <- NA
  adv[allgt] <- 1
  ret[alllt] <- NA
  ret[allgt] <- length(year_n)
  
  list(adv = setValues(template, adv), ret = setValues(template, ret))
  
}


## function to ensure we have clean boundaries (untidy for now)
snaptodoy <- function(date, mon = 2, mday = 15, start = TRUE) {
  ldate <- as.POSIXlt(date)
  doy <- as.integer(format(ISOdatetime(ldate$year + 1900, mon, mday, 0, 0, 0, tz = "UTC"), "%j"))
  test <- ldate$yday > doy 
  if (start) {
    if (test) {ldate$mday <- mday; ldate$mon <- mon - 1; ldate$year <- ldate$year + 1}
    if (!test) {ldate$mday <- mday; ldate$mon <- mon - 1}
  } else {
    if (test) {ldate$mday <- mday; ldate$mon <- mon - 1}
    if (!test) {ldate$mday <- mday; ldate$mon <- mon + 1; ldate$year <- ldate$year - 1}
    
  }
  as.POSIXct(ldate)
}
## function to read a series of time slices, and interpolate a full sequence optionally
readIceBrick <- function(dates, hemisphere = "south", interpolate = TRUE) {
  x <- suppressWarnings(readice(dates, hemisphere = hemisphere,  setNA = FALSE))
  ## we need to interpolate (or insert monthly clim)
  if (!interpolate) return(x)
  if (nlayers(x) == length(dates)) return(x)
  
  xmat <- values(x)
  xmat[xmat > 100] <- 0
  dimnames(xmat) <- list(NULL, NULL)
  
  ## rebuild the entire data set with linear interpolation
  xmat2 <- matrix(0, nrow(xmat), length(dates))
  t0 <- timedateFrom(getZ(x))
  for (i in seq_len(nrow(xmat2))) {
    xmat2[i,] <- approxfun(t0, xmat[i,], rule = 2)(dates)
  }
  x <- setValues(x, xmat2)
  ##names(x) <- sprintf("layer%0.3i", seq(nlayers(x)))
  names(x) <- format(dates, "%Y_%m_%d")
  setZ(x, dates)
}



library(raadtools)
library(dplyr)
sfiles <- icefiles(hemisphere = "south")
sfiles %>% summarize(count = n(), datemin = min(date), datemax = max(date))


library(raster)
dp <- "/rdsi/PRIVATE/raad/data_local/aad.gov.au/iceseason"
library(raadtools)



hmon <- 2
hemi <- "south"
hmday <- 15
allfiles <- sfiles
startdate <- snaptodoy(min(allfiles$date), mon = hmon, mday = hmday, start = TRUE)
endate <- snaptodoy(max(allfiles$date), mon = hmon, mday = hmday -1, start = FALSE)
## start dates, including the end of the last year
bdates <- seq(startdate, endate, by = "1 year")
if (max(sfiles$date) >= (max(bdates) + 365.25 * 24 * 3600)) bdates <- c(bdates, max(bdates) + 24 * 365.24 * 3600)
for (iyear in head(seq_along(bdates), -1)) {
  #yearn <- (ydates %>% filter(yearN == levels(yearN)[iyear]))$date
  yearn <- seq(bdates[iyear], bdates[iyear + 1], by = "1 day")
  tfilename <- file.path(dp, "yearfiles", 
                         sprintf("%s_ice_%s_%s.grd", hemi, format(min(yearn), "%Y_%m_%d"), 
                                 format(max(yearn), "%Y_%m_%d")))
  print(iyear)
  if (!file.exists(tfilename)) {
    ice <- readIceBrick(yearn, hemisphere = hemi, interpolate = TRUE)
    writeRaster(ice, tfilename)
  }
}





library(raster)
dp <- "/rdsi/PRIVATE/raad/data_local/aad.gov.au/iceseason"
library(raadtools)



files <- list.files(file.path(dp, "yearfiles"), full.names = TRUE, pattern = "^south.*grd$")



for (threshold in c(5, 15, 25, 75, 95)) {
advance <- vector("list", length(files))
retreat <- vector("list", length(files))
daycount <- vector("list", length(files))
for (i in seq_along(files)) {
  ## obj contains 2 rasters "adv" and "ret"
  obj <- calc_ice_season(files[i], threshval = threshold)
  advance[[i]] <- obj$adv
  retreat[[i]] <- obj$ret
  daycount[[i]] <- calc(readAll(brick(files[i])) > threshold, mean, na.rm = TRUE) * 365
}

advance <- readAll(brick(stack(advance)))
retreat <- readAll(brick(stack(retreat)))
daycount <- readAll(brick(stack(daycount)))

saveRDS(advance, file.path(dp, sprintf("ice_%02i_advance.rds", threshold)))
saveRDS(retreat, file.path(dp, sprintf("ice_%02i_retreat.rds", threshold)))
saveRDS(daycount,file.path(dp, sprintf("ice_%02i_daycount.rds", threshold)))

}

####################################################################################################
library(raster)
library(dplyr)
read_season <- function(thresh = "15", dp = "/rdsi/PRIVATE/raad/data_local/aad.gov.au/iceseason") {
  stopifnot(is.character(thresh))
  stopifnot(nchar(thresh) == 2)
  files <- fs::dir_ls(dp, glob = "*ice*.rds")
  outfiles <- grep(thresh, files, value = TRUE)
  stopifnot(length(outfiles) == 3)
  keep10 <- function(x) {
    l <- nlayers(x)
    subset(x, (l-9):l)
  }
  retreat <- keep10(readRDS(grep("retreat", outfiles, value = TRUE)))
  advance <- keep10(readRDS(grep("advance", outfiles, value = TRUE)))
  daycount <- keep10(readRDS(grep("daycount", outfiles, value = TRUE)))
  adv <- calc(advance, median, na.rm = TRUE)
  ret <- calc(retreat, median, na.rm = TRUE)
  day <- calc(daycount, median, na.rm = TRUE)
  brick(list(advance = adv, retreat = ret, daycount = day))
}

season05 <- read_season("05")
season15 <- read_season("15")
season25 <- read_season("25")
season75 <- read_season("75")
season95 <- read_season("95")



prj <- projection(season05)
tab <- readRDS("prototypes/DecadalProto/grid_tab.rds")
tabxy <- reproj::reproj(cbind(tab$longitude, tab$latitude), prj, 
                        source = "+proj=longlat +datum=WGS84")[, 1:2, drop = FALSE]
tab[c("advance05", "retreat05")] <- raster::extract(season05, 
                                                    tabxy, 
                                                    method = "bilinear")
tab[c("advance15", "retreat15")] <- raster::extract(season15, 
                                                    tabxy, 
                                                    method = "bilinear")
tab[c("advance25", "retreat25")] <- raster::extract(season25, 
                                                    tabxy, 
                                                    method = "bilinear")
tab[c("advance75", "retreat75")] <- raster::extract(season75, 
                                                    tabxy, 
                                                    method = "bilinear")
tab[c("advance95", "retreat95")] <- raster::extract(season95, 
                                                    tabxy, 
                                                    method = "bilinear")

latbreaks <- seq(-80.5, -19.5, by = 1)
adv_tab<- tab %>% 
  #dplyr::filter(!is.na(advance70)) %>%
  group_by(latitude = cut(latitude, latbreaks), sector) %>% 
  summarize(advance15 = median(advance15, na.rm = TRUE), 
            advance25 = median(advance25, na.rm = TRUE), 
            advance75 = median(advance75, na.rm = TRUE), 
            advance95 = median(advance95, na.rm = TRUE), 
            retreat15 = median(retreat15, na.rm = TRUE), 
            retreat25 = median(retreat25, na.rm = TRUE),            
            retreat75 = median(retreat75, na.rm = TRUE), 
            retreat95 = median(retreat95, na.rm = TRUE))

idx <- which((adv_tab$retreat05 - adv_tab$advance05) < 0)
if (length(idx) > 0) adv_tab$retreat05[idx] <- NA; adv_tab$advance05[idx] <- NA
idx <- which((adv_tab$retreat15 - adv_tab$advance15) < 0)
if (length(idx) > 0) adv_tab$retreat15[idx] <- NA; adv_tab$advance15[idx] <- NA
idx <- which((adv_tab$retreat25 - adv_tab$advance25) < 0)
if (length(idx) > 0) adv_tab$retreat25[idx] <- NA; adv_tab$advance25[idx] <- NA
idx <- which((adv_tab$retreat75 - adv_tab$advance75) < 0)
if (length(idx) > 0) adv_tab$retreat75[idx] <- NA; adv_tab$advance75[idx] <- NA
idx <- which((adv_tab$retreat95 - adv_tab$advance95) < 0)
if (length(idx) > 0) adv_tab$retreat95[idx] <- NA; adv_tab$advance95[idx] <- NA

adv_tab$latitude <- (head(latbreaks, -1)+0.5)[adv_tab$latitude]

fronts <- spbabel::sptable(orsifronts::orsifronts)
fronts$front <- orsifronts::orsifronts$front[fronts$object_]
zones_ll <- sf::st_as_sf(aceecostats::aes_zone_ll) 

fronts$day <- raster::extract(season75[["daycount"]], reproj::reproj(cbind(fronts$x_, fronts$y_), prj, source = "+init=epsg:4326")[,1:2, drop = F], method = "bilinear")
fronts$day[!fronts$day > 0] <- NA
fronts$sector <- zones_ll$SectorName[ over(SpatialPoints(as.matrix(fronts[c("x_", "y_")])), 
                                           as(sf::st_set_crs(sf::st_geometry(zones_ll), NA), "Spatial"))]
front <- fronts %>% group_by(sector, front) %>% 
  summarize(front_median = median(y_), 
            front_hi = quantile(y_, 0.8), 
            front_lo = quantile(y_, 0.2), 
            day = median(day, na.rm = TRUE))

front$day[is.na(front$day)] <- 0
front <- front %>% ungroup() %>%  inner_join(tibble(front = c("pf", "saf", "saccf", "sbdy", "stf")), 
                                                     "front")

library(ggplot2)
ggplot(adv_tab) + 
  geom_segment(aes(latitude, advance15, xend = latitude, yend = retreat15), lwd = 5) + 
  #geom_segment(aes(latitude, advance25, xend = latitude, yend = retreat25), lwd = 4, col = "purple2") + 
  
 geom_segment(aes(latitude, advance75, xend = latitude, yend = retreat75), lwd = 3, col = "#21908C") + 
  geom_segment(aes(latitude, advance95, xend = latitude, yend = retreat95), lwd = 2, col = "yellow3") + 
  
  facet_wrap(~ordered(sector, c("WestPacific", "EastPacific", "Atlantic", "Indian")), ncol = 1) +
  geom_segment(data = front, lwd = 2, aes(front_lo, day, xend = front_hi, yend = day, col = front)) + 
  geom_point(data = front, aes(front_median, day)) +
  ylab("sea ice season") + 
   xlim(c(-80, -30)) + 
   ggtitle("Zonal-median sea ice season (2009-2019)", "Black: 15%,  Green: 75%, Yellow: 95% concentration")

     #ggsave("ICE-season.png")
   
   
   
