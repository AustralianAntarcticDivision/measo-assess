

## MLD
#mldf <- "/rdsi/PUBLIC/raad/data/sose.ucsd.edu/SO6/ITER122/bsose_i122_2013to2017_monthly_MLD.nc"
read_mld_sose <- function(date, ...) {
  library(dplyr)
  f <- raadfiles::get_raad_filenames() %>% dplyr::filter(stringr::str_detect(file, "bsose_i122_2013to2017_monthly_MLD")) %>% 
    dplyr::transmute(fullname = file.path(root, file))
  times <- RNetCDF::utcal.nc(ncmeta::nc_att(f$fullname[1], "time", "units")$value$units, angstroms::rawdata(f$fullname[1], "time"))
  times <- as.data.frame(times)
  dates <- ISOdatetime(times$year, times$month, times$day, times$hour, times$minute, times$second, tz = "UTC")
  files <- tibble::tibble(fullname = f$fullname, date = dates)
  
  if (missing(date)) date <- max(files$date) else  date <- as.POSIXct(date, tz = "UTC")
  if (date > max(files$date) || date < min(files$date)) stop("date not within range 2013-2017")
  i <- findInterval(date, files$date)
  data <- raster::readAll(angstroms::romsdata(files$fullname[i], varname = "BLGMLD"))
  #     coords = angstroms::romscoords(files$fullname[i], c("XC", "YC")))
  xy <- raster::values(angstroms::romscoords(files$fullname[i], c("XC", "YC")))
  r <- raster::raster(raster::extent(0, 360, -78, -30), res = 0.25)
  idx <- FNN::get.knnx(xy, sp::coordinates(r), k = 1)
  raster::rotate(raster::setValues(r, raster::extract(data, idx$nn.index[,1, drop = TRUE])))
}

get_mld <- memoise::memoize(read_mld_sose)
## http://soki.aq/display/MEASO/MEASO+Habitat%3A+6-Production
library(raadtools)
ext <- extent(-180, 180,-80,-35)
product <- "MODISA"
#raadtools:::readchla_mean(date, xylim = ext)

date <- seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by = "1 day")
xylim <- ext
init <- raadtools:::.init_bin(raadtools:::product2nrows(product))

maxbin <- croc::lonlat2bin(180, -35, 4320)
bin_sub <- NULL

files <- oc_sochla_files(product = product)
files <- files[findInterval(date, as.Date(files$date)), ]

a <- numeric(maxbin)
n <- integer(maxbin)

area_cell <- 4600 * 4600
for (i in seq_along(date)) {
  bins <- read_oc_sochla(date[i], product = product, inputfiles = files)  %>% 
    dplyr::filter(bin_num <= maxbin) %>% 
    dplyr::select(chla_johnson, bin_num)
  mld <- get_mld(date[i])
  xybin <- croc::bin2lonlat(bins$bin_num, 4320)
  mldvals <- raster::extract(mld, xybin, method = "bilinear")
  if (nrow(bins) > 0) {
    a[bins$bin_num] <- a[bins$bin_num] + bins$chla_johnson * mldvals * area_cell
    n[bins$bin_num] <- n[bins$bin_num] + 1L
    print(i)
  }
}

