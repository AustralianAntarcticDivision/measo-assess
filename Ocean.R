library(raadtools)
f <- sshfiles(ssha = FALSE) %>% dplyr::filter(date > (Sys.time() - 10 * 365.25 * 24 * 3600), 
                                              format(date, "%m") == "02")

ssh <- readssh(f$date, xylim = extent(0, 360, -80, -29), lon180 = FALSE)
medssh <- calc(ssh, median, na.rm = TRUE)
u <- readcurr(f$date, xylim = extent(-180, 180, -80, -29), uonly = TRUE, lon180 = FALSE)
v <- readcurr(f$date, xylim = extent(-180, 180, -80, -29), vonly = TRUE, lon180 = FALSE)

sst <- readsst(f$date, xylim = extent(0, 360, -80, -29), lon180 = FALSE)
medsst <- calc(sst, median, na.rm = TRUE)
