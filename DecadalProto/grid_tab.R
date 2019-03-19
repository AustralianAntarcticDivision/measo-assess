"grid_tab.rds"
# dimensions  : 100, 720, 72000  (nrow, ncol, ncell)
# resolution  : 0.5, 0.5  (x, y)
# extent      : -180, 180, -79.5, -29.5  (xmin, xmax, ymin, ymax)
# 
r <- raster(extent(-180, 180, -79.5, -29.5), nrow = 100, ncol = 720, crs = "+proj=longlat +datum=WGS84 +no_defs")

