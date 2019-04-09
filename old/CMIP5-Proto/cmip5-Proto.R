  dp <- "/mnt/home/ryan/RAATD_01/RAATDfuture/Working/Data/Climate"
  fs::dir_ls(dp)
  
  
  thing_getter <- function(patt = c("tos", "historical"), model = "ACCESS1-0") {
    files <- tibble::tibble(fullname = fs::dir_ls(file.path(dp, model))) 
    for (i in seq_along(patt))  {
      files <- dplyr::filter(files, stringr::str_detect(basename(fullname), patt[i]))
    }
    files
  }
  f <- thing_getter()$fullname
  
  tidync::tidync(f)
  
  library(angstroms)
  x <- angstroms::romsdata(f, varname = "tos")
  cd <- romscoords(f, c("lon", "lat"), transpose = TRUE)
  library(quadmesh)
  
remotes::install_github("AustralianAntarcticDivision/aceecostats")  
## map model coords to our zones
zones_ll <- sf::st_as_sf(aceecostats::aes_zone) 
mesh_plot(x, coords = cd,crs = projection(zones_ll))
plot(zones_ll, add = TRUE, col = NA)

  