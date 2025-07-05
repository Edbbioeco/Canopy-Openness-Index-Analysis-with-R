canopy_Openess <- function(x){
  
  require(c("tidyverse", "hemispheR"))
  
  stringr::str_glue("analysis for {x}") |> message()
  
  raster <- x |>
    hemispheR::import_fisheye() |>
    hemispheR::binarize_fisheye()
  
  values_0 <- raster[raster > 0] |> 
    terra::ncell()
  
  values_all <- raster |> 
    terra::ncell()
  
  result <- values_0 / values_all
  
  print(result)
  
}