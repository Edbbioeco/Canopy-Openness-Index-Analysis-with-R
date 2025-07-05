# Packages ----

library(terra)

library(tidyverse)

library(tidyterra)

library(hemispheR)

# Data ----

## Importing ----  

images <- list.files(path = "cropped-images", 
                     pattern = ".png")

images

## Visualizing ----

visualizing_canopy <- function(x){
  
  raster_bi <- terra::rast(x)
  
  ggplots <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster_bi) +
    scale_fill_continuous(na.value = "transparent") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) 
  
  print(ggplots)
  
}

purrr::walk(images, visualizing_canopy)

# Calculating Canopy Openess ----

## Visualizing ----

canopy_visualizing <- function(x){ 
  
  analy <- stringr::str_glue("analysis for {x}") 
  
  file <- x  |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()
  
  ggplt <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = analy) +
    theme_bw()
  
  print(ggplt)
  
}

purrr::walk(images, canopy_visualizing)

## Calculating ----

canopy_Openess <- function(x){
  
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

purrr::walk(images, canopy_Openess)
