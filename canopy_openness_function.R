canopy_visualizing <- function(x){ 
  
  x <- paste0("cropped-images/", x)
  
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