# Pacotes ----

library(terra)

library(tidyterra)

library(magrittr)

# Dados ----

## Importando ----

imagem <- terra::rast("cropped-images/imagem.jpg")

## Visualizando ----

imagem

ggplot() +
  tidyterra::geom_spatraster_rgb(data = imagem)

# Recortando ----

## Extensão ----

ext <- imagem |> 
  terra::ext()

ext

## Centróide ----

centroide <- imagem |> 
  terra::ext() |> 
  terra::as.polygons() |> 
  terra::centroids()

centroide

## Raio ----

raio <- min((terra::xmax(ext) - terra::xmin(ext)),
            (terra::ymax(ext) - terra::ymin(ext))) / 2

raio

## Buffer ----

buffer <- terra::buffer(centroide, width = raio)

buffer

buffer |> plot()

## Recortando a imagem ----

imagem_crop <- imagem |> 
  terra::crop(buffer) |> 
  terra::mask(buffer)

imagem_crop

ggplot() +
  tidyterra::geom_spatraster_rgb(data = imagem_crop)

# Binarizando ----

## Convertendo para preto e branco ----

imagem_bw <- 0.299 * imagem[[1]] + 
  0.587 * imagem[[2]] + 
  0.114 * imagem[[3]]

imagem_bw_crop <- imagem_bw |> 
  terra::crop(buffer) |> 
  terra::mask(buffer)

ggplot() +
  tidyterra::geom_spatraster(data = imagem_bw_crop) +
  tidyterra::scale_fill_grass_c(palette = "grey")

## Convertendo em escala de 0 a 1

convert <- function(x){
  
  scl <- (x - min(x)) / (max(x) - min(x))
  
  return(scl)
  
}

imagem_bw_scale <- imagem_bw |> 
  tidyterra::mutate(imagem_1 = imagem_1 |> 
                      convert()) |> 
  terra::crop(buffer) |> 
  terra::mask(buffer)

imagem_bw_scale

ggplot() +
  tidyterra::geom_spatraster(data = imagem_bw_scale) +
  tidyterra::scale_fill_grass_c(palette = "grey")

## Binarizando ----

imagem_bi <- imagem_bw_scale |> 
  dplyr::mutate(imagem_1 = ifelse(imagem_1 > 0.75,
                                  1,
                                  0))

imagem_bi

ggplot() +
  tidyterra::geom_spatraster(data = imagem_bi) +
  scale_fill_viridis_c(na.value = "transparent")

# Índice ----

## Função para calcular o índice ----

coi_index <- function(x){
  
  n_ceu <- x[x > 0] |> 
    terra::ncell()
  
  n_total <- x |> 
    terra::ncell()
  
  indice <- n_ceu / n_total
  
  return(indice)
  
}

## Calculando ----

coi_index(imagem_bi)
