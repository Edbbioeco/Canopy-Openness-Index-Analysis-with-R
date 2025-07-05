---
author:
- Edson Silva-Júnior
authors:
- Edson Silva-Júnior
editor: visual
execute:
  cache: false
theme:
  dark: darkly
  light: flatly
title: Canopy Openess Index in R
toc-title: Table of contents
---

<style>
body {
text-align: justify;
font-size: 20px}
blockquote {
  background-color: #FAEFA6;
  padding: 10px;
  font-size: 14.5px
}
</style>

# Canopy Openess Index in R

In ecology reasearchs about forest species, such leaf litter animals,
plants and fungi, one useful environmental process is the canopy
openness, which is the how much the tree canopy is open. This affects
several ecological process, due the solar radiation rates achieving that
ecosystems. For that, ecology researchers use fish eye lens to
photograph the forest canopy, to better analyse those processes. Canopy
may be measure by an index, where:

-   0: a canopy fully close;

-   1: there is not canopy.

Several image analysis softwares can generate canopy openness index
analysis, such as ImageJ. Although, for many images, programming
languages can automatize process, accelerating the research's progress.
In R, it is possible to analyse a set of canopy images and getting its
canopy openness indexes.

# Packages

For those analysis, we gonna use the packages:

-   terra: importing images as `spatRast` objets and get its propieties;

-   tidyverse: to assistant plots and loop processes;

-   tidyterra: to better plot the `spatRast` objets;

-   hemispheR: to binarize canopy imagens, for canopy openness indexes
    analysis.

Remind to confer whether the packages where previously installed to
library them.

::::::: cell
``` {.r .cell-code}
library(terra)
```

::: {.cell-output .cell-output-stderr}
    terra 1.8.50
:::

``` {.r .cell-code}
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}
    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.0.4     
:::

::: {.cell-output .cell-output-stderr}
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ tidyr::extract() masks terra::extract()
    ✖ dplyr::filter()  masks stats::filter()
    ✖ dplyr::lag()     masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
:::

``` {.r .cell-code}
library(tidyterra)
```

::: {.cell-output .cell-output-stderr}

    Anexando pacote: 'tidyterra'

    O seguinte objeto é mascarado por 'package:stats':

        filter
:::

``` {.r .cell-code}
library(hemispheR)
```
:::::::

# Data

## Importing

Our files are images taken by fish eye lens. Those images are archived
on `cropped-images` folder. Initialy, using `list.files()` function, we
get imagens path link.

:::: cell
``` {.r .cell-code}
images <- list.files(path = "cropped-images", 
                     pattern = ".png")

images
```

::: {.cell-output .cell-output-stdout}
    [1] "imagem1.png" "imagem2.png" "imagem3.png" "imagem4.png"
:::
::::

## Visualizing

The next step is the importing and visualizing the images.

To importing, we use `terra::rast()` function, loading images as
`spatRast` objects. Then, we use `ggplot2` and `tidyterra` functions to
plot. Moreover, we construct a loop for the every imagens, constructing
a function and loading it with `purrr` package.

::::::::::: cell
``` {.r .cell-code}
visualizing_canopy <- function(x){
  
  x <- paste0("cropped-images/", x)
  
  raster_bi <- terra::rast(x)
  
  ggplots <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster_bi) +
    scale_fill_continuous(na.value = "transparent") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) 
  
  print(ggplots)
  
}

purrr::walk(images, visualizing_canopy)
```

::: {.cell-output .cell-output-stderr}
    <SpatRaster> resampled to 501264 cells.
:::

::: cell-output-display
![](README_files/figure-markdown/unnamed-chunk-3-1.png)
:::

::: {.cell-output .cell-output-stderr}
    <SpatRaster> resampled to 501264 cells.
:::

::: cell-output-display
![](README_files/figure-markdown/unnamed-chunk-3-2.png)
:::

::: {.cell-output .cell-output-stderr}
    <SpatRaster> resampled to 501264 cells.
:::

::: cell-output-display
![](README_files/figure-markdown/unnamed-chunk-3-3.png)
:::

::: {.cell-output .cell-output-stderr}
    <SpatRaster> resampled to 501264 cells.
:::

::: cell-output-display
![](README_files/figure-markdown/unnamed-chunk-3-4.png)
:::
:::::::::::

# Calculating Canopy Openness

## Visualizing

To calculating canopy openness index, we first binarize images, to
pixels equivalent to canopy tree equals to 0 value and pixels equivalent
to open sky equals to 1 value. For that, we use
`hemispheR::import_fisheye()` to crop images to circle shape and then
`hemispheR::binarize_fisheye()` to binarize circle shaped images.

:::::::::::: cell
``` {.r .cell-code}
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
```

::: {.cell-output .cell-output-stderr}
    It is a circular fisheye, where xc, yc and radius are 1485.5, 1485.5, 1483.5
:::

::: {.cell-output .cell-output-stderr}
    <SpatRaster> resampled to 501264 cells.
:::

::: cell-output-display
![](README_files/figure-markdown/unnamed-chunk-4-1.png)
:::

::: {.cell-output .cell-output-stderr}
    It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5
    <SpatRaster> resampled to 501264 cells.
:::

::: cell-output-display
![](README_files/figure-markdown/unnamed-chunk-4-2.png)
:::

::: {.cell-output .cell-output-stderr}
    It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5
    <SpatRaster> resampled to 501264 cells.
:::

::: cell-output-display
![](README_files/figure-markdown/unnamed-chunk-4-3.png)
:::

::: {.cell-output .cell-output-stderr}
    It is a circular fisheye, where xc, yc and radius are 1500, 1500, 1498
    <SpatRaster> resampled to 501264 cells.
:::

::: cell-output-display
![](README_files/figure-markdown/unnamed-chunk-4-4.png)
:::
::::::::::::

## Calculating

Finally, with binarize raster, we measuring the ration pixels = 1 (open
sky) count by all pixels image count. For that, we use the
`terra::ncell()` function.

::::::: cell
``` {.r .cell-code}
canopy_Openess <- function(x){ 
  
  x <- paste0("cropped-images/", x)
  
  stringr::str_glue("analysis for {x}") |> message()
  
  raster <- x |>
    hemispheR::import_fisheye() |>
    hemispheR::binarize_fisheye()
  
  values_1 <- raster[raster > 0] |> terra::ncell()
  
  values_all <- raster |> terra::ncell()
  
  result <- values_1 / values_all
  
  print(result)
  
}

purrr::walk(images, canopy_Openess)
```

::: {.cell-output .cell-output-stdout}
    [1] 0.49774
:::

::: {.cell-output .cell-output-stdout}
    [1] 0.5004268
:::

::: {.cell-output .cell-output-stdout}
    [1] 0.2591377
:::

::: {.cell-output .cell-output-stdout}
    [1] 0.1944832
:::
:::::::

Notice the images with more open sky presented higher canopy openness
index values.
