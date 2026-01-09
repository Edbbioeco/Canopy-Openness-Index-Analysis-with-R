# ⚠️❌CRITICAL WARNING: PLEASE READ THIS WARNING BELLOW BEFORE TO PROCEDE❌⚠️

> This analysis, although useful, became obsolete. To check a easily way to procede for analyse canaopy openness index, check my package [coiR](https://github.com/Edbbioeco/coiR), which compiles a better way to do that analysis.

# Analysing Canopy Openness Index Analysis with R

Canopy openness index (COI) is often used as predictor variable in
ecological reseaches. COI can be calculated for the ratio between the
image pixels count understanded as the skt and the imagel total pixels
count, ranging from 0 (a full closed canopy) to 1 (a full open canopy).
Usely, it’s used fisheye images.

``` math

COI = \frac{N-Pixels_{sky}}{N-Pixels_{total}}
```

initialy, canopy images are binarized, to posteriorly pixels be counted.
In R, we can calculate COI easily by simples image analyses.

# Required Packages

For our analysis, we use the required packages:

- [terra](https://github.com/rspatial/terra): for import images as
  rasters to calculate raster pixels count;

- [tidyverse](https://www.tidyverse.org/packages): for transform data,
  generate visualizations by ggplot graphs and make loops, throught
  [purrr package](https://purrr.tidyverse.org);

- [tidyterra](https://dieghernan.github.io/tidyterra): for visualizing
  rasters;

- [hemispheR](https://canopyphotography.wordpress.com/2022/04/05/hemispher-an-r-package-for-fisheye-canopy-image-analysis):
  to recorte fisheye images and binarize them;

``` r
library(terra)

library(tidyverse)

library(tidyterra)

library(hemispheR)
```

# Data

Our data are images, taken by a fisheye lens.

## Images path

Firs, we informate images files path, as `list.files()` function,
informing its directory path (`path`) and its file class (`pattern`).

``` r
images <- list.files(path = "cropped-images", 
                     pattern = ".png")

images
```

    ## [1] "imagem1.png" "imagem2.png" "imagem3.png" "imagem4.png"

## Visualizing cannopy images through a looping

First to analysis, we have our files path, we build a
[loop](https://stackoverflow.com/questions/74794193/how-to-use-the-purrr-package-in-r-instead-of-for-loop-to-iterate-over-indices)
to visualize our images, making a function and run it on
`purrr::walk()`, informating our image paths and maked function. That
step is only to visualizing our images.

``` r
visualizing_canopy <- function(x){ 
  
  x <- paste0("cropped-images/", x)
  
  raster_bi <- terra::rast(x)
  
  ggplots <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster_bi) +
    scale_fill_continuous(na.value = "transparent") +
    scale_x_continuous(expand = FALSE) +
    scale_y_continuous(expand = FALSE) 
  
  print(ggplots)
  
}

purrr::walk(images, visualizing_canopy)
```

    ## Warning: [rast] unknown extent

    ## <SpatRaster> resampled to 501264 cells.

![](readme_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

    ## Warning: [rast] unknown extent

    ## <SpatRaster> resampled to 501264 cells.

![](readme_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

    ## Warning: [rast] unknown extent

    ## <SpatRaster> resampled to 501264 cells.

![](readme_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

    ## Warning: [rast] unknown extent

    ## <SpatRaster> resampled to 501264 cells.

![](readme_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

# Calculating Canopy Openess index

Our next step is to calculate Canopy Openess index (COI). Initialy, we
import the images, throught `hemispheR::import_fisheye()` function, and
binarize images, throught `hemispheR::binarize_fisheye()`. [hemispheR
package](https://canopyphotography.wordpress.com/2022/04/05/hemispher-an-r-package-for-fisheye-canopy-image-analysis)
binarize images for the images light incidence index. For now, lets see
binarized images. To facilite our work, lets make a loop, as previously
made.

## Visualizing each binarized images through a looping

``` r
canopy_visualizing <- function(x){ 
  
  path_file <- paste0("cropped-images/", x) 
  
  image_name <- stringr::str_remove(x, ".png")
  
  analy <- stringr::str_glue("Binnarized image for {image_name}") 
  
  file <- path_file  |>
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

    ## It is a circular fisheye, where xc, yc and radius are 1485.5, 1485.5, 1483.5

    ## <SpatRaster> resampled to 501264 cells.

![](readme_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

    ## It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5
    ## <SpatRaster> resampled to 501264 cells.

![](readme_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

    ## It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5
    ## <SpatRaster> resampled to 501264 cells.

![](readme_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

    ## It is a circular fisheye, where xc, yc and radius are 1500, 1500, 1498
    ## <SpatRaster> resampled to 501264 cells.

![](readme_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

## Calculating opennes Index for each images through a looping

Finaly, we calculate COI. To count sky pixels and image total pixels, we
use `terra::ncell()` function. TO only sky pixel, we filter image
raster, as `raster[raster > 0]`. The results are values, ranging from 0
to 1.

``` r
canopy_Openess <- function(x){ 
  
  path_file <- paste0("cropped-images/", x)
  
  image_name <- stringr::str_remove(x, ".png")
  
  stringr::str_glue("Cannopy Opennes Index for {image_name}:") |> 
    crayon::green() |> 
    message()
  
  raster <- path_file |>
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
```

    ## Cannopy Opennes Index for imagem1:

    ## It is a circular fisheye, where xc, yc and radius are 1485.5, 1485.5, 1483.5

    ## [1] 0.4971959

    ## Cannopy Opennes Index for imagem2:

    ## It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5

    ## [1] 0.4991239

    ## Cannopy Opennes Index for imagem3:
    ## It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5

    ## [1] 0.2578474

    ## Cannopy Opennes Index for imagem4:

    ## It is a circular fisheye, where xc, yc and radius are 1500, 1500, 1498

    ## [1] 0.1944832

# Tidying values into a drataframe

Now, lets tidy our results, and input them into a dataframe. First, we
built a null vector object, `results <- c()`. Next, we build a function
to calculate COI, but hyperdeclaring a objetct `results`, usind `<<-`
(please, do not confuse `<<-` to `<-`, and `results` to `result`),
making this object a global environment object.

``` r
results <- c()

canopy_Openess_df <- function(x){ 
  
  path_file <- paste0("cropped-images/", x)
  
  image_name <- stringr::str_remove(x, ".png")
  
  stringr::str_glue("Cannopy Opennes Index for {image_name}:") |> 
    crayon::green() |> 
    message()
  
  raster <- path_file |>
    hemispheR::import_fisheye() |>
    hemispheR::binarize_fisheye()
  
  values_0 <- raster[raster > 0] |> 
    terra::ncell()
  
  values_all <- raster |> 
    terra::ncell()
  
  result <- values_0 / values_all
  
  results <<- c(results, result)
  
} 

purrr::walk(images, canopy_Openess_df)
```

    ## Cannopy Opennes Index for imagem1:

    ## It is a circular fisheye, where xc, yc and radius are 1485.5, 1485.5, 1483.5

    ## Cannopy Opennes Index for imagem2:

    ## It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5

    ## Cannopy Opennes Index for imagem3:

    ## It is a circular fisheye, where xc, yc and radius are 1499.5, 1499.5, 1497.5

    ## Cannopy Opennes Index for imagem4:

    ## It is a circular fisheye, where xc, yc and radius are 1500, 1500, 1498

``` r
results
```

    ## [1] 0.4971959 0.4991239 0.2578474 0.1944832

Now, finally, we build a dataframe.

``` r
df_coi <- data.frame(images = images |> stringr::str_remove(".png"),
                     COI = results)
df_coi
```

    ##    images       COI
    ## 1 imagem1 0.4971959
    ## 2 imagem2 0.4991239
    ## 3 imagem3 0.2578474
    ## 4 imagem4 0.1944832

We can export it also, using `writexl` package.

``` r
df_coi |> 
  writexl::write_xlsx("results_coi.xlsx")
```


