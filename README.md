# Easy TRMM download
Download TRMM as '.tif' and using a shapefile

In R, type: <br>
```{r setup}
if(!require(pacman)) install.packages('pacman')
pacman::p_load(dplyr, raster, rgdal, ncdf4, shiny, ggplot2,
               sp, lubridate, rgeos, shinyjs, shineFiles, update = F)

runGitHub("TRMM_down", "daniel-althoff")
```
