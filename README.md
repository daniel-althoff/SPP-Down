<div class="fluid-row" id="header">
    <img src='./www/rain.png' height='150' width='auto' align='right'>
    <h1 class="title toc-ignore">Satellite Precipitation Products Download</h1>
    <h4 class="author"><em>Daniel Althoff</em></h4>
</div>

# About

Satellite Precipitation Products Download (SPP-Down) is an App developed with <a href='https://shiny.rstudio.com/' target='blank'>Shiny</a> to ease the download of Precipitation Measuring Mission datasets from the <a href='https://disc.gsfc.nasa.gov/datasets/' target='blank'>Goddard Earth Sciences Data and Information Services Center</a> (GES DISC).<br> 

* Preprocessing of downloaded data consists of:
    * <em>.nc4</em> conversion to <em>.tif</em>;
    * crop/mask <em>raster</em> files using chosen shapefile; and,
    * if the desired time resolution is <b>Monthly</b>, the original unit (mm/hr) is converted to mm/month.
    
<br>
You'll need an Earthdata account with a link to the GES DISC (see <b>How to use</b> bellow).<br>
The download button appears once all required data are filled.<br>
Data will download inside the folder named <b>"TRMM_folder"</b> which will be created in the <b>output directory</b>.

# Release History

* <b>1.0.0</b> (27-11-2019)
    * Available dataset: <a href='https://pmm.nasa.gov/data-access/downloads/trmm' target='blank'>Tropical Rainfall Measuring Mission</a> (TRMM) 
    * Visualization of shapefile and buffer used to crop/mask

# How to use

<a href='https://urs.earthdata.nasa.gov/home' target='blank'>Create Earthdata account</a><br>
<a href='https://disc.gsfc.nasa.gov/earthdata-login' target='blank'>Link GES DISC with your account</a>

<!--<h4>1. Open the <b>SPP-Down</b> app</h4>
<h6>With ShinyApp</h6>
<p>Access the <a href="https://daniel-althoff.shinyapps.io/SPP-Down/" target="blank">SPP-Down</a> app directly in shinyapps.io.</p>-->

<h5>With R</h5>
<p>Install the required packages and run the <b>SPP-Down</b> app</p>

```{r setup}
ipacman::p_load(dplyr, raster, rgdal, ncdf4, shiny, ggplot2,
               sp, lubridate, rgeos, shinyjs, shineFiles, update = F)

runGitHub("SPP-Down", "daniel-althoff")
```

<img src="./misc/fig1.png"
     style="float: left; margin-right: 10px;" />

* <b>Fill all required information</b> 
    * Provide your Earthdata account and password
    * Choose desired data range
    * Choose desired time resolution (Daily or Monthly)
    * Select your shapefile (remeber to select all components, i.e, <em>.shp, shx, .dbf, .prj, etc...</em>)
    * Input buffer (>= 0)
    * Select output folder
    * Check if output folder is correct
    
<p> After the shapefile upload completes, the main panel will render a plot showing the your original shapefile, the buffer, and the average annual precipitation (mm/year) from 1998 to 2017 (20 years) <br>
    For this tutorial, we used the Cerrado biome (Brazil) shapefile as an example. </p> 
<p>The <b>download button</b> appears once all required information are filled.
  
<img src="./misc/fig2.png"
     style="float: left; margin-right: 10px;" />
     
<p><b>Ready? Start download!</b> and check your output directory for a new folder (<em>"TRMM_folder"</em>) and if data is being downloaded inside it.

<h4>Enjoy!</h4>
:smile:

# Acknowledgements

Special thanks to the Goddard Space Flight Center, Goddard Earth Sciences Data and Information Services Center and NASA's Earth Science Data Systems (ESDS) program.
