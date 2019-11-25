library(shiny)
library(shinyFiles)
library(raster)
library(rgdal)
library(ncdf4)
library(dplyr)
library(sp)
library(rgeos)

#User-interface
ui <- fluidPage(
  titlePanel(tags$div(h4('Get TRMM (TMPA) Precipitation V7'),
                      h6('Download data as ".tif" and using a shapefile '))),
  fluidRow(
    column(3,
           wellPanel(
             h5('If you have not already done so, please register:'),
             h6(a('Create Earthdata account', href='https://urs.earthdata.nasa.gov/home')),
             h6(a('Link GES DISC with your account', href='https://disc.gsfc.nasa.gov/earthdata-login')),
             
             textInput('acc', h6(tags$b('Earthdata account:'))),
             textInput('pass', h6(tags$b('Password:')))
           )
    ),
    
    column(3,
           wellPanel(
             dateInput("date1", h6(tags$b("From:")), value = "1998-01-01"),
             dateInput("date2", h6(tags$b("To:")), value = "2019-08-31"),
             tags$br(),
             radioButtons('timescale', h6(tags$b('Time resolution:')), 
                          choices = list('Daily', 'Monthly'),
                          inline=T,
                          selected = 'Daily')
           )
    ),
    
    column(3,
           wellPanel(
             fileInput('shpFile', h6(tags$b('Input shapefile'), h6('(Select all shapefile components)')), accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
             # shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
             actionButton("button","All set? Start download"),
             tags$br(),tags$br(),
             p('Made with ', a('Shiny', href='http://shiny.rstudio.com'), '.'),
             img(src='Shiny.png', height='50x')
           )
    ),
    
    column(9,
           mainPanel()
    )
    
  )
)

#Server-side
server <- function(input, output){
  options(shiny.maxRequestSize=30*1024^2)
  uploadShpfile <- reactive({
    if (!is.null(input$shpFile)){
      shpDF <- input$shpFile
      prevWD <- getwd()
      uploadDirectory <- dirname(shpDF$datapath[1])
      setwd(uploadDirectory)
      for (i in 1:nrow(shpDF)){
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
      shpPath <- paste(uploadDirectory, shpName, sep="/")
      setwd(prevWD)
      shpFile <- readOGR(shpPath)
      return(shpFile)
    } else { return() }
  })
  
  shinyDirChoose(input, 'folder', roots=c(wd=getwd()), filetypes=c('', 'txt'))
  
  # outfolder <- reactive({
  #   return(input$folder)
  # })

  data_range <- reactive({
    switch(input$timescale,
           'Daily' = gsub('-','',seq.Date(from = input$date1, to = input$date2, by=1)),
           'Monthly' = substr(gsub('-','',seq.Date(from = input$date1, to = input$date2, by=1)),1,6))
  })
  
  acc <- reactive({
    acc <- input$acc
    acc <- sub('@','%40', acc)
    return(acc)
  })
  
  files_to_download <- reactive({
    paste0('https://',
           acc(),':',input$pass,'@',
           'disc2.gesdisc.eosdis.nasa.gov/opendap/',
           'TRMM_L3/TRMM_3B42_Daily.7/',substr(data_range(),1,4),
           '/',substr(data_range(),5,6),'/3B42_Daily.',
           data_range(),'.7.nc4.nc4?precipitation,lon,lat')
  })
  
  shp_buffer <- reactive({
    shp <- spTransform(uploadShpfile(), CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'))
    shp <- buffer(shp, 0.25)
    return(shp)
  })
  
  # observe({
  #   req(input$shpFile)
  #   print(files_to_download()[1])
  #   print(paste("TRMM",data_range()[1],".tif",sep = ""))
  #   print(outfolder())
  # })
  
  observeEvent(input$button, {
    imgs_folder <- "TRMM_folder"
    if(!file.exists(imgs_folder))
      dir.create("TRMM_folder")
    # temp <- tempfile(fileext = ".nc4", tmpdir = "TRMM_folder")
    for(i in seq_along(data_range())){
      filename = paste0('TRMM_folder\\temp.nc4')
      fileName <- paste("TRMM_folder\\TRMM_",data_range()[i],".tif",sep = "")
      download.file(files_to_download()[i], filename, mode = "wb", quiet = T)
      # addResourcePath("TRMM_folder",imgs_folder)
      XX <- ncdf4::nc_open(filename)
      
      lon <- ncvar_get(XX, 'lon')
      nlon <- dim(lon)
      
      lat <- ncvar_get(XX,'lat')
      nlat <- dim(lat)
      
      prec <- ncvar_get(XX, 'precipitation')
      fillv <- ncatt_get(XX,'precipitation', '_FillValue')
      nc_close(XX)
      unlink(filename)
      
      prec[prec == fillv$value] <- NA
      r <- raster((prec), xmn=-180, xmx=180, ymn=-50, ymx=50, 
                  crs=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0')) %>%
        flip(direction='y') %>% crop(shp_buffer()) %>% mask(shp_buffer())
      writeRaster(r, fileName, format='GTiff', overwrite=T)
    }
  })
  
}

#ShinyApp
shinyApp(ui = ui, server = server)

