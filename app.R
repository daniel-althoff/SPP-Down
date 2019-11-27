library(shiny)
library(raster)
library(rgdal)
library(ncdf4)
library(dplyr)
library(sp)
library(rgeos)
library(lubridate)
library(shinyjs)
library(shinyFiles)
library(ggplot2)


#User-interface
ui <- fluidPage(
  titlePanel(tags$div(h4('Get TRMM (TMPA) Precipitation V7'),
                      h6('Download data as ".tif" and using a shapefile ')),
             windowTitle = 'Easy TRMM download'),
  useShinyjs(),
  fluidRow(
    column(3,
           wellPanel(
             h5('If you have not already done so, please register:'),
             h6(a('Create Earthdata account', href='https://urs.earthdata.nasa.gov/home')),
             h6(a('Link GES DISC with your account', href='https://disc.gsfc.nasa.gov/earthdata-login')),
             
             textInput('acc', h6(tags$b('Earthdata account:'))),
             passwordInput('pass', h6(tags$b('Password:'))),
             
             dateRangeInput('date', h6(tags$b("Date range:")), 
                            start = '1998-01-01', end = '2019-08-31',
                            min = '1998-01-01', max = '2019-08-31'),
             
             radioButtons('timescale', h6(tags$b('Time resolution:')), 
                          choices = list('Daily', 'Monthly'),
                          inline=T,
                          selected = 'Daily')
           ),
           wellPanel(
             p('Made with ', a('Shiny', href='http://shiny.rstudio.com'), '.'),
             img(src='Shiny.png', height='50x')
           )
    ),
    
    column(3,
           wellPanel(
             div(style='height: 110px',
                 fileInput('shpFile', h6(tags$b('Input shapefile components'), h6(tags$em('(.shp, .shx, .dbf, .prj, etc...)'))),
                           multiple=TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"))),
             div(style='height: 80px',
                 numericInput('buffer', h6(tags$b('Buffer'), tags$em('(ex: 0.25 degrees)')), min = 0, max = 50, step = 0.25, value = 0.25))
           )
    ),
    
    column(3,
           wellPanel(
             h6(tags$b('Choose download output folder')),
             div(shinyDirButton('dir', 'Click here to browse folder', title='Choose path'), style="text-align: center; height: 35px" ),
             tags$br(),tags$br(),
             h6(tags$b('Confirm your output folder:')),
             verbatimTextOutput("dir")
           )
    ),
    
    column(9,
           mainPanel(
             column(6,h6(tags$b('Download button will appear here:'))),
             column(3,div(actionButton("button","Ready? Start download!"), style="text-align: center")),
             plotOutput(outputId = 'AvgTRMM', height='325px')
           )
    )
    
  )
)

#Server-side
server <- function(input, output){
  
  observe({
    shinyjs::hide('button')
    
    req(shp_buffer())
    req(acc())
    req(input$pass)
    req(dir())
    shinyjs::show('button')
  })
  
  roots = c(name = unlist(strsplit(getwd(), .Platform$file.sep))[1])
  shinyDirChoose(input, 'dir', roots = roots)
  dir <- reactive({
    return(print(parseDirPath(roots, input$dir)))
  })
  output$dir <- renderPrint(dir())
  
  options(shiny.maxRequestSize=50*1024^2)
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
  
  
  data_range <- reactive({
    switch(input$timescale,
           'Daily' = gsub('-','',seq.Date(from = input$date[1], to = input$date[2], by='day')),
           'Monthly' = gsub('-','',seq.Date(from = input$date[1], to = input$date[2], by='month')))
  })
  
  correcao <- reactive({
    switch(input$timescale,
           'Daily' = rep(1, length(data_range())),
           'Monthly' = 24*days_in_month(seq.Date(from = input$date[1], to = input$date[2], by='month'))
           
    )
  })
  
  acc <- reactive({
    acc <- input$acc
    acc <- sub('@','%40', acc)
    return(acc)
  })
  
  files_to_download <- reactive({
    switch(input$timescale,
           'Daily' = paste0('https://',
                            acc(),':',input$pass,'@',
                            'disc2.gesdisc.eosdis.nasa.gov/opendap/',
                            'TRMM_L3/TRMM_3B42_Daily.7/',substr(data_range(),1,4),
                            '/',substr(data_range(),5,6),'/3B42_Daily.',
                            data_range(),'.7.nc4.nc4?precipitation,lon,lat'),
           'Monthly' = paste0('https://',
                              acc(),':',input$pass,'@',
                              'disc2.gesdisc.eosdis.nasa.gov/opendap/',
                              'TRMM_L3/TRMM_3B43.7/',substr(data_range(),1,4),
                              '/3B43.',
                              substr(data_range(),1,6),'01.7.HDF.nc4?precipitation,nlon,nlat')
    )
  })
  
  shp_buffer <- reactive({
    req(input$shpFile)
    shp <- spTransform(uploadShpfile(), CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'))
    shp <- buffer(shp, input$buffer)
    return(shp)
  })
  
  
  observeEvent(input$button, {
    req(input$shpFile)
    TRMM_folder <- paste0(dir(),"\\TRMM_folder")
    if(!file.exists(TRMM_folder))
      dir.create(TRMM_folder)
    # temp <- tempfile(fileext = ".nc4", tmpdir = "TRMM_folder")
    for(i in seq_along(data_range())){
      filename = tempfile(fileext = '.nc4', tmpdir = TRMM_folder)
      fileName <- switch(input$timescale,
                         'Daily' = paste(TRMM_folder,"\\TRMM_",data_range()[i],".tif",sep = ""),
                         'Monthly' = paste(TRMM_folder,"\\TRMM_",substr(data_range()[i],1,6),".tif",sep = "")
      )
      download.file(files_to_download()[i], filename, mode = "wb", quiet = T)
      addResourcePath("TRMM_folder",TRMM_folder)
      XX <- ncdf4::nc_open(filename)
      
      lon <- switch(input$timescale,
                    'Daily' = ncvar_get(XX, 'lon'),
                    'Monthly' = ncvar_get(XX, 'nlon'))
      nlon <- dim(lon)
      
      lat <- switch(input$timescale,
                    'Daily' = ncvar_get(XX, 'lat'),
                    'Monthly' = ncvar_get(XX, 'nlat'))
      nlat <- dim(lat)
      
      prec <- ncvar_get(XX, 'precipitation')
      fillv <- ncatt_get(XX,'precipitation', '_FillValue')
      nc_close(XX)
      unlink(filename)
      
      prec[prec == fillv$value] <- NA
      r <- raster((prec), xmn=-180, xmx=180, ymn=-50, ymx=50, 
                  crs=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0')) %>%
        flip(direction='y') %>% crop(shp_buffer()) %>% mask(shp_buffer())
      
      writeRaster(r*correcao()[i], fileName, format='GTiff', overwrite=T)
    }
  })
  
  localtif <- reactive({
    req(input$shpFile)
    loadtif = tempfile(fileext = '.tif')
    download.file('https://github.com/danielalthoff/TRMM_down/raw/master/www/1998_2017_avg.tif',
                  destfile = loadtif, mode='wb', quiet = T)
    localtif = raster(loadtif)
    crs(localtif) <- crs(shp_buffer())
    localtif = crop(localtif, shp_buffer()) %>% mask(shp_buffer())
    unlink(loadtif)
    return(localtif)
  })
  
  # observe({
  #   req(input$shpFile)
  #   raster_pts <- localtif() %>% crop(shp_buffer()) %>%
  #     mask(shp_buffer()) %>% rasterToPoints() %>% tbl_df() %>% rename('Rain' = 3)
  #   print(names(raster_pts)[3])
  # })
  
  output$AvgTRMM <- renderPlot({
    req(input$shpFile)
    raster_pts <- localtif() %>% crop(shp_buffer()) %>% 
      mask(shp_buffer()) %>% rasterToPoints() %>% tbl_df() %>% rename('Rain' = 3)
    
    ggplot(raster_pts) +
      geom_point(aes(x=x,y=y, color=Rain), shape = 15, size=1.) +
      geom_path(data = fortify(uploadShpfile()), aes(x=long, y=lat, group=group, linetype='Original'), color='black') +
      geom_path(data = fortify(shp_buffer()), aes(x=long, y=lat, group=group, linetype='Buffer'), color='black') +
      scale_linetype_manual(name='', values=c(3,1))+
      scale_color_viridis_c(direction=-1,
                            name=expression(paste('Rainfall (mm year'^-1,')'))) +
      coord_fixed(ratio=1) + labs(x='Longitude', y='Latitude', subtitle = 'Average from 1998 to 2017') +
      theme(text = element_text('serif'), legend.title = element_text('serif', face = 'plain'),
            panel.border = element_rect(fill='transparent', color='black'))
    
    # plot(localtif())
    # plot(uploadShpfile(), add=T, lty=2)
    # plot(shp_buffer(), add=T, border='red')
  })
  
}

#ShinyApp
shinyApp(ui = ui, server = server)

