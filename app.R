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
library(rasterVis)
library(viridis)
library(latticeExtra)
library(rvest)



#### User-interface
ui <- fluidPage(
  titlePanel(tags$div(h4(a('Satellite Precipitation Products Download v1.0.1', href = 'https://github.com/daniel-althoff/SPP-Down')),
                      h6('Datasets available: ',
                         a('TRMM', href='https://pmm.nasa.gov/data-access/downloads/trmm'),
                         'and',
                         a('GPM IMERG', href='https://pmm.nasa.gov/data-access/downloads/gpm')))),
  useShinyjs(),
  fluidRow(
    ### First column (Left > Right)
    column(3,
           ### First row (Top > Down)
           wellPanel(
             h5('If you have not already done so, please register:'),
             h6(a('Create Earthdata account', href='https://urs.earthdata.nasa.gov/home')),
             h6(a('Link GES DISC with your account', href='https://disc.gsfc.nasa.gov/earthdata-login')),
             
             ## Input authentication 
             textInput('acc', label = div(style = 'font-size:10px;',"Earthdata account:"), placeholder = 'e-mail'),
             passwordInput('pass', label = div(style = 'font-size:10px;','Password:'), placeholder = '******'),
             
             ## Data product
             radioButtons('ppt', label = div(style = 'font-size:10px;','Precipitation product:'), 
                          choices = list('TRMM', 'GPM'),
                          inline=T,
                          selected = 'TRMM'),
             tags$br(),
             
             ## Time scale
             radioButtons('timescale', label = div(style = 'font-size:10px;','Time resolution:'), 
                          choices = list('Daily', 'Monthly'),
                          inline=T,
                          selected = 'Daily'),
             tags$br(),
             
             ## Data range
             uiOutput('daterange')),
             
           
           ## Configuring box sizes
           tags$head(
             tags$style(
               HTML('
                    #acc{font-size: 10px; height: 25px}
                    #pass{font-size: 10px; height: 25px}
                    #timescale{height: 30px}
                    #ppt{height: 30px}
                    #daterange{font-size: 10px; height: 40px}
                    ')
               )
           ),
           
           ### Second row 
           wellPanel(style = 'background: white; border: white; height: 25px;',
             p(img(src='Shiny.png', height='25x'),
               'Made with ', a('Shiny.', href='http://shiny.rstudio.com'))
             )
    ),
    
    ### Second column
    column(3,
           wellPanel(
             div(style='height: 110px; font-size: 10px;',
                 fileInput('shpFile', h6(tags$b('Input shapefile components'), h6(tags$em('(.shp, .shx, .dbf, .prj, etc...)'))),
                           multiple=TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"))),
             div(style='height: 80px; font-size: 10px;',
                 numericInput('buffer', h6(tags$b('Buffer'), tags$em('(ex: 0.25 degrees)')), min = 0, max = 50, step = 0.25, value = 0.0))
           )
    ),
    
    ### Third column
    column(3,
           wellPanel(
             h6(tags$b('Choose download output folder')),
             div(shinyDirButton('dir', 'Click here to browse folder', title='Choose path'), style="text-align: center; height: 35px" ),
             tags$br(),tags$br(),
             h6(tags$b('Confirm your output folder:')),
             verbatimTextOutput("dir")
           )
    ),
    
    ### Main panel
    column(9,
           mainPanel(
             column(6,h6(tags$b('Download button will appear here:'))),
             column(3,div(actionButton("button","Ready? Start download!"), style="text-align: center")),
             plotOutput(outputId = 'AvgTRMM')
           )
    )
  )
)


##### Server-side
server <- function(input, output){
  ## Welcome message
  cat('\n Welcome to SPP-Down! \n For a tutorial or information o satellite products, visit: \n https://github.com/daniel-althoff/SPP-Down \n\n')
  cat('Please enter your Earthdata account and password... \n')

  ## Dynamic date range based on rainfall product
  output$daterange <- renderUI({
    dateRangeInput('daterange', "Date range:",
                   start = switch(input$ppt, 'TRMM' = '1998-01-01', 'GPM' = '2000-06-01'),
                   end = switch(input$ppt, 'TRMM' = '2019-11-30', 'GPM' = '2019-10-31'),
                   min = switch(input$ppt, 'TRMM' = '1998-01-01', 'GPM' = '2000-06-01'),
                   max = switch(input$ppt, 'TRMM' = '2019-11-30', 'GPM' = '2019-10-31'))
  })
  
  ## Hide download button until shp, acc, pass, and dir are provided
  observe({
    shinyjs::hide('button')
    req(shp_buffer())
    req(acc())
    req(pass())
    req(dir())
    shinyjs::show('button')
  })
  
  ## Getting root folder for folder choice
  roots = c(name = unlist(strsplit(getwd(), .Platform$file.sep))[1])
  shinyDirChoose(input, 'dir', roots = roots)
  dir <- reactive({
    return(print(parseDirPath(roots, input$dir)))
  })
  ## When chosen, print output dir to confirm
  output$dir <- renderPrint(dir())
  
  ## Maximum shapefile upload size = 50 mb
  options(shiny.maxRequestSize=50*1024^2)
  ## Upload shapefile
  uploadShpfile <- reactive({
    if (!is.null(input$shpFile)){
      shpDF <- input$shpFile
      prevWD <- getwd()
      uploadDirectory <- dirname(shpDF$datapath[1])
      setwd(uploadDirectory)
      for (i in 1:nrow(shpDF)){
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp$")]
      shpPath <- paste(uploadDirectory, shpName, sep="/")
      setwd(prevWD)
      shpFile <- readOGR(shpPath, verbose = F)
      shpFile <- spTransform(shpFile, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'))
      cat('\n Shapefile OK! \n')
      return(shpFile)
    } else { return() }
  })
  
  ## Grabbing input date-range
  data_range <- reactive({
    range <- switch(input$timescale,
           'Daily' = gsub('-','',seq.Date(from = input$daterange[1], to = input$daterange[2], by='day')),
           'Monthly' = gsub('-','',seq.Date(from = input$daterange[1]-day(input$daterange[1])+1, to = input$daterange[2]-day(input$daterange[2])+1, by='month')))
  })
  
  ## Coefficient to convert data units (monthly comes in mm/hr)
  correcao <- reactive({
    switch(input$timescale,
           'Daily' = rep(1, length(data_range())),
           'Monthly' = 24*days_in_month(seq.Date(from = input$daterange[1], to = input$daterange[2], by='month'))
    )
  })
  
  ## Storing account and password
  acc <- reactive({
    # acc <- URLencode(input$acc, reserved = T)
    acc <- input$acc
    return(acc)
  })
  pass <- reactive({
    # pass <- URLencode(input$pass, reserved = T)
    pass <- input$pass
    return(pass)
  })
  
  ## Getting list of files to download
  observeEvent(c(input$ppt, input$timescale), {
    switch(input$ppt, 
           'TRMM' = switch(input$timescale,
                           'Daily' = cat('- Daily TRMM list selected \n'),
                           'Monthly' = cat('- Monthly TRMM list selected \n')),
           'GPM' = switch(input$timescale,
                          'Daily' = cat('- Daily GPM list selected \n'),
                          'Monthly' = cat('- Monthly GPM list selected \n'))
           )
  })
  files_to_download <- reactive({
    lista <- switch(input$ppt,
                    'TRMM' = switch(input$timescale,
                           'Daily' = 'https://github.com/daniel-althoff/SPP-Down/raw/master/lists/TRMM_3B42_Daily_7.txt',
                           'Monthly' = 'https://github.com/daniel-althoff/SPP-Down/raw/master/lists/TRMM_3B43_Monthly_7.txt'),
                    'GPM' = switch(input$timescale,
                          'Daily' = 'https://github.com/daniel-althoff/SPP-Down/raw/master/lists/GPM_3IMERGDF_06_daily.txt',
                          'Monthly' = 'https://github.com/daniel-althoff/SPP-Down/raw/master/lists/GPM_3IMERGM_06_monthly.txt')
                    )
    lista <- lista %>% 
      read.delim(header=F) %>% 
      as_tibble() %>%
      rename(Link = 1) %>%
      mutate(Link = as.character(Link))
    lista <- lista[grep(x = lista$Link, pattern='.nc4'),]$Link
    return(lista)
  })
  
 
  ## Creating shape + buffer (if any)
  shp_buffer <- reactive({
    req(input$shpFile)
    shp <- spTransform(uploadShpfile(), CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'))
    options(warn = -1)
    shp <- buffer(shp, input$buffer)
    options(warn = 0)
    return(shp)
  })
  
  ## Download files
  observeEvent(input$button, {
    req(input$shpFile)
    
    ## Check existence/create download folder
    SPP_folder <- paste0(dir(),"\\SPP_folder")
    if(!file.exists(SPP_folder)) {dir.create(SPP_folder)}
    
    ## Get authorization in Earthdata
    req(input$acc)
    req(input$pass)
    cat('\n Checking authorization... \n')
    session <- html_session("https://urs.earthdata.nasa.gov/")
    form <- html_form(session)[[1]]
    form <- set_values(form, username = acc(), password = pass())
    session_open <- submit_form(session, form)
    
    ## Begin for loop download
    for(i in seq_along(data_range())){
      # Temp filename
      filename = tempfile(fileext = '.nc4')
      # Final save filename
      fileName <- switch(input$ppt, 
                         'TRMM' = switch(input$timescale,
                                        'Daily' = paste(SPP_folder,"\\TRMM_",data_range()[i],".tif",sep = ""),
                                        'Monthly' = paste(SPP_folder,"\\TRMM_",substr(data_range()[i],1,6),".tif",sep = "")),
                         'GPM' = switch(input$timescale,
                                        'Daily' = paste(SPP_folder,"\\GPM_",data_range()[i],".tif",sep = ""),
                                        'Monthly' = paste(SPP_folder,"\\GPM_",substr(data_range()[i],1,6),".tif",sep = ""))
                         )
      # Grab binary info and download to temporary file
      if (i==1) cat('Downloading... \n')
      file_to_download <- files_to_download()[grep(x = files_to_download(), pattern = data_range()[i])]
      session_down <- jump_to(session_open, file_to_download)
      writeBin(session_down$response$content, filename)
      
      addResourcePath("SPP_folder",SPP_folder)
      
      # Open .nc4 file and get coordinates
      XX <- ncdf4::nc_open(filename)
      # lon <- switch(input$ppt,
      #               'TRMM' = switch(input$timescale,
      #                               'Daily' = ncvar_get(XX, 'lon'),
      #                               'Monthly' = ncvar_get(XX, 'nlon')),
      #               'GPM' = switch(input$timescale,
      #                              'Daily' = ncvar_get(XX, 'lon'),
      #                              'Monthly' = ncvar_get(XX, 'lon'))
      #               )
      # nlon <- dim(lon)
      # 
      # lat <- switch(input$ppt,
      #               'TRMM' = switch(input$timescale,
      #                               'Daily' = ncvar_get(XX, 'lat'),
      #                               'Monthly' = ncvar_get(XX, 'nlat')),
      #               'GPM' = switch(input$timescale,
      #                              'Daily' = ncvar_get(XX, 'lat'),
      #                              'Monthly' = ncvar_get(XX, 'lat'))
      #               )
      # nlat <- dim(lat)
      
      layername <- switch(input$ppt,
                          'TRMM' = 'precipitation',
                          'GPM' = switch(input$timescale,
                                         'Daily' = 'precipitationCal',
                                         'Monthly' = 'precipitation')
                          )
      prec <- ncvar_get(XX, layername)
      fillv <- ncatt_get(XX,layername, '_FillValue')
      nc_close(XX)
      unlink(filename)
      
      # Convert to raster and save as .tif
      prec[prec == fillv$value] <- NA
      ylim <- switch(input$ppt,
                     'TRMM' = 50,
                     'GPM' = 90)
      r <- raster((prec), xmn=-180, xmx=180, ymn=-ylim, ymx=ylim, 
                  crs=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0')) %>%
        flip(direction='y')
      
      # crop/mask including border grid cells of shapefile
      options(warn = -1)
      cls <- cellFromPolygon(r, shp_buffer(), weights = TRUE)[[1]][, "cell"]
      r[][-cls] <- NA
      r <- trim(r)
      options(warn = 0)
      
      writeRaster(r*correcao()[i], fileName, format='GTiff', overwrite=T)
      cat(paste0(fileName,' saved... \n'))
    }
    cat('\n Done! \n')
  })
  
  
  ### Creating preview plot
  ## download average TRMM 1998-2017
  localtif <- reactive({
    req(input$shpFile)
    loadtif = tempfile(fileext = '.tif')
    download.file('https://github.com/daniel-althoff/SPP-Down/raw/master/www/1998_2017_avg.tif',
                  destfile = loadtif, mode='wb', quiet = T)
    localtif = raster(loadtif)
    crs(localtif) <- crs(shp_buffer())
    
    options(warn = -1)
    cls <- cellFromPolygon(localtif, shp_buffer(), weights = TRUE)[[1]][, "cell"]
    localtif[][-cls] <- NA
    localtif <- trim(localtif)
    options(warn = 0)
    
    unlink(loadtif)
    return(localtif)
  })
  
  ## Create a preview plot of shapefile and average annual rainfall
  output$AvgTRMM <- renderPlot({
    req(input$shpFile)
    
    img <- localtif()
    shp1 <- shp_buffer()
    shp2 <- uploadShpfile()
    
    levelplot(img, margin=F, 
              xlab='Latitude', ylab='Longitude', 
              main ='Preview: 1998-2017 average (TRMM)',
              colorkey=list(space='right',
                            axis.line=list(col='black')),
              par.settings=list(axis.line=list(col='transparent')),
              scales=list(draw=T), col.regions=viridis(n=101, direction=-1)) +           
      layer(sp.polygons(shp2, lwd=1), data = list(shp2 = shp2)) +
      layer(sp.polygons(shp1, lwd=1, lty=2), data = list(shp1=shp1))
  }, height = 225)
}


##### ShinyApp
shinyApp(ui = ui, server = server)

