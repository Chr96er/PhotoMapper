#'@import shinyUtils
#'@import shiny
#'@importFrom leaflet renderLeaflet
#'@importFrom exif read_exif
library(exif)
library(shiny)
library(shinyUtils)
library(PhotoMapper)
library(gtools)

options(shiny.maxRequestSize = 30 * 1024 ^ 2)


extension <- ".jpg"

server <- function(input, output, session) {
  output$manual <- renderUI({
    manual(
      "The PhotoMapper application plots uploaded images on a map based on EXIF location and time information.
      Images without the required tags will be excluded. Note: all files are temporarily uploaded to shinyapps.io
      servers from which they will be deleted upon closing the application."
    )
  })
  
  output$version <- renderUI({
    renderVersion(url = "https://github.com/Chr96er/PhotoMapper")
  })
  
  output$head <- renderUI({
    list(htmlStyle())
  })
  
  #ToDo: Responsive text
  output$body <- renderUI({
    tags$style(type = 'text/css',
               "footer{position: absolute; bottom:5%; left: 33%; padding:5px;}")
  })
  
  #########################
  
  shinyInput <- function(FUN,id,num,value,offset,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      inputs[i] <- as.character(FUN(paste0(id,i + offset),label=NULL,value = value[i],...))
    }
    inputs
  }

  #observer for filenames reactiveValue
  filenames <- reactiveValues(data = NULL)
  observeEvent(input$example,
               {
                 cleanUp(path = normalizePath("www/images/converted/"), extension)
                 filenames$data = imageDirectory("www/images/examples", extension)
               })
  observeEvent(input$loadImages,
               {
                 cleanUp(path = normalizePath("www/images/converted/"), extension)
                 cleanUp(path = normalizePath("www/images/downloads/"), extension)
                 urls <- strsplit(input$urls, "\\n")[[1]]
                 sapply(urls, function(x) {
                   download.file(
                     url = x,
                     destfile = paste0("www/images/downloads/", basename(x)),
                     mode = 'wb'
                   )
                 })
                 filenames$data = imageDirectory("www/images/downloads", extension)
               })
  observeEvent(input$photos,
               { photos = input$photos
                 cleanUp(path = normalizePath("www/images/converted/"), extension)
                 localFilenames <- as.matrix(photos)[, "datapath"]
                 names(localFilenames) <- NULL
                 file.rename(localFilenames, paste0(localFilenames, extension))
                 localFilenames <- paste0(localFilenames, extension)
                 filenames$data = localFilenames
               })
    
  importFiles <- reactive({
    if(is.null(filenames$data)) return()
    else{
      return(filenames$data)
    }
  })
  
  
  computeExif <- reactive({
    filenames <- importFiles()
    if(is.null(filenames)){
      return(NULL)
    }
    exifFiles <- read_exif(filenames)
    exifFiles$filename <- filenames
    exifFiles$basefilename <- basename(filenames)
    exifFiles$checked <- T
    exifFiles$missingTimestamp <- F
    exifFiles$missingLocation <- F
    exifFiles <- imputeExif(exifFiles, c("latitude", "longitude"), c(0.001, 0.001))
    #ToDo: warning if none of the uploaded images contain location information
    # validate(need(!is.null(exifFiles) && nrow(exifFiles) > 0, message = "None of the uploaded images contain EXIF location information."))
    #ToDo: Impute time just like location
    missingTimestamp <- which(exifFiles$digitised_timestamp == "")
    #Set timestamp to 1970
    exifFiles$digitised_timestamp[missingTimestamp] <-
      "1970:01:01 00:00:00"
    #ToDo: Might lead to unexpected sideeffects if more than 999 timestamps missing
    exifFiles$subsecond_timestamp[missingTimestamp] <-
      1:length(missingTimestamp) 
    exifFiles$missingTimestamp[missingTimestamp] <- T
    # Parse time
    op <- options(digits.secs = 3)
    exifFiles$digitised_timestamp <-
      strptime(
        paste(
          exifFiles$digitised_timestamp,
          ".",
          exifFiles$subsecond_timestamp,
          "0",
          sep = ""
        ),
        format = "%Y:%m:%d %H:%M:%OS"
      )
    exifFiles <- exifFiles[order(exifFiles$digitised_timestamp), ]
    exifFiles$LatLon <-
      cbind(exifFiles$longitude, exifFiles$latitude)
    return(exifFiles)
  })
  
  currentCheckboxes <- reactiveValues(data = NULL)
  
  output$filenames <- DT::renderDataTable({
    exifFiles <- mapPhotosFilter()
    if(is.null(exifFiles)){
      return(NULL)
    }
    # pick <- paste0('<input type="checkbox" name="row', exifFiles$filename, '" value="', 1, '" ', exifFiles$checked,'>',"")
    exifDT <- cbind(Pick = shinyInput(checkboxInput,"srows_",nrow(exifFiles),value=exifFiles$checked,offset=currentCheckboxes$data, width=1),exifFiles[,c("basefilename","digitised_timestamp",
                                      "latitude","longitude", "altitude"
    )])
    names(exifDT) <- c("Pick", "Name", "Date/Time", "Latitude", "Longitude", "Altitude")
    DT::datatable(exifDT,options = list(dom = "tip",
                                        drawCallback = JS(
                                          'function(settings) {
                                           Shiny.bindAll(this.api().table().node());}')
                                        ),
                  escape = F,class = "compact",selection = list(mode="single"))
      })
  
  
  #'@export
  mapPhotosFilter <-
    reactive({
      exifFiles <- computeExif()
      if(is.null(exifFiles)){
        return(NULL)
      }
      ignoreMissingLocation <- input$ignoreMissingLocation
      ignoreMissingTimestamp <- input$ignoreMissingTimestamp
      
      exifFiles$checked = T
      
      if(ignoreMissingTimestamp){
        exifFiles[which(exifFiles$missingTimestamp),]$checked = F
      }
      
      if(ignoreMissingLocation){
        exifFiles[which(exifFiles$missingLocation),]$checked = F
      }
      
      return(exifFiles)
    })
  
  convertImages <- reactive({
    #trigger if new files have been imported or quality changed (todo: lazy conversion depending on selection)
    exifFiles <- computeExif()
    if(is.null(exifFiles)){
      return(NULL)
    }
    imageQuality <- input$imageQuality
    cleanUp("www/images/converted/",extension = extension)
    exifFiles$temppath <-
      paste0("www/images/converted/", basename(exifFiles$filename))
    
    withProgress({
      for (i in 1:nrow(exifFiles)) {
        jpeg::writeJPEG(
          image = jpeg::readJPEG(exifFiles$filename[i], native = T),
          quality = imageQuality,
          target = exifFiles$temppath[i]
        )
        incProgress(1)
      }
    }, min = 1, max = nrow(exifFiles), message = "Converting images...")
    
    for (i in 1:nrow(exifFiles)) {
      rotateImage(exifFiles$temppath[i], exifFiles$orientation[i])
    }
    return(exifFiles)
  })
  
  rowsSelected <- reactiveValues(data = NULL)
  
  observeEvent(input$refresh,{
    rows <- names(input)[grepl(pattern = "srows_",names(input))]
    rows <- mixedsort(rows)
    offset <- isolate(currentCheckboxes$data)
    rowsSelected$data <- as.numeric(paste(unlist(lapply(rows,function(i){
      if(input[[i]]){
        currentRowIndex <- as.numeric(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
        if(currentRowIndex > offset){
          return(currentRowIndex - offset) 
        }
      }
    }))))
  })
  
  observeEvent(filenames$data,{
    rows=names(input)[grepl(pattern = "srows_",names(input))]
    currentCheckboxes$data <- max(as.numeric(paste(unlist(lapply(rows,function(i){
      if(input[[i]]){
        return(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
      }
    })))),0)
  })
  
  #' Wrapper for mapping images on leaflet map
  output$map <- leaflet::renderLeaflet({
    exifFiles <- convertImages()
    selectedRows <- rowsSelected$data
    exifFiles <- exifFiles[selectedRows,]
    if(is.null(exifFiles)){
      return(NULL)
    }

    imageWidth <- input$imageWidth

    validate(
      need(nrow(exifFiles) > 0, message = "None of the uploaded images contain EXIF location information.")
    )

    map <-
      leaflet::leaflet(matrix(unlist(exifFiles$LatLon), ncol = 2))
    map <- leaflet::addTiles(map)
    map <-
      leaflet::addCircleMarkers(
        map,
        color = grDevices::rainbow(nrow(exifFiles), alpha = NULL),
        popup = popupLocalImage(exifFiles$temppath, imageWidth)
      )
    return(map)
  })
  }