server <-
function(input, output) {
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
  
  #' Wrapper for mapping images on leaflet map
  #' 
  #' Triggered when example button is pressed or files have been selected
  output$map <- renderLeaflet({
    #input$example defaults to 0 and changes its value upon pressing button
    if(input$example){
      return(mapPhotosWrapper(imageDirectory("www/images/examples")))
    }
    
    #input$loadImages defaults to 0 and changes its value upon pressing button
    if(input$loadImages){
      urls <- strsplit(input$urls,"\\n")[[1]]
      sapply(urls, function(x) {download.file(url = x, destfile = paste0("www/images/downloads/", basename(x)), mode = 'wb')})
      return(mapPhotosWrapper(imageDirectory("www/images/downloads")))
    }
    
    #input$photos defaults to NULL and changes its value upon selecting files
    photos <- input$photos
    
    if (is.null(photos)) {
      return(NULL)
    } else{
      filenames <- as.matrix(photos)[,"datapath"]
      names(filenames) <- NULL
      file.rename(filenames,paste0(filenames,extension))
      filenames <- paste0(filenames,extension)
      return(mapPhotosWrapper(filenames))
    }
  })
  
  mapPhotosWrapper <- function(filenames){
    mapPhotos(filenames,input$imputeExif,input$ignoreMissingTimestamp,input$imageQuality,input$imageWidth) 
  }
  
  #'Test cases: Zero files, 1/multiple broken file/no exif, 1 valid exif file, multiple valid exif files, mixed broken/valid exif files
  #'
  mapPhotos <- function(filenames,imputeExif,ignoreMissingTimestamp,imageQuality,imageWidth){
    # exifFiles <- tryCatch(as.data.frame(as.matrix(t(sapply(filenames,read_exif)))),error=function(e) NULL) #works with master branch of exif
    exifFiles <- read_exif(filenames)
    # validate(need(!is.null(exifFiles),message = "One of the uploaded images contains broken EXIF information. 
    # Upload of images from sources other than cameras is currently not supported!"))
    exifFiles$filename <- filenames
    if(imputeExif){
      exifFiles <- imputeExif(exifFiles, c("latitude", "longitude"), c(0.01,0.01))  
    }
    
    #Ignore files without location information, and also files without timestamp if user selected this option
    excludeFiles <- which(exifFiles$latitude == 0)
    missingTimestamp <- which(exifFiles$digitised_timestamp == "")
    if(ignoreMissingTimestamp){
      excludeFiles <- c(excludeFiles, missingTimestamp)
    }else{
      #Set timestamp to 1970
      exifFiles$digitised_timestamp[missingTimestamp] <- "1970:01:01 00:00:00"
      exifFiles$subsecond_timestamp[missingTimestamp] <- 1:length(missingTimestamp)
    }
    if (length(excludeFiles) > 0) {
      exifFiles <-
        exifFiles[-excludeFiles, ]
    }
    
    validate(need(!is.null(exifFiles) && nrow(exifFiles)>0,message = "None of the uploaded images contain EXIF location information."))
    
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
    exifFiles$LatLon <- cbind(exifFiles$longitude, exifFiles$latitude)
    
    exifFiles$temppath <- paste0("www/images/",basename(exifFiles$filename))
    # withProgress({
      for(i in 1:nrow(exifFiles)){
        jpeg::writeJPEG(
          image = readJPEG(exifFiles$filename[i], native = T),
          quality = imageQuality,
          target = exifFiles$temppath[i]
        )
        # incProgress(1)
      }
        # },min = 1,max=nrow(exifFiles),message = "Converting images...")
    
    for(i in 1:nrow(exifFiles)){
      rotateImage(exifFiles$temppath[i],exifFiles$orientation[i])
    }
    
    map <- leaflet(matrix(unlist(exifFiles$LatLon),ncol = 2))
    map <- addTiles(map)
    map <-
      addCircleMarkers(
        map,
        color = rainbow(nrow(exifFiles), alpha = NULL),
        popup = popupLocalImage(exifFiles$temppath,imageWidth)
      )
    return(map)
  }
    
}
