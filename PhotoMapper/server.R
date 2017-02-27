# devtools::install_github("ironholds/exif")
library(exif) #install.packages("C:\\Users\\Chris\\exif_0.1.0.tar\\exif_0.1.0_try.tar",repos = NULL, type="source")
library(shiny)
require(ggmap)
require(leaflet)
# require(mapview) #cannot be installed by shinyapps.io
require(jpeg)
library(imager)
library(htmlwidgets)
source("utilities.R")
source("shinyUtilities.R")

VERSION = strsplit(gsub(".VERSION",replacement = "",dir()[grep(dir(),pattern = "VERSION")]),"\\.")[[1]] 

options(shiny.maxRequestSize=30*1024^2)

extension <- ".jpg"

server <- function(input, output) {
  
  
  output$manual <- renderUI({
    manual("The PhotoMapper application plots uploaded images on a map based on EXIF location and time information. Images without the required tags will be excluded. Note: all files are temporarily uploaded to shinyapps.io servers from which they will be deleted upon closing the application.")
  })
  
  output$version <- renderUI({
    list(tags$p("Source code available under https://github.com/Chr96er/PhotoMapper"),
         tags$p("Version ", paste(VERSION[1:3],collapse = "."), "; build ", VERSION[4],align="right"))
  })

  output$head <- renderUI({
    htmlStyle()
  })
  
  output$map <- renderLeaflet({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    photos <- input$photos
    
    if (is.null(photos)){
      return(NULL)
    }else{
      return(mapPhotos(photos))
    }
  }) 
  
  mapPhotos <- function(photos){
    filenames <- as.matrix(photos)[,"datapath"]
    names(filenames) <- NULL
    file.rename(filenames,paste0(filenames,extension))
    filenames <- paste0(filenames,extension)
    exifFiles <- as.data.frame(as.matrix(t(sapply(filenames,read_exif))))
    # exifFiles <-
    #   read_exif(filenames)
    exifFiles$filename <- filenames
    if(input$imputeExif){
      exifFiles <- imputeExif(exifFiles, c("latitude", "longitude"), c(0.01,0.01))  
    }
    
    excludeFiles <-
      c(which(exifFiles$digitised_timestamp == ""),
        which(exifFiles$latitude == 0))
    if (length(excludeFiles) > 0) {
      exifFiles <-
        exifFiles[-excludeFiles, ]
    }
    
    #Todo: If timestamp is missing, don't exclude image but handle carefully    
    validate(need(!is.null(exifFiles) && nrow(exifFiles)>0,message = "None of the uploaded images contain EXIF location information as well as timestamp."))
    
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
    withProgress({
      for(i in 1:nrow(exifFiles)){
        jpeg::writeJPEG(
          image = readJPEG(exifFiles$filename[i], native = T),
          quality = input$imageQuality,
          target = exifFiles$temppath[i]
        )
        incProgress(1)
      }},min = 1,max=nrow(exifFiles),message = "Converting images...")
    
    for(i in 1:nrow(exifFiles)){
      rotateImage(exifFiles$temppath[i],exifFiles$orientation[i])
    }
    
    map <- leaflet(matrix(unlist(exifFiles$LatLon),ncol = 2))
    map <- addTiles(map)
    map <-
      addCircleMarkers(
        map,
        color = rainbow(nrow(exifFiles), alpha = NULL),
        popup = popupLocalImage(exifFiles$temppath,input$imageWidth)
      )
    return(map)
  }
  
  ### local images -----
  #Based on code by https://github.com/environmentalinformatics-marburg/mapview/blob/master/R/popupImage.R
  popupLocalImage <- function(img, width, height) {
    info <- sapply(img, function(...) rgdal::GDALinfo(..., silent = TRUE))
    yx_ratio <- as.numeric(info["rows", ]) / as.numeric(info["columns", ])
    xy_ratio <- as.numeric(info["columns", ]) / as.numeric(info["rows", ])
    
    if (missing(height) && missing(width)) {
      width <- 300
      height <- yx_ratio * width
    } else if (missing(height)) height <- yx_ratio * width else
      if (missing(width)) width <- xy_ratio * height
    
    paste0("<image src='images/",
           basename(img),
           "' width=",
           width,
           " height=",
           height,
           ">")
    
  }
  
}