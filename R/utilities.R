#'@importFrom exif read_exif
#'@importFrom imager save.image imrotate load.image
#'@importFrom rgdal GDALinfo
#'@import shiny
#'@import shinyUtils
#'@importFrom jpeg readJPEG writeJPEG
#'@importFrom leaflet leaflet addTiles addCircleMarkers
#'@importFrom grDevices rainbow

#'@export
launch_application <- function()
{
  runApp(appDir = system.file("application", package = "PhotoMapper"))
}

#'@export
rotateImage <- function(origin, orientation, target = origin) {
  if (orientation != 1 && orientation != 0) {
    imager::save.image(imager::imrotate(imager::load.image(origin),
                                        angle = switch(
                                          as.character(orientation),
                                          "6" =
                                            90,
                                          "3" =
                                            180,
                                          "8" =
                                            270
                                        )),
                       target)
  }
}

#ToDo: Apply actual imputation functions using timestamp
#'@export
imputeExif <-
  function(exif, fields, offset = rep(0, length(fields))) {
    lastValue = 0
    if (nrow(exif) == 1)
      return(exif)
    cachedIndex <- c()
    for (i in 1:nrow(exif)) {
      if (all(exif[i, fields] == 0)) {
        cachedIndex = c(cachedIndex, i)
      } else {
        cachedIndex = c(cachedIndex, i)
        lastValue = i
        exif[cachedIndex, fields] = t(apply(seq((
          length(cachedIndex) - 1
        ), 0) %o% offset, 1, function(x) {
          unlist(exif[lastValue, fields]) + x
        }))
        cachedIndex = c()
      }
    }
    if (lastValue == 0) {
      return(NULL)
    }
    if (length(cachedIndex) > 0)
      exif[cachedIndex, fields] = t(apply(seq(1, (
        length(cachedIndex)
      )) %o% offset, 1, function(x) {
        unlist(exif[lastValue, fields]) + x
      }))
    return(exif)
  }


### local images -----
#Based on code by https://github.com/environmentalinformatics-marburg/mapview/blob/master/R/popupImage.R
#'@export
popupLocalImage <- function(img, width, height) {
  info <-
    sapply(img, function(...)
      rgdal::GDALinfo(..., silent = TRUE))
  yx_ratio <-
    as.numeric(info["rows",]) / as.numeric(info["columns",])
  xy_ratio <-
    as.numeric(info["columns",]) / as.numeric(info["rows",])
  
  if (missing(height) && missing(width)) {
    width <- 300
    height <- yx_ratio * width
  } else if (missing(height))
    height <- yx_ratio * width
  else
    if (missing(width))
      width <- xy_ratio * height
  
  paste0("<image src='images/",
         basename(img),
         "' width=",
         width,
         " height=",
         height,
         ">")
  
}

#'@export
imageDirectory <-
  function(directory, extensions = c(".jpg", ".jpeg")) {
    photos <-
      dir(path = directory, pattern = paste0(".*(\\", paste(extensions, collapse = "|\\"), ")"))
    if (is.null(photos)) {
      #ToDo: Test
      return(NULL)
    } else{
      return(paste0(directory, "/", photos))
    }
  }

#'@export
mapPhotos <-
  function(filenames,
           imputeExif,
           ignoreMissingTimestamp,
           imageQuality,
           imageWidth,
           showProgress = T) {
    # exifFiles <- tryCatch(as.data.frame(as.matrix(t(sapply(filenames,read_exif)))),error=function(e) NULL) #works with master branch of exif
    exifFiles <- read_exif(filenames)
    # validate(need(!is.null(exifFiles),message = "One of the uploaded images contains broken EXIF information.
    # Upload of images from sources other than cameras is currently not supported!"))
    exifFiles$filename <- filenames
    if (imputeExif) {
      exifFiles <-
        imputeExif(exifFiles, c("latitude", "longitude"), c(0.01, 0.01))
    }
    
    #Ignore files without location information, and also files without timestamp if user selected this option
    excludeFiles <- which(exifFiles$latitude == 0)
    missingTimestamp <- which(exifFiles$digitised_timestamp == "")
    if (ignoreMissingTimestamp) {
      excludeFiles <- c(excludeFiles, missingTimestamp)
    } else{
      #Set timestamp to 1970
      exifFiles$digitised_timestamp[missingTimestamp] <-
        "1970:01:01 00:00:00"
      exifFiles$subsecond_timestamp[missingTimestamp] <-
        1:length(missingTimestamp)
    }
    if (length(excludeFiles) > 0) {
      exifFiles <-
        exifFiles[-excludeFiles,]
    }
    
    validate(
      need(!is.null(exifFiles) &&
             nrow(exifFiles) > 0, message = "None of the uploaded images contain EXIF location information.")
    )
    
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
    exifFiles <- exifFiles[order(exifFiles$digitised_timestamp),]
    exifFiles$LatLon <- cbind(exifFiles$longitude, exifFiles$latitude)
    
    exifFiles$temppath <-
      paste0("www/images/", basename(exifFiles$filename))
    
    if (showProgress) {
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
    } else{
      for (i in 1:nrow(exifFiles)) {
        jpeg::writeJPEG(
          image = readJPEG(exifFiles$filename[i], native = T),
          quality = imageQuality,
          target = exifFiles$temppath[i]
        )
      }
    }
    
    for (i in 1:nrow(exifFiles)) {
      rotateImage(exifFiles$temppath[i], exifFiles$orientation[i])
    }
    
    map <- leaflet::leaflet(matrix(unlist(exifFiles$LatLon), ncol = 2))
    map <- leaflet::addTiles(map)
    map <-
      leaflet::addCircleMarkers(
        map,
        color = grDevices::rainbow(nrow(exifFiles), alpha = NULL),
        popup = popupLocalImage(exifFiles$temppath, imageWidth)
      )
    return(map)
  }
