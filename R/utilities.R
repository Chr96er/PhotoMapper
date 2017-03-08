#'@importFrom exif read_exif
#'@importFrom imager save.image imrotate load.image
#'@importFrom rgdal GDALinfo
#'@import shiny
#'@import shinyUtils
#'@importFrom jpeg readJPEG writeJPEG
#'@importFrom leaflet leaflet addTiles addCircleMarkers
#'@importFrom grDevices rainbow

extension <- ".jpg"

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
    lastValue <- 0
    imputedValues <- c()
    if (nrow(exif) == 1)
      return(exif)
    cachedIndex <- c()
    for (i in 1:nrow(exif)) {
      if (all(exif[i, fields] == 0)) {
        cachedIndex <- c(cachedIndex, i)
        imputedValues <- c(imputedValues , i)
      } else {
        cachedIndex <- c(cachedIndex, i)
        lastValue <- i
        exif[cachedIndex, fields] <- t(apply(seq((
          length(cachedIndex) - 1
        ), 0) %o% offset, 1, function(x) {
          unlist(exif[lastValue, fields]) + x
        }))
        cachedIndex <- c()
      }
    }
    if (lastValue == 0) {
      return(exif)
    }
    if (length(cachedIndex) > 0)
      exif[cachedIndex, fields] <- t(apply(seq(1, (
        length(cachedIndex)
      )) %o% offset, 1, function(x) {
        unlist(exif[lastValue, fields]) + x
      }))
    exif$missingLocation = 1:nrow(exif)%in%imputedValues
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
  
  paste0("<image src='images/converted/",
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
cleanUp <- function(path, extension, exclude = ".*README.*"){
  jpgFiles <- getJpgFiles(path, extension, exclude)
  if(!is.null(jpgFiles)){
    file.remove(jpgFiles)  
  }
}

#'@export
getJpgFiles <- function(path, extension, exclude = ".*README.*"){
  files <- dir(path, paste0(".*\\", extension))
  if(length(files) > 0){
    return(paste0(path,files)) 
  } else {
    return(NULL)
  }
}