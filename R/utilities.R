#'@importFrom exif read_exif
#'@importFrom imager save.image imrotate load.image
#'@importFrom jpeg readJPEG writeJPEG
#'@importFrom leaflet leaflet addTiles addCircleMarkers
#'@importFrom grDevices rainbow
#'@import shinyUtils
#'@importFrom leaflet renderLeaflet
#'@importFrom exif read_exif
#'@importFrom DT renderDataTable
#'@import shiny
#'@importFrom leaflet leafletOutput
#'@importFrom DT dataTableOutput
#'@importFrom shinydashboard box
#'@import magrittr
#'@import data.table
#'@import tidyr

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
    if (nrow(exif) == 1) {
      if (all(exif[1, fields] == 0)) {
        exif$missingLocation <- T
      }
      return(exif)
    }
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
    exif$missingLocation <- 1:nrow(exif) %in% imputedValues
    return(exif)
  }


### local images -----
#Based on code by https://github.com/environmentalinformatics-marburg/mapview/blob/master/R/popupImage.R
#'@export
popupLocalImage <-
  function(filename,
           tooltipText = "",
           size) {
    imageDimensions <- normalizeImage(filename, size)
    return(HTML(
      paste0(
        "<style> div.leaflet-popup-content {width:auto !important;}</style>, <img src='images/converted/",
        basename(filename),
        "' width='",
        imageDimensions$width,
        "' height='",
        imageDimensions$height,
        "' />"
      )
    ))
  }

#'@export
normalizeImage <-
  function(filenames,
           size = 300,
           base = c("max", "width", "height")) {
    as.data.frame(t(sapply(filenames, function(filename) {
      imgCached <- jpeg::readJPEG(filename)
      yx_ratio <-
        nrow(imgCached) / ncol(imgCached)
      if ((base == "height") || (base == "max" && yx_ratio > 1)) {
        #More rows than columns -> size is basis for height
        height <- size
        width <- height / yx_ratio
      } else{
        width <- size
        height <- yx_ratio * width
      }
      return(c(height = height, width = width))
    })))
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
cleanUp <- function(path, extension, exclude = ".*README.*") {
  path <- normalizePath(path)
  jpgFiles <- getJpgFiles(path, extension, exclude)
  if (!is.null(jpgFiles)) {
    file.remove(jpgFiles)
  }
}

#'@export
getJpgFiles <- function(path, extension, exclude = ".*README.*") {
  path <- normalizePath(path)
  files <- dir(path, paste0(".*\\", extension))
  if (length(files) > 0) {
    return(paste0(path, "/", files))
  } else {
    return(NULL)
  }
}

#'@export
substrRight <- function(x, n) {
  if (n < (nchar(x) - 3)) {
    return(paste0("...", substr(x, nchar(x) - n + 1, nchar(x))))
  } else{
    return(x)
  }
}