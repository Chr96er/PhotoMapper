#'@import shinyUtils
#'@import shiny
#'@importFrom leaflet renderLeaflet

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
  
  #' Wrapper for mapping images on leaflet map
  #'
  #' Triggered when example button is pressed or files have been selected
  output$map <- leaflet::renderLeaflet({
    #input$example defaults to 0 and changes its value upon pressing button
    if (input$example) {
      return(mapPhotosWrapper(imageDirectory("www/images/examples", extension)))
    }
    
    #input$loadImages defaults to 0 and changes its value upon pressing button
    if (input$loadImages) {
      urls <- strsplit(input$urls, "\\n")[[1]]
      sapply(urls, function(x) {
        download.file(
          url = x,
          destfile = paste0("www/images/downloads/", basename(x)),
          mode = 'wb'
        )
      })
      return(mapPhotosWrapper(imageDirectory("www/images/downloads", extension)))
    }
    
    #input$photos defaults to NULL and changes its value upon selecting files
    photos <- input$photos
    
    if (is.null(photos)) {
      return(NULL)
    } else{
      filenames <- as.matrix(photos)[, "datapath"]
      names(filenames) <- NULL
      file.rename(filenames, paste0(filenames, extension))
      filenames <- paste0(filenames, extension)
      return(mapPhotosWrapper(filenames))
    }
  })
  
  mapPhotosWrapper <- function(filenames) {
    mapPhotos(
      filenames,
      input$imputeExif,
      input$ignoreMissingTimestamp,
      input$imageQuality,
      input$imageWidth,
      T
    )
  }
  }
