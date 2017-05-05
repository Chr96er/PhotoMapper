#' Copy all imports to utilities.R since this file is not sourced during Build
#'@import shinyUtils
#'@import shiny
#'@importFrom leaflet renderLeaflet
#'@importFrom exif read_exif
#'@importFrom DT renderDataTable
#'@import magrittr
require(shiny)
require(shinyUtils)
require(PhotoMapper)
require(data.table)
require(magrittr)

options(shiny.maxRequestSize = 30 * 1024 ^ 2)
extension <- ".jpg"
translation <-
  as.data.table(read.table(
    file = "www/l10n.csv",
    sep = ",",
    header = T,
    as.is = T
  ))


server <- function(input, output, session) {
  tr <- function(text) {
    # translates text into current language
    sapply(text, function(s) {
      textTranslated <-
        unlist(translation[l10n == s, ifelse(is.null(input$language), "gb", input$language$iso2), with = F])
      ifelse(length(textTranslated) == 0, s, textTranslated) #if translation not found -> return current text
    }, USE.NAMES = F)
  }
  
  output$manual <- renderUI({
    manual(tr("manual"))
  })
  
  output$version <- renderUI({
    renderVersion(url = "https://github.com/Chr96er/PhotoMapper")
  })
  
  #ToDo: Responsive text
  output$body <- renderUI({
    tags$style(type = 'text/css',
               "footer{position: absolute; bottom:5%; left: 33%; padding:5px;}")
  })
  
  output$head <- renderUI({
    list(htmlStyle())
  })
  
  observeEvent({
    input$map_bounds
    input$imageSize
  }, {
    if (is.null(input$map_bounds)) {
      return(NULL)
    }
    shinyjs::runjs("if(typeof openPhotoSwipe === 'function'){
                   openPhotoSwipe();
  }")
    })
  
  output$main <- renderUI({
    if (is.null(input$mobileBrowser)) {
      return(NULL)
    }
    if (!input$mobileBrowser) {
      #   #Fullscreen mode, adapted from http://shiny.rstudio.com/gallery/superzip-example.html
      div(
        class = "outer",
        tags$head(# Include our custom CSS
          includeCSS("styles.css")),
        leaflet::leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = T,
          draggable = T,
          top = 60,
          left = "auto",
          right = 20,
          bottom = "auto",
          width = 430,
          height = "auto",
          h2("PhotoMapper"),
          insertMainTabpanel()
        )
      )
    } else {
      #mobile mode
      fluidRow(
        column(width = 3, insertMainTabpanel()),
        column(width = 6, leaflet::leafletOutput("map")),
        column(
          width = 3,
          absolutePanel(
            id = "photoswipeInner",
            class = "panel panel-default",
            fixed = T,
            # bottom = 0,
            # left = 20,
            # right = "auto",
            # top = "auto",
            width = input$imageSize,
            height = input$imageSize,
            insertPhotoswipe(),
            hidden = ""
          )
        )
      )
    }
  })
  
  output$photoswipe <- renderUI({
    div(
      absolutePanel(
        id = "photoswipeInner",
        class = "panel panel-default",
        fixed = T,
        bottom = 0,
        left = 20,
        right = "auto",
        top = "auto",
        width = input$imageSize,
        height = input$imageSize,
        insertPhotoswipe()
      )
    )
  })
  
  shinyInput <- function(FUN, id, num, value, offset, ...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      inputs[i] <-
        as.character(FUN(
          paste0(id, i + offset),
          label = NULL,
          value = value[i],
          ...
        ))
    }
    inputs
  }
  
  #observer for filenames reactiveValue
  filenames <- reactiveValues(local = NULL, original = NULL)
  observeEvent(input$example,
               {
                 cleanUp(path = normalizePath("www/images/converted"), extension)
                 filenames$local <-
                   imageDirectory("www/images/examples", extension)
                 filenames$original <-
                   imageDirectory("www/images/examples", extension)
               })
  observeEvent(input$loadImages,
               {
                 cleanUp(path = normalizePath("www/images/converted"), extension)
                 cleanUp(path = normalizePath("www/images/downloads"), extension)
                 urls <- strsplit(input$urls, "\\n")[[1]]
                 sapply(urls, function(x) {
                   download.file(
                     url = x,
                     destfile = paste0("www/images/downloads/", basename(x)),
                     mode = 'wb'
                   )
                 })
                 filenames$local <-
                   imageDirectory("www/images/downloads", extension)
                 filenames$original <-
                   imageDirectory("www/images/downloads", extension)
               })
  observeEvent(input$photos,
               {
                 photos <- input$photos
                 cleanUp(path = normalizePath("www/images/converted"), extension)
                 photoFilenames <-
                   as.matrix(photos)[, c("name", "datapath")]
                 names(photoFilenames) <- NULL
                 if(is.null(nrow(photoFilenames))){
                   localFilenames <- photoFilenames[2]
                   originalFilename <- photoFilenames[1]
                 } else {
                   localFilenames <- photoFilenames[, 2]
                   originalFilename <- photoFilenames[, 1]
                 }
                 file.rename(localFilenames, paste0(localFilenames, extension))
                 localFilenames <-
                   paste0(localFilenames, extension)
                 filenames$local <- localFilenames
                 filenames$original <-
                   basename(originalFilename)
               })
  
  computeExif <- reactive({
    if (is.null(filenames$local))
      return()
    exifFiles <- exif::read_exif(filenames$local)
    exifFiles$filename <- filenames$local
    exifFiles$baseFilename <- basename(filenames$local)
    exifFiles$originalFilename <- filenames$original
    exifFiles$shortName <- sapply(exifFiles$originalFilename,
                                  function(x) {
                                    substrRight(x, 23)
                                  })
    exifFiles$checked <- T
    exifFiles$missingTimestamp <- F
    exifFiles$missingLocation <- F
    exifFiles <-
      imputeExif(exifFiles, c("latitude", "longitude"), c(0.001, 0.001))
    #ToDo: Impute time just like location
    missingTimestamp <-
      which(exifFiles$digitised_timestamp == "")
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
    exifFiles <-
      exifFiles[order(exifFiles$digitised_timestamp), ]
    exifFiles$LatLonShort <-
      paste(
        round(exifFiles$longitude, digits = 2),
        round(exifFiles$latitude, digits = 2),
        sep = " : "
      )
    updateTabsetPanel(session = session,
                      inputId = "menuTabs",
                      selected = "filter")
    return(exifFiles)
  })
  
  output$filenames <- DT::renderDataTable({
    exifFiles <- mapPhotosFilter()
    if (is.null(exifFiles)) {
      return(NULL)
    }
    exifDT <-
      exifFiles[, c(
        "shortName",
        "digitised_timestamp",
        "LatLonShort",
        "missingLocation",
        "missingTimestamp"
      )]
    names(exifDT) <-
      tr(c(
        "Name",
        "Date/Time",
        "Latitude/Longitude",
        "Location",
        "Timestamp"
      ))
    rowSelection <- which(exifFiles$checked)
    DT::datatable(
      exifDT,
      filter = 'top',
      options = list(
        dom = "tip",
        drawCallback = DT::JS(
          'function(settings) {
          Shiny.bindAll(this.api().table().node());}'
        )
        ),
      escape = F,
      class = "compact",
      selection = list(mode = "multiple", selected = rowSelection),
      rownames = F
      )
  })
  
  mapPhotosFilter <-
    reactive({
      exifFiles <- computeExif()
      if (is.null(exifFiles)) {
        return(NULL)
      }
      ignoreMissingLocation <- input$ignoreMissingLocation
      ignoreMissingTimestamp <- input$ignoreMissingTimestamp
      
      exifFiles$checked = T
      
      if (ignoreMissingTimestamp &&
          length(which(exifFiles$missingTimestamp))) {
        exifFiles[which(exifFiles$missingTimestamp),]$checked = F
      }
      
      if (ignoreMissingLocation &&
          length(which(exifFiles$missingLocation))) {
        exifFiles[which(exifFiles$missingLocation),]$checked = F
      }
      
      return(exifFiles)
    })
  
  convertImages <- reactive({
    #trigger if new files have been imported or quality changed (todo: lazy conversion depending on selection)
    exifFiles <- computeExif()
    if (is.null(exifFiles)) {
      return(NULL)
    }
    imageQuality <- input$imageQuality
    cleanUp("www/images/converted/", extension = extension)
    exifFiles$temppath <-
      paste0("www/images/converted/",
             basename(exifFiles$filename))
    
    withProgress({
      for (i in 1:nrow(exifFiles)) {
        jpeg::writeJPEG(
          image = jpeg::readJPEG(exifFiles$filename[i], native = T),
          quality = imageQuality,
          target = exifFiles$temppath[i]
        )
        incProgress(1)
      }
    }, min = 1, max = nrow(exifFiles), message = tr("Converting images..."))
    
    #Rotate images in case such information is avilable
    for (i in 1:nrow(exifFiles)) {
      rotateImage(exifFiles$temppath[i], exifFiles$orientation[i])
    }
    return(exifFiles)
  })
  
  mapData <- reactive({
    exifFiles <- convertImages()
    if (is.null(exifFiles)) {
      return(NULL)
    }
    if (is.null(input$map_bounds)) {
      return(exifFiles)
    }
    exifFiles <- as.data.table(exifFiles[,-12])
    exifFiles[latitude < input$map_bounds$north &
                latitude > input$map_bounds$south &
                longitude < input$map_bounds$east &
                longitude > input$map_bounds$west]
  })
  
  observeEvent({
    input$photoswipe_index
    input$map_bounds
  }, {
    if (is.null(input$photoswipe_index)) {
      return(NULL)
    }
    exifFiles <- mapData()
    
    validate(need(
      !is.null(exifFiles) && nrow(exifFiles),
      message = tr(
        "No images selected or no images with required exif information available"
      )
    ))
    radiusHighlight <- rep(10, nrow(exifFiles))
    radiusHighlight[input$photoswipe_index + 1] <- 20
    fillOpacityHighlight <- rep(0.5, nrow(exifFiles))
    fillOpacityHighlight[input$photoswipe_index + 1] <- 0.8
    
    leaflet::leafletProxy("map", data = cbind(exifFiles$longitude, exifFiles$latitude)) %>%
      leaflet::clearMarkerClusters() %>%
      leaflet::addCircleMarkers(
        color = grDevices::rainbow(nrow(exifFiles), alpha = NULL),
        layerId = seq_len(nrow(exifFiles)),
        clusterOptions = leaflet::markerClusterOptions(),
        stroke = F,
        fillOpacity = fillOpacityHighlight,
        radius = radiusHighlight
      )
  })
  
  mouseover <- reactiveValues(toggle = T)
  observeEvent({
    input$map_marker_mouseover
    input$map_marker_mouseout
  }, {
    if (is.null(input$map_marker_mouseover) ||
        (
          mouseover$toggle &&
          !is.null(input$map_marker_mouseout$id) &&
          input$map_marker_mouseout$id == input$map_marker_mouseover$id
          #If mouseovertoggle is true and mouseout and mouseover have same id
          # --> This means that mouse left marker
        )) {
      mouseover$toggle <- F
    } else{
      mouseover$toggle <- T
    }
  })
  
  observeEvent(input$map_marker_click, {
    if (is.null(input$map_marker_click)) {
      return(NULL)
    }
    shinyjs::runjs(paste0("gallery.goTo(", input$map_marker_click$id - 1, ");"))
  })
  
  output$exifTable <- renderUI({
    selected <- input$map_marker_mouseover
    if (is.null(selected) || !mouseover$toggle) {
      return(NULL)
    }
    exifFiles <- mapData()
    exifFiles <- exifFiles[selected$id,]
    exifTemp <-
      exifFiles[, c("shortName",
                    "digitised_timestamp",
                    "LatLonShort",
                    "altitude")]
    names(exifTemp) <-
      tr(c("Name", "Date/Time", "Latitude/Longitude", "Altitude"))
    rownames(exifTemp) <- ""
    htmlTable::htmlTable(t(exifTemp), header = "EXIF-Information")
  })
  
  filteredData <- reactive({
    exifFiles <- convertImages()
    selectedRows <- input$filenames_rows_selected
    exifFiles <- exifFiles[selectedRows,]
  })
  
  output$photoswipeInitialization <- renderUI({
    exifFiles <- mapData()
    if (is.null(exifFiles)) {
      return(NULL)
    }
    validate(need(
      !is.null(exifFiles) && nrow(exifFiles),
      message = tr(
        "No images selected or no images with required exif information available"
      )
    ))
    #build items array for photoswipe
    normalizedImages <-
      normalizeImage(exifFiles$filename, input$imageSize, base = "height")
    items <-
      jsonlite::toJSON(as.data.frame(cbind(
        src = gsub("www/", "", exifFiles$filename),
        w = normalizedImages$width,
        h = round(normalizedImages$height, digits = 1)
      )))
    
    #Open photoswipe, i.e. set imagesize
    updateSliderInput(session = session,
                      inputId = "imageSize",
                      value = 300)
    
    #Initialize photoswipe
    tags$head(tags$script(HTML(
      paste0(
        "
        var openPhotoSwipe = function() {
        var pswpElement = document.querySelectorAll('.pswp')[0];
        
        // build items array
        var items = ",
        items,
        ";
        
        // define options (if needed)
        var options = {
        // optionName: 'option value'
        // for example:
        index: 0,// start at first slide
        modal: false,
        pinchToClose: false,
        closeOnScroll: false,
        closeOnVerticalDrag: false,
        escKey: false,
        tapToClose: false,
        clickToCloseNonZoomable: false,
        closeEl: false,
        closeElClasses: []
        };
        
        // Initializes and opens PhotoSwipe
        // not using *var* will ensure that PhotoSwipe becomes global and thus accessible within entire R session
        gallery = new PhotoSwipe( pswpElement, PhotoSwipeUI_Default, items, options);
        gallery.init();
        gallery.listen('afterChange', function() {
        // index - index of a slide that was loaded
        // item - slide object
        Shiny.onInputChange('photoswipe_index', gallery.getCurrentIndex());
        });
        };"
    )
      )))
})
  
  #' Wrapper for mapping images on leaflet map
  observe({
    exifFiles <- filteredData()
    validate(need(
      !is.null(exifFiles) && nrow(exifFiles),
      message = tr(
        "No images selected or no images with required exif information available"
      )
    ))
    radiusHighlight <- rep(10, nrow(exifFiles))
    radiusHighlight[1] <- 20
    fillOpacityHighlight <- rep(0.5, nrow(exifFiles))
    fillOpacityHighlight[1] <- 0.8
    leaflet::leafletProxy("map", data = cbind(exifFiles$longitude, exifFiles$latitude)) %>%
      leaflet::fitBounds(
        min(exifFiles$longitude),
        min(exifFiles$latitude),
        max(exifFiles$longitude),
        max(exifFiles$latitude)
      ) %>%
      leaflet::addCircleMarkers(
        color = grDevices::rainbow(nrow(exifFiles), alpha = NULL),
        layerId = seq_len(nrow(exifFiles)),
        clusterOptions = leaflet::markerClusterOptions(),
        stroke = F,
        fillOpacity = fillOpacityHighlight,
        radius = radiusHighlight
      )
  })
  
  #Initial rendering of map
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::setView(lng = 0,
                       lat = 0,
                       zoom = 2) %>%
      leaflet::addTiles()
  })
  
  insertMainTabpanel <- function() {
    tabsetPanel(
      id = "menuTabs",
      tabPanel(
        styledDiv("Import", "bold"),
        value = "Import",
        tabsetPanel(
          id = "filesTabs",
          tabPanel(
            styledDiv(tr("Local files"), "italic"),
            value = "Local files",
            fileInput(
              "photos",
              tr("Choose images"),
              accept = c("image/jpg", ".jpg"),
              multiple = T
            )
          ),
          tabPanel(
            styledDiv(tr("Online images"), "italic"),
            value = "Online images",
            tags$textarea(
              id = "urls",
              rows = 3,
              style = "width: 100%; font-size: 10px",
              placeholder = tr("Paste links")
            ),
            actionButton("loadImages", tr("Load images"))
          ),
          tabPanel(
            styledDiv("Demo", "italic"),
            value = "Demo",
            br(),
            actionButton("example", tr("Start demo"))
          )
        )
      ),
      tabPanel(
        styledDiv("Filter", "bold"),
        value = "filter",
        shinydashboard::box(
          title = tr("Imported files"),
          width = NULL,
          status = "primary",
          div(style = 'overflow-x: scroll', DT::dataTableOutput("filenames"))
        ),
        checkboxInput(
          "ignoreMissingLocation",
          tr("Ignore images with missing location"),
          value = F
        ),
        checkboxInput(
          "ignoreMissingTimestamp",
          tr("Ignore images with missing timestamp"),
          value = F
        )
      ),
      tabPanel(
        styledDiv("Display", "bold"),
        value = "Display",
        sliderInput(
          "imageQuality",
          tr("Image compression factor"),
          min = 0.05,
          max = 1,
          value = 0.3,
          ticks = T,
          step = 0.05
        ),
        sliderInput(
          "imageSize",
          tr("Image size (px)"),
          min = 0,
          max = 1000,
          value = 0,
          ticks = T,
          step = 1
        )
      )
    )
  }
}