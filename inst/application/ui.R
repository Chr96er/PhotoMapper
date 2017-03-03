#'@import shiny
#'@importFrom leaflet leafletOutput

fluidPage(
  theme = "bootstrap.css",
  titlePanel("PhotoMapper"),
  uiOutput("head"),
  uiOutput("body"),
  uiOutput("manual"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "imageQuality",
        "Image compression factor",
        min = 0.05,
        max = 1,
        value = 0.3,
        ticks = T,
        step = 0.05
      ),
      sliderInput(
        "imageWidth",
        "Image width (px)",
        min = 0,
        max = 1000,
        value = 300,
        ticks = T,
        step = 1
      ),
      checkboxInput(
        "imputeExif",
        "Add missing EXIF location information (experimental)",
        value = F
      ),
      checkboxInput(
        "ignoreMissingTimestamp",
        "Ignore images with missing timestamp",
        value = F
      ),
      tabsetPanel(
        tabPanel(
          "Local files",
          fileInput(
            "photos",
            "Choose images",
            accept = c("image/jpg", ".jpg"),
            multiple = T,
            progressLabelAlignment = "center"
          )
        ),
        tabPanel(
          "Online files",
          tags$textarea(id = "urls", rows = 3, style = "width: 100%; font-size: 10px"),
          tags$script(
            "function resizeTextarea (id) {
            var a = document.getElementById(id);
            a.style.height = 'auto';
            a.style.height = a.scrollHeight+'px';
            }
            
            function init() {
            var a = document.getElementsByTagName('textarea');
            for(var i=0,inb=a.length;i<inb;i++) {
            if(a[i].getAttribute('data-resizable')=='true')
            resizeTextarea(a[i].id);
            }
            }
            
            addEventListener('DOMContentLoaded', init);"
          ),
          actionButton("loadImages", "Load images")
          )
        ),
      br(),
      actionButton("example", "Demo using example images")
      #todo: input textfield for urls
        ),
    mainPanel(leaflet::leafletOutput("map"), uiOutput("version"))
        )
      )
