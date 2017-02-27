require(shiny)
require(shinythemes)

fluidPage(theme = "bootstrap.css",
          titlePanel("PhotoMapper"),
          uiOutput("head"),
          uiOutput("body"),
          uiOutput("manual"),
          sidebarLayout(
            sidebarPanel(
              sliderInput("imageQuality","Image compression factor",min = 0.05,max=1,value = 0.3,ticks = T,step = 0.05),
              sliderInput("imageWidth","Image width (px)",min = 0,max=1000,value = 300,ticks = T,step = 1),
              checkboxInput("imputeExif","Add missing EXIF location information (experimental)",value = F),
              fileInput("photos","Choose images",accept = c("image/jpg",".jpg"),multiple = T)
              #todo: input textfield for urls
            ),
            mainPanel(leaflet::leafletOutput("map"), uiOutput("version"))
            ))
