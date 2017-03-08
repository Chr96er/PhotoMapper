#'@import shiny
#'@importFrom leaflet leafletOutput
library(shinyUtils)
library(shinydashboard)
library(DT)
#########################

fluidPage(
  theme = "bootstrap.css",
  titlePanel("PhotoMapper"),
  uiOutput("head"),
  uiOutput("body"),
  uiOutput("manual"),
  textOutput("variables"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "menuTabs",
        tabPanel(
          styledDiv("Import", "bold"),
          tabsetPanel(
            tabPanel(
              styledDiv("Local files", "italic"),
              fileInput(
                "photos",
                "Choose images",
                accept = c("image/jpg", ".jpg"),
                multiple = T,
                progressLabelAlignment = "center"
              )
            ),
            tabPanel(
              styledDiv("Online files", "italic"),
              tags$textarea(
                id = "urls",
                rows = 3,
                style = "width: 100%; font-size: 10px",
                placeholder = "Paste links"
              ),
              actionButton("loadImages", "Load images")
            ),
            tabPanel(
              styledDiv("Demo", "italic"),
              br(),
              actionButton("example", "Start demo")
            )
          )
        ),
        tabPanel(
          styledDiv("Filter", "bold"),
          value = "filter",
          shinydashboard::box(
            title = "Imported files",
            width = NULL,
            status = "primary",
            div(style = 'overflow-x: scroll', DT::dataTableOutput("filenames"))
          ),
          checkboxInput(
            "ignoreMissingLocation",
            "Ignore images with missing location",
            value = F
          ),
          checkboxInput(
            "ignoreMissingTimestamp",
            "Ignore images with missing timestamp",
            value = F
          ),
          actionButton("refresh", "Refresh")
        ),
        tabPanel(
          styledDiv("Display", "bold"),
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
          )
        )
      ),
      width = 3
    ),
    mainPanel(leaflet::leafletOutput("map"), uiOutput("version"))
  )
)
