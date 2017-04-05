#' Copy all imports to utilities.R since this file is not sourced during Build
#'@import shiny
#'@importFrom leaflet leafletOutput
#'@importFrom DT dataTableOutput
#'@importFrom shinyDashboard box
library(shinyUtils)
#########################

fluidPage(
  theme = "bootstrap.css",
  titlePanel("PhotoMapper"),
  tags$head(
    tags$link(href = "tooltipster.bundle.min.css", rel = "stylesheet")
  ),
  tags$head(tags$script(src = "tooltipster.bundle.min.js")),
  tags$head(
    tags$link(href = "countryselect/css/countrySelect.css", rel = "stylesheet")
  ),
  tags$head(tags$script(src = "countryselect/js/countrySelect.js")),
  tags$head(
    HTML(
      '<input type="text" id="country">
      <input type="hidden" id="country_code" />
      <script>
      $("#country").countrySelect({
      onlyCountries: ["gb", "de"]
      });
      </script>'
    )
    ),
  tags$head(
    tags$script(
      "
      $(function(){
      $('body').on('focusout', '#country', function(){
      var language = $(this).countrySelect('getSelectedCountryData');
      Shiny.onInputChange('language', language);
      });
      })"
    )
    ),
  uiOutput("head"),
  uiOutput("body"),
  uiOutput("manual"),
  uiOutput("fluidRow1"),
  fluidRow(column(
    10, offset = 2, uiOutput("version"), uiOutput("mapJS")
  ))
  )
