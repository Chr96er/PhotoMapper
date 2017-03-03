popupLocalImage <-
function(img, width, height) {
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
