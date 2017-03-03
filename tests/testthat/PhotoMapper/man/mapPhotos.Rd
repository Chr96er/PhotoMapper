\name{mapPhotos}
\alias{mapPhotos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mapPhotos(filenames, imputeExif, ignoreMissingTimestamp, imageQuality, imageWidth)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenames}{
%%     ~~Describe \code{filenames} here~~
}
  \item{imputeExif}{
%%     ~~Describe \code{imputeExif} here~~
}
  \item{ignoreMissingTimestamp}{
%%     ~~Describe \code{ignoreMissingTimestamp} here~~
}
  \item{imageQuality}{
%%     ~~Describe \code{imageQuality} here~~
}
  \item{imageWidth}{
%%     ~~Describe \code{imageWidth} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filenames, imputeExif, ignoreMissingTimestamp, imageQuality, 
    imageWidth) 
{
    exifFiles <- read_exif(filenames)
    exifFiles$filename <- filenames
    if (imputeExif) {
        exifFiles <- imputeExif(exifFiles, c("latitude", "longitude"), 
            c(0.01, 0.01))
    }
    excludeFiles <- which(exifFiles$latitude == 0)
    missingTimestamp <- which(exifFiles$digitised_timestamp == 
        "")
    if (ignoreMissingTimestamp) {
        excludeFiles <- c(excludeFiles, missingTimestamp)
    }
    else {
        exifFiles$digitised_timestamp[missingTimestamp] <- "1970:01:01 00:00:00"
        exifFiles$subsecond_timestamp[missingTimestamp] <- 1:length(missingTimestamp)
    }
    if (length(excludeFiles) > 0) {
        exifFiles <- exifFiles[-excludeFiles, ]
    }
    validate(need(!is.null(exifFiles) && nrow(exifFiles) > 0, 
        message = "None of the uploaded images contain EXIF location information."))
    op <- options(digits.secs = 3)
    exifFiles$digitised_timestamp <- strptime(paste(exifFiles$digitised_timestamp, 
        ".", exifFiles$subsecond_timestamp, "0", sep = ""), format = "\%Y:\%m:\%d \%H:\%M:\%OS")
    exifFiles <- exifFiles[order(exifFiles$digitised_timestamp), 
        ]
    exifFiles$LatLon <- cbind(exifFiles$longitude, exifFiles$latitude)
    exifFiles$temppath <- paste0("www/images/", basename(exifFiles$filename))
    for (i in 1:nrow(exifFiles)) {
        jpeg::writeJPEG(image = readJPEG(exifFiles$filename[i], 
            native = T), quality = imageQuality, target = exifFiles$temppath[i])
    }
    for (i in 1:nrow(exifFiles)) {
        rotateImage(exifFiles$temppath[i], exifFiles$orientation[i])
    }
    map <- leaflet(matrix(unlist(exifFiles$LatLon), ncol = 2))
    map <- addTiles(map)
    map <- addCircleMarkers(map, color = rainbow(nrow(exifFiles), 
        alpha = NULL), popup = popupLocalImage(exifFiles$temppath, 
        imageWidth))
    return(map)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
