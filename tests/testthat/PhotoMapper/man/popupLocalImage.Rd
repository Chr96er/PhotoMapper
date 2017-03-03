\name{popupLocalImage}
\alias{popupLocalImage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
popupLocalImage(img, width, height)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{
%%     ~~Describe \code{img} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
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
function (img, width, height) 
{
    info <- sapply(img, function(...) rgdal::GDALinfo(..., silent = TRUE))
    yx_ratio <- as.numeric(info["rows", ])/as.numeric(info["columns", 
        ])
    xy_ratio <- as.numeric(info["columns", ])/as.numeric(info["rows", 
        ])
    if (missing(height) && missing(width)) {
        width <- 300
        height <- yx_ratio * width
    }
    else if (missing(height)) 
        height <- yx_ratio * width
    else if (missing(width)) 
        width <- xy_ratio * height
    paste0("<image src='images/", basename(img), "' width=", 
        width, " height=", height, ">")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
