\name{imputeExif}
\alias{imputeExif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
imputeExif(exif, fields, offset = rep(0, length(fields)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{exif}{
%%     ~~Describe \code{exif} here~~
}
  \item{fields}{
%%     ~~Describe \code{fields} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
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
function (exif, fields, offset = rep(0, length(fields))) 
{
    lastValue = 0
    if (nrow(exif) == 1) 
        return(exif)
    cachedIndex <- c()
    for (i in 1:nrow(exif)) {
        if (all(exif[i, fields] == 0)) {
            cachedIndex = c(cachedIndex, i)
        }
        else {
            cachedIndex = c(cachedIndex, i)
            lastValue = i
            exif[cachedIndex, fields] = t(apply(seq((length(cachedIndex) - 
                1), 0) \%o\% offset, 1, function(x) {
                unlist(exif[lastValue, fields]) + x
            }))
            cachedIndex = c()
        }
    }
    if (lastValue == 0) {
        return(NULL)
    }
    if (length(cachedIndex) > 0) 
        exif[cachedIndex, fields] = t(apply(seq(1, (length(cachedIndex))) \%o\% 
            offset, 1, function(x) {
            unlist(exif[lastValue, fields]) + x
        }))
    return(exif)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
