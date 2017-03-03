imputeExif <-
function(exif, fields, offset = rep(0,length(fields))) {
  lastValue = 0
  if (nrow(exif) == 1)
    return(exif)
  cachedIndex <- c()
  for (i in 1:nrow(exif)) {
    if (all(exif[i, fields] == 0)) {
      cachedIndex = c(cachedIndex, i)
    } else {
      cachedIndex = c(cachedIndex, i)
      lastValue = i
      exif[cachedIndex, fields] = t(apply(seq((length(cachedIndex)-1),0)%o%offset,1,function(x){unlist(exif[lastValue, fields]) + x}))
      cachedIndex = c()
    }
  }
  if(lastValue == 0){
    return(NULL)
  }
  if (length(cachedIndex) > 0)
    exif[cachedIndex, fields] = t(apply(seq(1,(length(cachedIndex)))%o%offset,1,function(x){unlist(exif[lastValue, fields]) + x}))
  return(exif)
}
