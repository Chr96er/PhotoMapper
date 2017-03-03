imageDirectory <-
function(directory){
  photos <- dir(path = directory)
  if (is.null(photos)) {
    #ToDo: Test
    return(NULL)
  } else{
    return(paste0(directory,"/",photos))
  } 
}
