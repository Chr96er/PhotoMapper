rotateImage <-
function(origin, orientation, target = origin) {
  if (orientation != 1 && orientation != 0) {
    save.image(imrotate(load.image(origin),
                        angle = switch(
                          as.character(orientation),
                          "6" =
                            90,
                          "3" =
                            180,
                          "8" =
                            270
                        )),
               target)
  }
}
