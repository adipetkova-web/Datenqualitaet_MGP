#install.packages(c("magick", "stringr"))  # once
library(magick)
library(stringr)

dir <- "c:\\Users\\otto\\tmp\\anim_mgp\\"
files <- stringr::str_sort(list.files(dir, pattern = "(?i)\\.jpe?g$", full.names = TRUE),
                           numeric = TRUE)        # natural order: 1,2,...,51

imgs <- image_read(files)                         # read all JPGs
imgs <- image_scale(imgs, "1080")                 # respect EXIF rotation
# Optional resize (set a width, keeps aspect). Comment out if not needed:
# imgs <- image_scale(imgs, "1080")

anim <- image_animate(imgs, fps = 5, loop = 0)   # loop=0 → forever
image_write(anim, "c:\\Users\\otto\\tmp\\anim_mgp\\animation.gif")
