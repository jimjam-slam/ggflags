library(png)

# mogrify -background none -density 625 -resize 400x400 -quality 75 -format png *.svg

lf <- list.files(path="png", pattern="\\.png$", full.names = TRUE)


.flaglist <- lapply(lf, readPNG)
names(.flaglist) <- tolower(gsub("^png/|\\.png", "", lf))

save(.flaglist, file="lflags.rda")

