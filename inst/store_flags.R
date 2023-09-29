library(rsvg)
library(grImport2)

download.file(
  "https://github.com/HatScripts/circle-flags/archive/refs/heads/gh-pages.zip",
  "circle-flags.zip")
unzip("circle-flags.zip", exdir = "circle-flags")
dir.create("svg", showWarnings = FALSE)
flag_svgs <- list.files("circle-flags/circle-flags-gh-pages/flags",
  pattern = glob2rx("*.svg"), full.names = TRUE)
file.copy(flag_svgs, "svg")


lf <- list.files(path = "svg", pattern = glob2rx("*.svg"), full.names = FALSE)

dir.create("cairo", showWarnings = FALSE)
for (srcfile in lf)
{
  # check if file starts with `<svg` (to weed out symbolic links)
  file_ok <- length(grep("^<svg", readLines(file.path("svg", srcfile)))) > 0
  if (file_ok) {
    print(srcfile)
    rsvg_svg(file.path("svg", srcfile), file.path("cairo", srcfile))
  } else {
    message("Skipping symbolic link ", srcfile)
  }
}

lf <- list.files(path = "cairo", pattern = glob2rx("*.svg"), full.names = TRUE)

.flaglist <- lapply(lf, readPicture)
names(.flaglist) <- tolower(gsub("^cairo/|\\.svg", "", lf))

save(.flaglist, file = "lflags.rda")

# cleanup
unlink("circle-flags.zip")
unlink("circle-flags", recursive = TRUE)
unlink("svg", recursive = TRUE)
unlink("cairo", recursive = TRUE)
