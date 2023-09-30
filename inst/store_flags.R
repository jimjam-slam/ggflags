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

# note - looking to restrict the use of flags to the existing 2-letter codes
# until i can follow up a few things on licensing
lf <- lf[which(nchar(lf) == 6)]

process_flag <- function(input_file) {

  # get svg string
  svg_string <- paste(readLines(file.path("svg", input_file)), collapse = "\n")
  
  # bail if it doesn't look like a valid svg
  # (likely because a couple of them are symbolic links on github.com)
  stopifnot("Not a valid SVG" = length(grep("^<svg", svg_string)) > 0)

  # swap circular <mask> for a <clipPath>
  # (assu)
  svg_string <- sub(
    "<mask id=\"a\"><circle cx=\"256\" cy=\"256\" r=\"256\" fill=\"#fff\"/></mask>",
    "<clipPath id=\"a\"><circle cx=\"256\" cy=\"256\" r=\"256\"/></clipPath>",
    svg_string,
    fixed = TRUE)
  svg_string <- sub(
    pattern = "<g mask=\"url(#a)",
    replacement = "<g clip-path=\"url(#a)",
    svg_string,
    fixed = TRUE)

  # convert to raw and write out
  rsvg_svg(charToRaw(svg_string), file.path("cairo", input_file))
}

dir.create("cairo", showWarnings = FALSE)

# process each flag
for (srcfile in lf)
{
  print(srcfile)
  try({ process_flag(srcfile) })
}

# finally, get the processed flags, import with grImport2 and save out
lf <- list.files(path = "cairo", pattern = glob2rx("*.svg"), full.names = TRUE)

.flaglist <- lapply(lf, readPicture)
names(.flaglist) <- tolower(gsub("^cairo/|\\.svg", "", lf))

save(.flaglist, file = "lflags.rda")

# cleanup
unlink("circle-flags.zip")
unlink("circle-flags", recursive = TRUE)
unlink("svg", recursive = TRUE)
unlink("cairo", recursive = TRUE)
