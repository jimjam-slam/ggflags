# get-flags.r: downloads hatscripts/circle-flags and saves its flag svgs as a
# list with names corresponding to their (extensionless) filenames

cf_release <- "2.7.0"

download.file(
  paste0(
    "https://github.com/HatScripts/circle-flags/archive/refs/tags/v",
    cf_release, ".zip"),
  "circle-flags.zip")
unzip("circle-flags.zip", exdir = "circle-flags")

flag_paths <- list.files(
  file.path("circle-flags", paste0("circle-flags-", cf_release), "flags"),
  pattern = glob2rx("*.svg"), full.names = TRUE)

# add each flag's svg string to the list
lflags <- list()
for (path in flag_paths) {
  
  svg_string <- paste0(
    readLines(path, skipNul = TRUE, warn = FALSE),
    collapse = "\n")

  # skip if the flag is in fact a symbolic link
  if (!grepl("^<svg", svg_string)) {
    next
  }

  flag_code <- sub(".svg", "", x = basename(path))
  lflags[[tolower(flag_code)]] <- svg_string

}

# write the list out
save(lflags, file = "data/lflags.rda", compress = "xz")

# clean up
unlink("circle-flags.zip")
unlink("circle-flags", recursive = TRUE)
