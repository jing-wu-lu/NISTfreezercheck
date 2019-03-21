.onLoad <- function(libname, pkgname) {
  dir_pack <- system.file(package = "NISTfreezercheck")
  dir_sep = .Platform$file.sep
  if (!file.exists(paste(dir_pack, "data", "installed.RDS", sep = dir_sep))) {
    cat("\n\nFreezerCheck needs to be set up.\n\n")
    freezercheck_setup()
  } else {
    cat("\n\nFreezerCheck looks ready to go.\n\n")
  }
}
