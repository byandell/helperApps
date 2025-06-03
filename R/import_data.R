#' Import data from files
#'
#' This function imports data from files specified in the `import.csv` file
#' located in the package's `data` directory. The function reads files with
#' extensions `.csv`, `.rds`, or `.xlsx` and returns a list of data frames.
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @importFrom tools file_ext
#' @export
import_data <- function() {
  import <- read.csv(system.file("data/import.csv", package = "RPAshiny"))
  out <- list()
  for (i in 1:nrow(import)) {
    ext <- tools::file_ext(import$filename[i])
    filename <- system.file(import$filename[i], package = "RPAshiny")
    imported <- switch(ext,
           csv  = readr::read_csv(filename),
           xlsx = readxl::read_excel(filename),
           rds  = readRDS(filename),
           shp  = sf::st_read(filename))
    # Convert RDS or SHP data to CRS 4326.
    if(ext %in% c("rds", "shp")) {
      imported <- sf::st_transform(imported, crs = 4326)
    }
    out[[import$object[i]]] <- imported
  }
  out
}
