#' Names and Classes of Selected R Datasets
#'
#' @return data frame
#' @export
#' @importFrom stringr str_remove
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
datanames <- function() {
  # Find probable data names in package `datasets`.
  dataname <-
    stringr::str_remove(library(help = "datasets")$info[[2]], " .*$")
  dataname <- unique(c("faithful", "mtcars",
    dataname[dataname != "" & dataname != "datasets-package"]))
  classes <- sapply(dataname, function(x) {
    tryCatch(class(get(x))[1],
             error = function(e) "none")
  })
  tibble::as_tibble(data.frame(name = dataname, class = classes)) |>
    dplyr::filter(classes %in% c("data.frame", "matrix"))
}