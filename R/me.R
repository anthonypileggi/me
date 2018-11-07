
#' Read the config.toml file containing info about me
#' @export
read_config <- function(file = system.file("config.toml", package = "me")) {
  configr::read.config(file)
}

#' @export
experience <- function(x = read_config()) {
  purrr::map_df(x$experience, data.frame)
}

#' @export
skills <- function(x = read_config()) {
  purrr::map_df(
    names(x$skills),
    function(skill) {
      tmp <- purrr::map_df(x$skills[[skill]]$skill, tibble::as_tibble)
      tmp$type <- skill
      tmp
    })
}