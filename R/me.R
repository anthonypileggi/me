
#' Read the config.toml file containing info about me
#' @export
read_config <- function(file = system.file("me.toml", package = "me")) {
  configr::read.config(file)
}

#' @export
experience <- function(x = read_config()) {
 # purrr::map_df(x$experience, data.frame)
  purrr::map_df(x$experience, tibble::as_tibble) %>% 
    dplyr::mutate(
      description = purrr::map_chr(description, ~ifelse(is.null(.x), NA_character_, unlist(.x)))
      ) %>% 
    tidyr::nest(description, .key = "description")
}

#' @export
education <- function(x = read_config()) {
  purrr::map_df(x$education, tibble::as_tibble)
}

#' @export
skills <- function(x = read_config()) {
  purrr::map_df(x$skill, tibble::as_tibble)
}

#' @export
websites <- function(x = read_config()) {
  purrr::map_df(x$website, tibble::as_tibble)
}
