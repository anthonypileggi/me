
#' Read the config.toml file containing info about me
#' @export
read_config <- function(file = system.file("config.toml", package = "me")) {
  configr::read.config(file)
}

#' @export
experience <- function(x = read_config()) {
 # purrr::map_df(x$experience, data.frame)
  purrr::map_df(x$experience, tibble::as_tibble) %>% 
    mutate(
      description = purrr::map_chr(description, ~ifelse(is.null(.x), NA_character_, unlist(.x)))
      ) %>% 
    nest(description, .key = "description")
}

#' @export
education <- function(x = read_config()) {
  purrr::map_df(x$education, tibble::as_tibble)
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
