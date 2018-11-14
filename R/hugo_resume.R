# hugo-resume

#' Create a Hugo-Resume website
#' @param dir directory to place the new hugo-resume website
#' @param base_url base url for hosted website
#' @export
hugo_resume <- function(dir = file.path(getwd(), "my-hugo-resume"),
                        base_url = "https://www.mywebsite.com/") {
  
  # prepare my data
  me <- read_config()
  my_data <- c(me[c("name", "position", "summary")], me$address, me$contact, base_url = base_url)
  
  my_data$first_name <- head(stringr::str_split(me$name, " ")[[1]], 1)
  my_data$last_name <- tail(stringr::str_split(me$name, " ")[[1]], 1)
  
  # prepare directory
  if (dir.exists(dir))
    unlink(dir, recursive = TRUE)
  dir.create(dir)
  file.copy(system.file("hugo-resume/config.toml", package = "me"), dir)
  file.copy(system.file("hugo-resume/index.Rmd", package = "me"), dir)
  file.copy(system.file("hugo-resume/content", package = "me"), dir, recursive = TRUE)
  file.copy(system.file("hugo-resume/data", package = "me"), dir, recursive = TRUE)
  file.copy(system.file("hugo-resume/static", package = "me"), dir, recursive = TRUE)
  file.copy(system.file("hugo-resume/themes", package = "me"), dir, recursive = TRUE)
  
  # move images to 'static/img' folder
  file.copy(system.file("me.jpg", package = "me"), file.path(dir, "static/img"))
  file.copy(system.file("favicon.png", package = "me"), file.path(dir, "static/img"))
  
  # personalize 'config.toml'
  out <- readLines(system.file("hugo-resume/config_template.toml", package = "me"))
  out <- purrr::map_chr(out, ~glue::glue_data(my_data, .x))
  writeLines(out, file.path(dir, "config.toml"))
  unlink(file.path(dir, "config_template.toml"))
  
  # write json files
  # -- education.json
  education(me) %>%
    dplyr::mutate(
      range = paste(format(start_date, '%Y'), format(end_date, '%Y'), sep = " - ")
    ) %>%
    dplyr::select(school, degree, major, range) %>%
    jsonlite::toJSON(pretty = TRUE) %>%
    writeLines(file.path(dir, "data/education.json"))
  # -- experience.json
  experience(me) %>%
    dplyr::mutate(
      range = paste(format(start_date, '%Y'), format(end_date, '%Y'), sep = " - "),
      summary = purrr::map_chr(description, ~paste(.x$description, collapse = "; "))
    ) %>%
    dplyr::select(role = position, company, summary, range) %>%
    jsonlite::toJSON(pretty = TRUE) %>%
    writeLines(file.path(dir, "data/experience.json"))
  # -- skills.json
  skills(me) %>%
    tidyr::nest(-category) %>%
    dplyr::mutate(skills = purrr::map(data, "item")) %>%
    dplyr::select(grouping = category, skills) %>%
    jsonlite::toJSON(pretty = TRUE) %>%
    writeLines(file.path(dir, "data/skills.json"))
}
