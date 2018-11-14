# hugo-resume

#' Create a Hugo-Resume website
#' @param dir directory to place the new hugo-resume website
#' @param base_url base url for hosted website
#' @importFrom magrittr "%>%"
#' @export
hugo_resume <- function(dir = file.path(getwd(), "my-hugo-resume"),
                        base_url = "https://www.mywebsite.com/") {
  
  # prepare my data
  me <- read_config()
  my_data <- c(me[c("name", "position", "summary")], me$address, me$contact, base_url = base_url)
  my_data$first_name <- head(stringr::str_split(me$name, " ")[[1]], 1)
  my_data$last_name <- tail(stringr::str_split(me$name, " ")[[1]], 1)
  
  hugo_resume_prepare_directory(dir)

  # create summary '_index.md'
  my_summary <- glue::glue_data(my_data, '
---
title: "Home"
date: {Sys.Date()}
sitemap:
  priority : 1.0

outputs:
- html
- rss
- json
---
<p>{summary}</p>    
    ')
  writeLines(my_summary, file.path(dir, "content/_index.md"))
    
  # personalize 'config.toml'
  out <- readLines(system.file("hugo-resume/config_template.toml", package = "me"))
  out <- purrr::map_chr(out, ~glue::glue_data(my_data, .x))
  writeLines(out, file.path(dir, "config.toml"))
  unlink(file.path(dir, "config_template.toml"))
  
  # customize 'themes/hugo-resume/layouts/partials/nav.html'
  #   - insert custom external links (after line 45)
  nav_links <- websites(me)
  if (file.exists(file.path(dir, "static/cv.pdf")))
    nav_links <- rbind(nav_links, tibble::tibble(name = "Download CV", url = "cv.pdf"))
  if (nrow(nav_links) > 0) {
    nav_file <- file.path(dir, "themes/hugo-resume/layouts/partials/nav.html")
    nav <- readLines(nav_file)
    new_links <- glue::glue_data(nav_links, '
      <li class="nav-item">
      <a class="nav-link js-scroll-trigger" href="{url}">{name}</a>
      </li>
      ')
    nav <- c(nav[1:45], new_links, nav[46:length(nav)])
    writeLines(nav, nav_file)
  }

  
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
    dplyr::mutate(skills = purrr::map(data, ~dplyr::select(.x, name = item, link = url))) %>%
    dplyr::select(grouping = category, skills) %>% 
    jsonlite::toJSON(pretty = TRUE) %>%
    writeLines(file.path(dir, "data/skills.json"))
}



#' Prepare an new hugo-resume directory
hugo_resume_prepare_directory <- function(dir) {
  
  # remove any existing directory
  if (dir.exists(dir))
    unlink(dir, recursive = TRUE)
  
  # setup directory structure
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
  
  # move 'cv' to 'static' folder
  file.copy(system.file("cv.pdf", package = "me"), file.path(dir, "static/cv.pdf"))
}


hugo_resume_summary <- function() {
  
}
