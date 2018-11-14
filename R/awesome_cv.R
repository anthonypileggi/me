# awesome-cv

#' Create an Awesome-CV
#' @param dir directory to place the new Awesome-CV
#' @param color awesome-cv color
awesome_cv <- function(dir = file.path(getwd(), "my-awesome-cv"),
                       color = "emerald",
                       sections = c("education", "experience")) {
  
  # prepare my data
  me <- read_config()
  my_data <- c(me[c("name", "position")], me$address, me$contact, color = color)
  
  my_data$first_name <- head(stringr::str_split(me$name, " ")[[1]], 1)
  my_data$last_name <- tail(stringr::str_split(me$name, " ")[[1]], 1)
  
  # prepare 'my-awesome-cv' directory
  if (dir.exists(dir))
    unlink(dir, recursive = TRUE)
  dir.create(dir)
  dir.create(file.path(dir, "cv-sections"))
  file.copy(system.file("awesome-cv/awesome-cv.cls", package = "me"), dir)
  file.copy(system.file("awesome-cv/fontawesome.sty", package = "me"), dir)
  file.copy(system.file("awesome-cv/resume_cv_template.tex", package = "me"), dir)
  #file.copy(system.file("awesome-cv/cv-sections", package = "me"), dir, recursive = TRUE)
  file.copy(system.file("awesome-cv/fonts", package = "me"), dir, recursive = TRUE)
  
  # copy .tex file and insert my values
  out <- readLines(system.file("awesome-cv/resume_cv_template.tex", package = "me"))
  out <- purrr::map_chr(out, ~glue::glue_data(my_data, .x, .open = "$$", .close = "$$"))
  writeLines(out, file.path(dir, "resume_cv.tex"))
  unlink(file.path(dir, "resume_cv_template.tex"))
  
  # keep specified sections only
  me <- me[sections]
  
  # write each cv-section
  my_awesome_education(me, dir)
  my_awesome_skills(me, dir)
  my_awesome_experience(me, dir)
  my_awesome_honors(me, dir)
  my_awesome_publications(me, dir)
  my_awesome_presentations(me, dir)
  my_awesome_hobbies(me, dir)
  
  # compile as pdf
  
}


#' education.tex
my_awesome_education <- function(x, dir) {
  
  out_file <- file.path(dir, "cv-sections/education.tex")
  
  # create an empty .tex file if no education is present
  if (length(x$education) == 0) {
    writeLines("", out_file)
    return(FALSE)
  }
  
  # convert content to education.tex
  out <- purrr::map_chr(
    x$education, 
    function(edu) {
      # prepare bullet points
      description <- purrr::map_chr(
        c("thesis", "advisor", "concentration"), 
        ~switch(is.null(edu[[.x]]) + 1, paste0('\\item{', .x, ': \\emph{', edu[[.x]], '}}'), '')
        )
      description <- description[description != ""]
      if (length(description) > 0)
        description <- c("{", "\\begin{cvitems}", description, "\\end{cvitems}", "}")
      
      glue::glue_data(edu, "
\\cventry
{..degree..} % Degree
{..school..} % Institution
{..city.., ..state..} % Location
{..format(end_date, '%Y')..} % Date(s)
..paste(description, collapse = '\n')..
        ", 
        .open = "..", .close = ".."
        )
    })
  
  out <- c("\\cvsection{Education}", "\\begin{cventries}", out, "\\end{cventries}")
  writeLines(out, out_file)
  return(TRUE)
}


#' experience.tex
my_awesome_experience <- function(x = read_config(), dir) {
  
  out_file <- file.path(dir, "cv-sections/experience.tex")
  
  # create an empty .tex file if no education is present
  if (length(x$experience) == 0) {
    writeLines("", out_file)
    return(FALSE)
  }
  
  out <- purrr::map_chr(
    x$experience,
    function(exp) {
      # prepare bullet points
      description <- purrr::map_chr(
        exp$description, 
        ~switch(is.na(.x$item) + 1, paste0('\\item{', .x$item, '}'), '')
      )
      description <- description[description != ""]
      if (length(description) > 0)
        description <- c("\\begin{cvitems}", description, "\\end{cvitems}")
      description <- c("{", description, "}")
      
      entry <- glue::glue_data(exp, "
\\cventry
{..position..} % Job title
{..company..} % Organization
{..city.., ..state..} % Location
{..format(start_date, '%Y').. - ..format(end_date, '%Y')..} % Date(s)
    ", 
        .open = "..", .close = ".."
      )
      paste(c(entry, description, ""), collapse = "\n")
    }
  )
  
  out <- c("\\cvsection{Experience}", "\\begin{cventries}", out, "\\end{cventries}")
  writeLines(out, out_file)
  return(TRUE)
}


#' skills.tex
my_awesome_skills <- function(x = read_config(), dir) {
  
  out_file <- file.path(dir, "cv-sections/skills.tex")
  
  # create an empty .tex file if no content is present
  if (length(x$skills) == 0) {
    writeLines("", out_file)
    return(FALSE)
  }
  
  # TODO: build page if content exists
}


#' honors.tex
my_awesome_honors <- function(x = read_config(), dir) {
  
  out_file <- file.path(dir, "cv-sections/honors.tex")
  
  # create an empty .tex file if no content is present
  if (length(x$honors) == 0) {
    writeLines("", out_file)
    return(FALSE)
  }
  
  # TODO: build page if content exists
}


#' publications.tex
my_awesome_publications <- function(x = read_config(), dir) {
  
  out_file <- file.path(dir, "cv-sections/publications.tex")
  
  # create an empty .tex file if no content is present
  if (length(x$publications) == 0) {
    writeLines("", out_file)
    return(FALSE)
  }
  
  # TODO: build page if content exists
}


#' presentations.tex
my_awesome_presentations <- function(x = read_config(), dir) {
  
  out_file <- file.path(dir, "cv-sections/presentations.tex")
  
  # create an empty .tex file if no content is present
  if (length(x$presentations) == 0) {
    writeLines("", out_file)
    return(FALSE)
  }
  
  # TODO: build page if content exists
}


#' hobbies.tex
my_awesome_hobbies <- function(x = read_config(), dir) {
  
  out_file <- file.path(dir, "cv-sections/hobbies.tex")
  
  # create an empty .tex file if no content is present
  if (length(x$hobbies) == 0) {
    writeLines("", out_file)
    return(FALSE)
  }
  
  # TODO: build page if content exists
}
