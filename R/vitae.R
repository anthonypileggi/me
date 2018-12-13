
#' Create a CV using the `vitae` package
#' @param template which template should we use?
#' @param outdir full path to output directory
#' @param sections ordered vector of CV sections
#' @export
vitae_cv <- function(...,
                     template = "awesomecv",
                     outdir = file.path(getwd(), "my_cv"),
                     sections = c("education", "experience")) {
  
  # where am i now?
  start_dir <- getwd()
  
  # create new cv directory and go there
  outdir <- normalizePath(outdir)
  if (dir.exists(outdir)) 
    stop(paste("A directory already exists at", outdir)) 
  dir.create(outdir)
  setwd(outdir)
  
  # load template; copy all neccessary files
  templates <- function(loc, tex = paste0(loc, "template.tex")) {
    system.file("rmarkdown", "templates", loc, "resources", tex, package = "vitae")
  }
  this_template <- switch(template,
    hyndman = templates("hyndman"),
    twentyseconds = templates("twentyseconds"),
    moderncv = templates("moderncv", "moderncv.tex"),
    awesomecv = templates("awesomecv", "awesome-cv.tex")
  )
  if (is.null(this_template))
    stop("You must choose from available templates in `vitae`!")
  vitae:::copy_supporting_files(template)
  
  # write 'cv.Rmd' file
  outfile <- "cv.Rmd"
  writeLines(vitae_yaml_header(template), outfile)
  if ("education" %in% sections)
    write("# Education\n```{r}\nvitae_education()\n```\n", file = outfile, append = TRUE)
  if ("experience" %in% sections)
    write("# Experience\n```{r}\nvitae_experience()\n```\n", file = outfile, append = TRUE)

  # render as .pdf
  rmarkdown::render(outfile)
  
  # reset working directory
  setwd(start_dir)
  
  return(outfile)
}


## cv-build helpers ----------

#' Build yaml header for the 'vitae' pkg
#' @export
vitae_yaml_header <- function(template) {
  glue::glue_data(
    me::read_config(),
'
---
name: "{name}"
position: "{position}"
address: "{address$city}, {address$state}"
phone: "{contact$phone}"
profilepic: "{system.file("me.jpg", package = "me")}"
www: "{contact$homepage}"
email: "{contact$email}"
twitter: "{contact$twitter}"
github: "{contact$github}"
linkedin: "{contact$linkedin}"
date: "`r format(Sys.time(), \'%B %Y\')`"
aboutme: "{summary}"
output: vitae::{template}
---

```{{r setup, include=FALSE}}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(vitae)
library(me)
```

   ',
    .transformer = null_transformer()
    )
}

#' Build Education section
#' @export
vitae_education <- function() {
  vitae::detailed_entries(
    data = me::education(),
    what = degree,
    when = glue::glue("{format(start_date, '%Y')} - {format(end_date, '%Y')}"),
    with = school,
    where = glue::glue("{city}, {state}"),
    why = glue::glue("Concentration: {concentration}")
  )
}

#' Build Experience
#' @export
vitae_experience <- function() {
  vitae::detailed_entries(
    data = me::experience(),
    what = position,
    when = glue::glue("{format(start_date, '%Y')} - {format(end_date, '%Y')}"),
    with = company,
    where = glue::glue("{city}, {state}"),
    why = unlist(description)
  )
}


## glue helpers ------------

#' for dealing w/ NULL values 
null_transformer <- function(str = "") {
  function(text, envir) {
    out <- glue::identity_transformer(text, envir)
    if (is.null(out))
      return(str)
    out
  }
}
