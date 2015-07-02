#' Clean Rendering of Embedded RMarkdown Documents
#'
#' \code{clean_render} is a utility function for knitting embedded R Markdown documents. This
#' circumvents the write access error that typically occurs if you call `rmarkdown::render()`
#' from within an Rmarkdown document.
#'
#' @param toRender A string pertaining to the Rmd file to be rendered (no extension)
#' @param outDir The directory rendered reports should be saved to
#' @return NULL

clean_render <- function(toRender = NULL, outDir = NULL, extension = ".pdf") {
  file <- paste0(toRender, ".Rmd")
  if (is.null(outDir)) {
    output <- paste0(toRender, extension)
  } else output <- paste0(outDir, "/", toRender, extension)
  cat('
      rmarkdown::render("', file, '", output_file = "', output, '")
      ',
      sep = "", file = "tempRender.R")
  devtools::clean_source('tempRender.R', quiet = TRUE)
  file.remove("tempRender.R")
  return("Render complete.")
}
