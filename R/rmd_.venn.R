
#' @title Create R Markdown Script for [venn]
#' 
#' @description
#' Method dispatch to [venn] for S3 generic `rmd_` (in a different master package).
#' 
#' @param x a [venn]
#' 
#' @param xnm \link[base]{language} or \link[base]{character} scalar, call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @export rmd_.venn
#' @export
rmd_.venn <- function(x, xnm, ...) {
  return(c(
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'plot(%s)', xnm), # my [plot.venn] 
    '```'
  ))
}