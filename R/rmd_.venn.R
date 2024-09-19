
#' @title Create R Markdown Script for [venn]
#' 
#' @description
#' Method dispatch to [venn] for S3 generic `rmd_` (in a different master package).
#' 
#' @param x a [venn] object
#' 
#' @param xnm \link[base]{character} scalar, \link[base]{deparse}d call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @export rmd_.venn
#' @export
rmd_.venn <- function(x, xnm, ...) {
  # \link[venn.tzh]{venn} 
  # .. is 'gList', but I do not want to hide it (as for 'gDesc')
  # .. does not have ?base::dim
  return(c(
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'grid::grid.draw(venn.tzh::zero_venn(%s))', xnm), # my [plot.venn]
    '```'
  ))
}

