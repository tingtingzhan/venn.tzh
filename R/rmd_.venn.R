
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
  # \link[venn.tzh]{venn} 
  # .. is 'gList', but I do not want to hide it (as for 'gDesc')
  # .. does not have ?base::dim
  return(c(
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'grid::grid.draw(venn.tzh::zero_venn(%s))', xnm), # my [plot.venn]
    '```'
  ))
}

