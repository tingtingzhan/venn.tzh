
#' @title Create R Markdown Script for [venn]
#' 
#' @description
#' ..
#' 
#' @param x a [venn] object
#' 
#' @param xnm \link[base]{character} scalar, \link[base]{deparse}d call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @keywords internal
#' @export
rmd_.venn <- function(x, xnm, ...) {
  # .. is 'gList', but I do not want to hide it (as for 'gDesc')
  # .. does not have ?base::dim
  return(c(
    sprintf(fmt = 'Venn diagram is created using <u>**`R`**</u> package <u>**`VennDiagram`**</u>.'),
    '',
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'grid::grid.draw(venn.tzh::zero_venn(%s))', xnm), # my [plot.venn]
    '```'
  ))
}

