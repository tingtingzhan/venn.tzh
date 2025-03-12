
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
  # .. is 'gList'
  # .. does not have ?base::dim
  return(c(
    sprintf(fmt = 'Venn diagram is created using <u>**`R`**</u> package <u>**`VennDiagram`**</u>.'),
    '',
    '```{r}', 
    sprintf(fmt = 'venn.tzh::plot.venn(%s)', xnm),
    '```'
  ))
}

