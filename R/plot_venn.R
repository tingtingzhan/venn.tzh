
#' @title Plot [venn] Object
#' 
#' @param x a [venn] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @return 
#' Function [plot.venn] does not have a returned value.
#' 
#' @importFrom grid grid.newpage grid.draw
#' @export plot.venn
#' @export
plot.venn <- function(x, ...) {
  grid.newpage()
  grid.draw(zero_venn(x))
}




#' @title Print [venn] Object
#' 
#' @param x a [venn] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @return 
#' Function [print.venn] does not have a returned value.
#' 
#' @export print.venn
#' @export
print.venn <- function(x, ...) plot.venn(x, ...)
