

#' @title Sprintf.venn
#' 
#' @param model a [venn] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [Sprintf.venn] returns a \link[base]{character} scalar.
#' 
#' @export
Sprintf.venn <- function(model, ...) {
  sprintf(fmt = 'Venn diagram of %s are created using <u>**`R`**</u> package <u>**`VennDiagram`**</u>.', paste0('`', category_venn(model), '`', collapse = ', '))
}