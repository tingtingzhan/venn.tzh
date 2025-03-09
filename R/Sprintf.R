

#' @title Sprintf.venn
#' 
#' @param x a [venn] object
#' 
#' @returns
#' Function [Sprintf.venn] returns a \link[base]{character} scalar.
#' 
#' @export
Sprintf.venn <- function(x) {
  sprintf(fmt = 'Venn diagram of %s are created using <u>**`R`**</u> package <u>**`VennDiagram`**</u>.', paste0('`', category_venn(x), '`', collapse = ', '))
}