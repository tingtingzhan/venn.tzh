

#' @title Simplify Zero-Labels of [venn] Plot
#' 
#' @description
#' Simplify zero-labels of a [venn] plot.
#' 
#' @param x a [venn] object
#' 
#' @param zero \link[base]{character} scalar, label of zero counts. 
#' Default `''`
#' 
#' @details
#' Labels of subsets with zero counts are suppressed.
#' 
#' @returns
#' Function [zero_venn] returns a [venn] object.
#' 
#' @export
zero_venn <- function(x, zero = '') {
  if (!is.character(zero) || length(zero) != 1L || is.na(zero)) stop('illegal zero label')
  cls_ <- vapply(x, FUN = function(i) class(i)[1L], FUN.VALUE = '')
  for (i in which(cls_ == 'text')) {
    if (x[[i]]$label %in% c(
      '0%', # print.mode = 'percent'
      '0', # print.mode = 'raw'
      '0%\n(0)', # print.mode = c('percent', 'raw')
      '0\n(0%)' # print.mode = c('raw', 'percent')
    )) x[[i]]$label <- zero
  }
  return(x)
}
