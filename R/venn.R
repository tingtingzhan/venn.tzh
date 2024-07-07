


#' @title Venn Diagram via \CRANpkg{VennDiagram}
#' 
#' @description
#' 
#' For students, encourage them to use \link[VennDiagram]{venn.diagram}.
#' 
#' @param object ..
#' 
#' @param ind \link[base]{logical} scalar (see \link[VennDiagram]{draw.pairwise.venn}),
#' whether the function is to automatically draw the diagram before returning the \link[grid]{gList} object or not
#' 
#' @param lty \link[base]{character} scalar (see \link[VennDiagram]{draw.pairwise.venn}),
#' line dash pattern of the circles' circumferences.
#' Default `'blank'` disables circle circumference.
#' 
#' @param fill \link[base]{character} \link[base]{vector} (see \link[VennDiagram]{draw.pairwise.venn}), colors of the circles' areas.  
#' Default \link[grDevices]{rainbow}
#' 
#' @param alpha \link[base]{numeric} scalar (see \link[VennDiagram]{draw.pairwise.venn}), transparency of the circles' areas
#' 
#' @param cex \link[base]{numeric} scalar (see \link[VennDiagram]{draw.pairwise.venn})
#' 
#' @param cat.col \link[base]{character} \link[base]{vector} (see \link[VennDiagram]{draw.pairwise.venn}), colors of the category names.
#' Default value being the same as `fill`
#' 
#' @param cat.cex \link[base]{numeric} scalar (see \link[VennDiagram]{draw.pairwise.venn}), 
#' size of the category names
#' 
#' @param cat.fontface \link[base]{character} scalar (see \link[VennDiagram]{draw.pairwise.venn}),
#' font face of the category names.  Default `'bold'`
#' 
#' @param print.mode \link[base]{character} scalar or \link[base]{vector} (see \link[VennDiagram]{draw.pairwise.venn}),
#' Default `c('percent', 'raw')`
#' 
#' @param cat.default.pos ..
#' 
#' @param ... additional parameters of \link[VennDiagram]{draw.pairwise.venn}, etc.
#' 
#' @details
#' Workhorse of function [venn] is one of 
#' \link[VennDiagram]{draw.single.venn}, 
#' \link[VennDiagram]{draw.pairwise.venn}, 
#' \link[VennDiagram]{draw.triple.venn}, 
#' \link[VennDiagram]{draw.quad.venn}, 
#' or
#' \link[VennDiagram]{draw.quintuple.venn}.
#' 
#' Labels of subsets with zero counts are suppressed and printed as `'-'`).
#' 
#' @returns 
#' Function [venn] returns a \link[grid]{gList} object. 
#' 
#' @seealso 
#' 
#' \link[VennDiagram]{venn.diagram} does handle \link[base]{list} input, but not as elegantly as 
#' function [venn.matrix].
#'
#' @examples 
#' plot(venn(list(a = rep(TRUE, times = 10L), b = sample(c(FALSE, TRUE), size = 10L, replace = TRUE))))
#' plot(venn(list(
#'   A = state.name[1:30], 
#'   B = state.name[20:45], 
#'   C = state.name[c(15:40, 46:50)])))
#' 
#' plot(venn(list(
#'   A = state.name[1:20], 
#'   B = state.name[2:21], 
#'   C = state.name[3:22],
#'   D = state.name[4:23])))
#'
#' @importFrom grDevices rainbow
#' @importFrom VennDiagram draw.single.venn draw.pairwise.venn draw.triple.venn draw.quad.venn draw.quintuple.venn
#' @importFrom stats setNames
#' @name venn
#' @export
venn <- function(object, ...) {
  if (!length(object)) return(invisible())
  UseMethod('venn')
}

#' @rdname venn
#' @export venn.list
#' @export
venn.list <- function(object, ...) {
  cls <- lapply(object, FUN = class)
  if (!all(duplicated.default(cls)[-1L])) stop('all elements of the list must be the same class')
  cls <- cls[[1L]]
  obj <- if (cls == 'logical') {
    do.call(cbind, args = object)
  } else {
    if (anyNA(object, recursive = TRUE)) stop('each element of input list must not contain NA')
    if (!length(nm <- names(object)) || !all(nzchar(nm))) stop('input list must be fully named')
    do.call(cbind, args = lapply(object, FUN = `%in%`, x = unique.default(unlist(object, use.names = FALSE))))
  }
  venn.matrix(obj, ...)
}

#' @rdname venn
#' @importFrom stats complete.cases
#' @export venn.data.frame
#' @export
venn.data.frame <- function(object, ...) {
  obj <- object[complete.cases(object), ]
  venn.list(as.list.data.frame(obj), ...)
}

#' @rdname venn
#' @importFrom utils combn
#' @export venn.matrix
#' @export
venn.matrix <- function(
    object,
    ind = FALSE, 
    lty = 'blank',
    fill = rainbow(n = n_cat), 
    alpha = .25,
    cex = 1,
    cat.col = fill,
    cat.fontface = 'bold',
    cat.cex = 1.2,
    print.mode = c('percent', 'raw'),
    cat.default.pos = 'outer',
    ...
) {
  if (anyNA(object)) stop('do not allow missing in \'matrix\' input for Venn diagram')
  # object <- force_bool(object, else_return = stop('cannot convert to binary matrix')) # dont want to include my [force_bool]
  if (typeof(object) != 'logical') stop('input must be binary/logical matrix')
  if (!length(object)) return(invisible())
  
  rid <- (rowSums(object) == 0) # all-FALSE rows
  if (all(rid)) return(invisible())
  if (any(rid)) {
    message('Remove ', sum(rid), ' all-FALSE row(s)')
    # must perform *before* removing all-TRUE columns
    object <- object[!rid, , drop = FALSE]
  }
  
  if (dim(object)[2L] > 1L) {
    #cid <- (colSums(object) %in% c(0, dim(object)[1L])) # all-FALSE or all-TRUE columns
    cid <- (colSums(object) == 0) # all-FALSE columns
    if (all(cid)) return(invisible())
    if (any(cid)) {
      #message('Remove all-TRUE or all-FALSE columns ', paste(sQuote(dimnames(object)[[2L]][cid]), collapse = ', '))
      message('Remove all-FALSE columns ', paste(sQuote(dimnames(object)[[2L]][cid]), collapse = ', '))
      object <- object[, !cid, drop = FALSE]
    }
  }
  
  dimy <- dim(object)
  n_cat <- dimy[2L]
  
  area <- .colSums(object, m = dimy[1L], n = n_cat, na.rm = FALSE)
  
  ag1 <- c(
    list(category = paste0(dimnames(object)[[2L]], '\n(', area, ')')),
    setNames(as.list.default(area), nm = if (n_cat == 1L) 'area' else paste0('area', seq_len(n_cat)))
  )
  ag2 <- if (n_cat == 1L) NULL else if (n_cat == 2L) {
    list(cross.area = sum(rowSums(object) == dimy[2L])) 
  } else {
    cbs <- lapply(2:n_cat, FUN = combn, x = n_cat, simplify = FALSE) # all [c]om[b]ination indexe[s]
    fcbs <- do.call(c, args = cbs) # make '[f]lat'
    names(fcbs) <- paste0('n', vapply(fcbs, FUN = paste, collapse = '', FUN.VALUE = ''))
    lapply(fcbs, FUN = function(i) sum(rowSums(object[, i, drop = FALSE]) == length(i)))
  }
  
  ret <- do.call(what = switch(
    n_cat, 
    '1' = draw.single.venn, '2' = draw.pairwise.venn, '3' = draw.triple.venn,
    '4' = draw.quad.venn, '5' = draw.quintuple.venn,
    stop('cannot draw Venn diagram for 6 or more categories')
  ), args = c(ag1, ag2, list(
    cat.default.pos	= cat.default.pos,
    print.mode = print.mode,
    ind = ind, lty = lty, 
    fill = fill, 
    alpha = alpha, 
    cex = cex, 
    cat.col = fill, 
    cat.fontface = cat.fontface,
    cat.cex = cat.cex, 
    ...
  )))
  
  class(ret) <- c('venn', class(ret))
  return(ret)
  
}


#' @title zero_venn
#' 
#' @description
#' Simplify zero labels of a [venn] plot.
#' 
#' @param x a [venn] object
#' 
#' @param zero \link[base]{character} scalar, label of zero counts. Default `'-'`
#' 
#' @export
zero_venn <- function(x, zero = '-') {
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


#' @importFrom grid grid.newpage grid.draw
#' @export
plot.venn <- function(x, ...) {
  grid.newpage()
  grid.draw(zero_venn(x))
}

