


# to retrieve 'category' from 'venn'

category_venn <- function(x) {
  
  cls_ <- vapply(x, FUN = function(i) class(i)[1L], FUN.VALUE = '')
  
  text_ <- x[cls_ == 'text']
  text_col_ <- vapply(text_, FUN = function(i) i$gp$col, FUN.VALUE = '')
  # I always set 'category' in a color palette
  
  cat0 <- vapply(text_[text_col_ != 'black'], FUN = function(i) i$label, FUN.VALUE = '')
  
  vapply(strsplit(cat0, split = '\n'), FUN = `[[`, 1L, FUN.VALUE = '')
  
}



