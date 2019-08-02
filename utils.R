# Scalar character concatenate infix
`%+%` <- function(lhs, rhs) {
  if(length(lhs) != 1 || length(rhs)  != 1 || !is.atomic(lhs) || !is.atomic(rhs)) stop('Both sides must be scalars')
  paste0(lhs,rhs)
}

# Remove rownames from data.frame and return data.frame
unrowname <- function(df) {
  if(!any(class(df) == 'data.frame')) stop('df argument must be of class data.frame')
  rownames(df) <- NULL
  return(df)
}

# lapply then do.call rbind with returned list
lapplyDf <- function(lx,lf) {
  unrowname(do.call('rbind', lapply(lx,lf)))
}

#  Convert data.frame from long to wide with stats::reshape
widen <- function(df, keepCols, newCols, newColsVal) {
  out <- reshape(data =df, idvar = keepCols, timevar = newCols, v.names = newColsVal, direction = 'wide')
  rownames(out) <- NULL
  attr(out,'reshapeWide') <- NULL
  return(out)
}