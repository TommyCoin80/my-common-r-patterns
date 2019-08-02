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


# baseSummarize generic method. Groups and aggregates with split and vapply
baseSummarize <- function(x, ...) {
  
  groupAndSummarize <- function(fCols,out,vCol, acb, type, colName) {
    keys <- as.integer(do.call('interaction',fCols))
    out$keys <- as.character(keys)
    out <- unique(out)
    splitDf <- split(vCol, keys)
    nv <- vapply(splitDf, acb,type)
    out[colName] <- nv[out$keys]
    out$keys <- rownames(out) <- NULL
    return(out)
  }
  
  UseMethod('baseSummarize',x)
}

# baseSummarize data frame method for df argument and quoted columns
baseSummarize.data.frame <- function(df,vCol, ..., acb = sum, colName = 'sum', type = double(1)) {
  fCols <- as.list(substitute(...()))
  fColNames <- vapply(fCols,deparse,character(1))
  vCol <- eval(substitute(vCol),df)
  fCols <- lapply(fCols, eval, df)
  out <- df[,fColNames,drop=F]
  out <- groupAndSummarize(fCols,out,vCol, acb, type, colName)
  return(out)
}

#baseSummarize default method for vector columns
baseSummarize.default <- function(vCol, ..., acb = sum, colName = 'sum', fColNames = NULL, type = double(1)) {
  fCols <- list(...)
  if(is.null(fColNames))  fColNames <- paste0('Var', seq_along(fCols))
  out <- do.call('data.frame', c(fCols, stringsAsFactors = F))
  if(!is.null(fColNames)) names(out) <- fColNames
  out <- groupAndSummarize(fCols,out,vCol, acb, type, colName)
  return(out)
}



