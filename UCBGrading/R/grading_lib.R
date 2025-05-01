library(tidyverse)

# Safely construct a new dataframe applying RowOperation to
# each row of df[cols], returning a new data frame with columns new_cols
#' @export
ApplyRowOperation <- function(df, cols, new_cols, RowOperation) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(cols))
  stopifnot(is.character(new_cols))
  stopifnot(is.function(RowOperation))

  ncols <- length(cols)
  stopifnot(all(cols %in% names(df)))
  
  result <- apply(df[cols], RowOperation, MARGIN=1)
  
  if (is.list(result)) {
    stop("RowOperation produced a ragged array, which is not permitted.")
  }
  if (is.array(result)) {
    result_dim <- dim(result)
    if (length(result_dim) != 2) {
      stop("RowOperation produced an array that is not a matrix, which is not permitted.")
    }
    if (result_dim[1] != length(new_cols)) {
      stop("RowOperation produced a different dimension than new_cols")
    }
    if (result_dim[2] != nrow(df)) {
      stop("RowOperation produced a different number of rows than df")
    }
    df_new <- data.frame(t(result))
  } else if (is.numeric(result)) {
    if (length(result) != nrow(df)) {
      stop("RowOperation produced a different number of rows than df")
    }
    df_new <- data.frame(result)
  }
  names(df_new) <- new_cols

  return(df_new)
}


# Allow NULL specifications of the columns to act on to select all the dataframe columns
GetDefaultCols <- function(df, cols) {
  stopifnot(is.data.frame(df))
  if (is.null(cols)) {
    return(names(df))
  } else {
    return(cols)
  }
}

#' @export
ComputeWeightedMean <- function(df, cols=NULL, new_col="weighted_mean", weights=NULL) {
  cols <- GetDefaultCols(df, cols)
  if (is.null(weights)) {
    weights <- rep(1, length(cols))
    weights <- weights / sum(weights)
  }
  if (abs(sum(weights) - 1) > 1e-6 ) {
    warning("The weights do not sum to one; normalizing")
    weights <- weights / sum(weights)
  }
  stopifnot(length(cols) == length(weights))
 
  RowOperation <- function(grades) {
    sum(grades * weights)
  }

  return(ApplyRowOperation(
    df=df, cols=cols, new_cols=new_col, RowOperation=RowOperation))
}


#' @export
DropLowest <- function(df, num_drops, cols=NULL, new_prefix="drop_") {
  cols <- GetDefaultCols(df, cols)
  grade_len <- length(cols)
  keep_len <- grade_len - num_drops
  stopifnot(keep_len > 0)
  new_cols <- paste0(new_prefix, 1:keep_len)

  RowOperation <- function(grades) {
    grades <- as.numeric(grades)
    return(sort(grades, decreasing=TRUE)[1:keep_len])
  }
 
  return(ApplyRowOperation(
    df=df, cols=cols, new_cols=new_cols,
    RowOperation=RowOperation))
}

#' @export
NormalizeColsByNumber <- function(df, max_score, cols=NULL, suffix="_norm") {
  cols <- GetDefaultCols(df, cols)
  if (length(max_score) > 1) {
    stopifnot(length(max_score) == length(cols))
  }
  stopifnot(is.numeric(max_score))
  stopifnot(all(max_score > 0))

  RowOperation <- function(grades) {
    grades <- as.numeric(grades)
    return(grades / max_score)
  }
 
  return(ApplyRowOperation(
    df=df, cols=cols, new_cols=paste0(cols, suffix),
    RowOperation=RowOperation))
}


# It does not make sense to have a default for cols.
#' @export
NormalizeColsByCols <- function(df, max_score_cols, cols, suffix="_norm") {
  stopifnot(is.data.frame(df))
  num_cols <- length(cols)
  num_norm_cols <- length(max_score_cols)
  if (num_norm_cols > 1) {
    stopifnot(num_norm_cols == num_cols)
  }
  stopifnot(all(max_score_cols %in% names(df)))

  RowOperation <- function(vec) {
    # The first num_cols are the grades, the rest are the normalizers
    vec <- as.numeric(vec)
    grades <- vec[1:num_cols]
    norm_vals <- vec[(num_cols + 1):(num_cols + num_norm_cols)]

    return(grades / norm_vals)
  }

  return(ApplyRowOperation(
    df=df, cols=c(cols, max_score_cols), new_cols=paste0(cols, suffix),
    RowOperation=RowOperation))
}


#' @export
GetMatchingEntries <- function(svec, pattern) {
  svec[grepl(pattern, svec)]
}

#' @export
GetLetterGrade <- function(score) {
  grade <- case_when(
    score >= 0.99 ~ "A+",
    score >= 0.92 ~ "A",
    score >= 0.90 ~ "A-",
    score >= 0.89 ~ "B+",
    score >= 0.82 ~ "B",
    score >= 0.80 ~ "B-",
    score >= 0.79 ~ "C+",
    score >= 0.72 ~ "C",
    score >= 0.70 ~ "C-",
    score >= 0.69 ~ "D+",
    score >= 0.62 ~ "D",
    score >= 0.60 ~ "D-",
    TRUE ~ "F"
  )
  return(grade)
}