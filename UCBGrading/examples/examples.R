library(tidyverse)
library(UCBGrading)

n_students <- 20
grades <- data.frame(
  id=1:n_students,
  homework_1=1:n_students,
  homework_1_max=100,
  homework_2=(1:n_students) * 100,
  homework_2_max=50,
  homework_3=150 * runif(n_students),
  homework_3_max=150,
  quiz_1=runif(n_students),
  quiz_2=runif(n_students),
  quiz_3=runif(n_students),
  quiz_4=runif(n_students)
)


DivideRow <- function(row) {
  return(row / 100)
}

Identity <- function(row) {
  return(row)
}

SumRow <- function(row) {
  return(sum(row))
}

i <- 1
new_cols <- cols


ApplyRowOperation <- function(df, cols, new_cols, RowOperation) {
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




cols <- c("homework_1", "homework_2")
ApplyRowOperation(grades, cols, paste0(cols, "_new"), DivideRow)

cols <- c("homework_1", "homework_2")
ApplyRowOperation(grades, cols, "sum", SumRow)

v <- apply(grades[cols], SumRow, MARGIN=1)


v <- apply(grades[cols], Identity, MARGIN=1)
class(v)
is.list(v)
is.array(v)
dim(v)
is.matrix(v)
is.numeric(v)
data.frame(v)
