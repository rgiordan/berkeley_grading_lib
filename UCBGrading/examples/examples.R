library(tidyverse)
library(UCBGrading)

n_students <- 20
grades <- data.frame(
  id=1:n_students,
  homework_1=100 * runif(n_students),
  homework_1_max=100,
  homework_2=50 * runif(n_students),
  homework_2_max=50,
  homework_3=150 * runif(n_students),
  homework_3_max=150,
  quiz_1=10 * runif(n_students),
  quiz_2=10 * runif(n_students),
  quiz_3=10 * runif(n_students),
  quiz_4=10 * runif(n_students),
  quiz_max=10
)


##########
# Tests

# A kind of end-to-end with a check for preserving order

quiz_cols <- GetMatchingEntries(names(grades), "quiz_[0-9]+$")
hw_cols <- GetMatchingEntries(names(grades), "homework_[0-9]+$")
hw_max_cols <- GetMatchingEntries(names(grades), "homework_[0-9]+_max$")

comb_df <- bind_cols(
  grades["id"],
  grades %>%
    NormalizeColsByCols("quiz_max", cols=quiz_cols) %>%
    DropLowest(num_drops=1, new_prefix="quiz_dropped_") %>%
    ComputeWeightedMean(new_col="quiz_mean")
  ,
  grades %>%
    NormalizeColsByCols(max_score_cols=hw_max_cols, cols=hw_cols) %>%
    DropLowest(num_drops=1, new_prefix="hw_dropped_") %>%
    ComputeWeightedMean(new_col="hw_mean")
)

stopifnot(all(comb_df$id == grades$id))


# More tests

AssertValuesEqual <- function(df1, df2, tol=1e-9) {
  err <- mean(abs(as.matrix(df1) - as.matrix(df2)))
  if (err > tol) {
    stop(sprintf("Error > Tol: %f > %f", err, tol))
  }
}

# Check normalizing by a single column or number

CheckQuizNorm <- function(df) {
  for (quiz_num in 1:4) {
    AssertValuesEqual(
      df[paste0("quiz_", quiz_num, "_norm")],
      grades[paste0("quiz_", quiz_num)] / 10,
    )
  }
}

quiz_cols <- GetMatchingEntries(names(grades), "quiz_[0-9]+$")

NormalizeColsByCols(grades, cols=quiz_cols, max_score_cols="quiz_max") %>% CheckQuizNorm()
NormalizeColsByNumber(grades, cols=quiz_cols, max_score=10) %>% CheckQuizNorm()
NormalizeColsByNumber(grades[quiz_cols], max_score=10) %>% CheckQuizNorm()


# Check normalizing by multiple columns
hw_max <- c(100, 50, 150)
CheckHWNorm <- function(df) {
  for (hw_num in 1:3) {
    AssertValuesEqual(
      df[paste0("homework_", hw_num, "_norm")],
      grades[paste0("homework_", hw_num)] / hw_max[hw_num],
    )
  }
}

hw_cols <- GetMatchingEntries(names(grades), "homework_[0-9]+$")
hw_max_cols <- GetMatchingEntries(names(grades), "homework_[0-9]+_max$")
NormalizeColsByCols(grades, cols=hw_cols, max_score_cols=hw_max_cols) %>% CheckHWNorm()
NormalizeColsByNumber(grades, cols=hw_cols, max_score=hw_max) %>% CheckHWNorm()
NormalizeColsByNumber(grades[hw_cols], max_score=hw_max) %>% CheckHWNorm()


# Check non--homogeneous rows
test_df <- data.frame(
  a=runif(n_students), b=runif(n_students), 
  c=runif(n_students) + 1, d=runif(n_students) + 1)

test_df_norm <- NormalizeColsByCols(
  test_df, cols=c("a", "b"), max_score_cols=c("c", "d"))

AssertValuesEqual(
  test_df_norm,
  test_df[c("a", "b")] / test_df[c("c", "d")])


# Check weighted means
CheckHWMean <- function(df, weights=rep(1/3, 3)) {
  AssertValuesEqual(
    df, rowSums(grades[hw_cols] * rep(weights, each=nrow(grades))))
}

weights <- 1:3
weights_norm <- weights / sum(weights)

ComputeWeightedMean(grades, cols=hw_cols) %>% CheckHWMean()
ComputeWeightedMean(grades[hw_cols]) %>% CheckHWMean()
suppressWarnings(
  ComputeWeightedMean(grades[hw_cols], weights=weights) %>% 
    CheckHWMean(weights=weights_norm)
)




# Check dropping
AssertAllMeansLt <- function(df1, df2) {
  stopifnot(all(rowMeans(df1) < rowMeans(df2)))
}

quiz_mins <- apply(grades[quiz_cols], FUN=min, MARGIN=1)
quiz_maxs <- apply(grades[quiz_cols], FUN=max, MARGIN=1)

# Check that dropping only increases averages
AssertAllMeansLt(
  grades[quiz_cols], 
  DropLowest(grades, cols=quiz_cols, num_drops=1))

AssertAllMeansLt(
  DropLowest(grades, cols=quiz_cols, num_drops=1),
  DropLowest(grades, cols=quiz_cols, num_drops=2))

AssertAllMeansLt(
  DropLowest(grades, cols=quiz_cols, num_drops=2),
  DropLowest(grades, cols=quiz_cols, num_drops=3))

# Check that the lowest is dropped
AssertValuesEqual(
  rowSums(DropLowest(grades, cols=quiz_cols, num_drops=1)),
  rowSums(grades[quiz_cols]) - quiz_mins
)
  
# Check that the highest is kept
AssertValuesEqual(
  rowSums(DropLowest(grades, cols=quiz_cols, num_drops=3)),
  quiz_maxs
)

# Check the default columns
AssertValuesEqual(
  rowSums(DropLowest(grades[quiz_cols], num_drops=1)),
  rowSums(grades[quiz_cols]) - quiz_mins
)
