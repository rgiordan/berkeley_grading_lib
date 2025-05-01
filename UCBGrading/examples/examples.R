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

quiz_cols <- GetMatchingEntries(names(grades), "quiz_[0-9]+$")
hw_cols <- GetMatchingEntries(names(grades), "homework_[0-9]+$")
hw_max_cols <- GetMatchingEntries(names(grades), "homework_[0-9]+_max$")



NormalizeColsByCols(grades, "quiz_max", cols=quiz_cols) %>%
DropLowest(num_drops=1, new_prefix="quiz_dropped_")

grades %>%
  NormalizeColsByCols("quiz_max", cols=quiz_cols) %>%
  DropLowest(num_drops=1, new_prefix="quiz_dropped_") %>%
  ComputeWeightedMean(new_col="quiz_mean")



bind_cols(
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

ComputeWeightedMean(grades, cols=quiz_cols, new_col="quiz_mean", weights=1:4)
DropLowest(grades, cols=hw_cols, num_drops=1, new_prefix="hw_dropped_")

NormalizeColsByCols(grades, cols=quiz_cols, max_score_cols="quiz_max")


##########
# Tests

AssertValuesEqual <- function(df1, df2, tol=1e-9) {
  err <- mean(abs(as.matrix(df1) - as.matrix(df2)))
  if (err > tol) {
    stop(sprintf("Error > Tol: %f > %f", err, tol))
  }
}


CheckQuizNorm <- function(df) {
  for (quiz_num in 1:4) {
    AssertValuesEqual(
      df[paste0("quiz_", quiz_num, "_norm")],
      grades[paste0("quiz_", quiz_num)] / 10,
    )
  }
}

NormalizeColsByCols(grades, cols=quiz_cols, max_score_cols="quiz_max") %>% CheckQuizNorm()
NormalizeColsByNumber(grades, cols=quiz_cols, max_score=10) %>% CheckQuizNorm()
NormalizeColsByNumber(grades[quiz_cols], max_score=10) %>% CheckQuizNorm()






