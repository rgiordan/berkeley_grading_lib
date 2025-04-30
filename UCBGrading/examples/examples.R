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


bind_cols(
  grades["id"],
  grades %>%
    NormalizeColsByCols(quiz_cols, "quiz_max") %>%
    DropLowest(paste0(quiz_cols, "_norm"), num_drops=1, new_prefix="quiz_dropped_") %>%
    ComputeWeightedMean(paste0("quiz_dropped_", 1:3), "quiz_mean"),
  grades %>%
    NormalizeColsByCols(hw_cols, hw_max_cols) %>%
    DropLowest(paste0(hw_cols, "_norm"), num_drops=1, new_prefix="hw_dropped_") %>%
    ComputeWeightedMean(paste0("hw_dropped_", 1:2), "hw_mean")
)

ComputeWeightedMean(grades, quiz_cols, "quiz_mean", weights=1:4)
DropLowest(grades, hw_cols, num_drops=1, new_prefix="hw_dropped_")

NormalizeColsByCols(grades, quiz_cols, "quiz_max")
