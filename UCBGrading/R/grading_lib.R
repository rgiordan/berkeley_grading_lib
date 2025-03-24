if (FALSE) {
  # Make sure the dropping logic works with dplyr
  
  # You have to have grouped before calling DropLowest  
  DropLowest <- function(df, vals, drop_num) {
    df_drop <-
      df %>%
      mutate(sorting_ind=rank({{vals}})) %>%
      filter(sorting_ind > drop_num) %>%
      select(-sorting_ind)
    return(df_drop)    
  }
  
  foo <- data.frame(g1=rep(c("a", "b"), each=6),
                    g2=rep(c("c", "d"), 6)) %>%
    arrange(g2, g1) %>%
    mutate(v=1:n())
  
  foo %>% 
    group_by(g1, g2) %>%
    mutate(sorting_ind=order(v)) %>%
    filter(sorting_ind > 1)
  
  foo %>% 
    group_by(g1, g2) %>%
    DropLowest(v, 1)
  
  foo %>% 
    group_by(g1, g2) %>%
    DropLowest(v, 2)
}


if (FALSE) {
  # Visual sanity check of assignment max scores for each assignment
  df_long %>%
    group_by(assignment) %>%
    summarize(max_value=min(max_value), max_value2=max(value)) %>%
    View()
}



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

if (FALSE) {
  # Test
  for (base in c(0.5, 0.6, 0.7, 0.8, 0.9)) {
    for (offset in c(0.01, 0.05, 0.099)) {
      print(GetLetterGrade(base + offset))
    }
  }
}

