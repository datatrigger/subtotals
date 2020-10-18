# Import necessary packages
library(tibble)
library(dplyr)
library(forcats)

# Generate the data
df <- tibble(
  Group = c(rep("X", 4), rep("Y", 6)),
  Subgroup = c(rep("a", 3), "z", rep("a", 3), rep("z", 3)),
  Value = sample(20, 10, replace = TRUE)
)

df %>%
  # Concatenate
  bind_rows(
    df %>% mutate(Subgroup = "Total"),
    df %>% mutate(across(c(Group, Subgroup), ~ "Total"))
  ) %>%
  # Aggregate
  group_by(
    Group,
    Subgroup
  ) %>%
  summarise(
    Mean = mean(Value)
  ) %>%
  ungroup() %>%
  # Reorder factor levels
  mutate(
    Group = Group %>% as.factor() %>% fct_relevel("Total", after = Inf),
    Subgroup = Subgroup %>% as.factor() %>% fct_relevel("Total", after = Inf)
  ) %>%
  # Arrange
  arrange(
    Group,
    Subgroup
  )