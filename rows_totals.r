# Import necessary packages
library(tibble)
library(dplyr)
library(forcats)
library(ggplot2)

# Generate the data
df <- tibble(
  Group = c(rep("X", 4), rep("Y", 6)),
  Subgroup = c(rep("a", 3), "z", rep("a", 3), rep("z", 3)),
  Value = sample(20, 10, replace = TRUE)
)

# Grouped table
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

# Dataviz example

## Group data
df <- mtcars %>%
  mutate(
    cyl = cyl %>% as.character()
  ) %>%
  bind_rows(
    mtcars %>% mutate(cyl = 'Total')
  ) %>%
  mutate(
    cyl = cyl %>% as.factor() %>% fct_relevel("Total", after = Inf)
  ) %>%
  group_by(
    cyl
  ) %>%
  summarize(
    mean_mpg = mean(mpg)
  )

# Visualize
ggplot(data = df) +
  aes(x = cyl, y = mean_mpg) +
  geom_col(color = "cornflowerblue", fill = "cornflowerblue") +
  xlab("Number of cylinders") +
  scale_y_continuous(name = "Miles / gallon", breaks = seq(0, 30, 5)) +
  ggtitle("Average miles/gallon by number of cylinders") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size=25)
  )