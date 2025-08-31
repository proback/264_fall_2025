set.seed(5625)
students <- c("Rania", "Maroova", "Elise", "Rain", "George", "Gracia",
              "Anh", "Jackson", "Mario", "Zuzanna", "Theresa", "Z")
tables <- rep(c("Table 2", "Table 3", "Table 5"), 4)
tibble(students = students, tables = tables) %>%
  mutate(tables = sample(tables)) %>%
  arrange(tables)
