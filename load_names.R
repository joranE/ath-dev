library(readr)
NAMES <- read_csv("name.csv",
                  col_names = TRUE,
                  col_types = "c")