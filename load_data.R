# library(readr)
# DATA <- read_csv("fis.csv",
#                  col_names = TRUE,
#                  col_types = "iccccccciidd")
FIS <- src_sqlite("fis.sqlite3",create = FALSE)
DATA <- tbl(FIS,"main")