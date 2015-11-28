# library(readr)
# DATA <- read_csv("fis.csv",
#                  col_names = TRUE,
#                  col_types = "iccccccciidd")
#FIS <- src_sqlite("fis.sqlite3",create = FALSE)
FIS <- src_mysql(options()$mysql$dbName,
                 options()$mysql$host,
                 options()$mysql$port,
                 options()$mysql$user,
                 options()$mysql$password)
DATA <- tbl(FIS,"main")
PENALTY <- DATA %>%
  group_by(raceid) %>%
  summarise(penalty = min(fispoints)) %>%
  collect()
N_RACE <- DATA %>%
  group_by(fisid) %>%
  summarise(n_race = n_distinct(raceid)) %>%
  filter(n_race >= 30) %>%
  collect()

# x <- x %>%
#   group_by(raceid) %>%
#   mutate(penalty = min(fispoints,na.rm = TRUE)) %>%
#   group_by(fisid) %>%
#   mutate(n_race = n_distinct(raceid)) %>%
#   filter(n_race >= 30) %>%
#   select(raceid,date,season,cat1,gender,type,tech,fisid,name,age,rank,fispoints,penalty)