# library(statskier2)
# library(dplyr)
# conl <- db_xc_local()
# x <- statskier2::ss_query(conl,"select * from main")
# x <- x %>%
#   group_by(raceid) %>%
#   mutate(penalty = min(fispoints,na.rm = TRUE)) %>%
#   group_by(fisid) %>%
#   mutate(n_race = n_distinct(raceid)) %>%
#   filter(n_race >= 30) %>%
#   select(raceid,date,season,cat1,gender,type,tech,fisid,name,age,rank,fispoints,penalty)
# 
# write.table(x = x,
#             file = "fis.csv",
#             sep = ",",
#             row.names = FALSE,
#             col.names = TRUE)
# 
# fis_db <- src_sqlite("fis.sqlite3",create = TRUE)
# fis_main <- copy_to(fis_db,
#                     x,
#                     name = "main",
#                     temporary = FALSE,
#                     indexes = list("raceid","fisid","cat1","type"))
# 
# write.table(x = data.frame(name = unique(x$name)),
#             file = "name.csv",
#             sep = ",",
#             row.names = FALSE,
#             col.names = TRUE)