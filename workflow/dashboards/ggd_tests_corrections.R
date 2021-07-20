temp = list.files(path = "data-rivm/tests/",pattern="*.csv.gz", full.names = T)

myfiles = lapply(temp, read.csv)

tests_per_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

tests_agg_day <- aggregate(Tested_positive ~ Date_of_statistics + Date_of_report, data = tests_per_day, FUN = sum)

tests_agg_day <- tests_agg_day %>%
  group_by(Date_of_statistics) %>%
  mutate(diff = Tested_positive - lag(Tested_positive))

tests.corr <- tests_agg_day %>%
  filter(diff > 0 | diff < 0)

write.csv(tests.corr, file = "corrections/ggd_tests_corrections.csv",row.names=F)

