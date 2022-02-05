tests.df <- aggregate(cbind(Tested_with_result,Tested_positive) ~ Date_of_statistics, data = tests, FUN = sum)

tests.df <- tests.df %>%
  mutate(pos.rate = Tested_positive/Tested_with_result*100) %>%
  mutate(tests_7davg = frollmean(Tested_with_result,7)) %>%
  mutate(tests_14d = lag(tests_7davg,7)) %>%
  mutate(pos.rate.3d.avg = round(frollsum(Tested_positive,3)/frollsum(Tested_with_result,3)*100,1)) %>%
  mutate(pos.rate.7d.avg = round(frollsum(Tested_positive,7)/frollsum(Tested_with_result,7)*100,1)) %>%
  mutate(pos_tests_7davg = frollmean(pos.rate,7)) %>%
  mutate(pos_tests_14d = lag(pos_tests_7davg,7)) %>%
  mutate(growth_pos_tests = round(pos_tests_7davg/pos_tests_14d*100,1)) %>%
  mutate(growth_tests = round(tests_7davg/tests_14d*100,1))
  

colnames(tests.df) <- c("date","values.tested_total","values.infected","values.infected_percentage","tests.7d.avg","tests.7d_lagged_7","pos.rate.3d.avg","pos.rate.7d.avg",
                        "values.infected_7d.avg","values.infected_7d_lagged","growth_infected","growth_tests")

write.csv(tests.df, file = "data-dashboards/percentage-positive-daily-national.csv",row.names = F)

tests$pos.rate <- tests$Tested_positive/tests$Tested_with_result*100
colnames(tests) <- c("Version","Date_of_report","date","Security_region_code","Security_region_name","values.tested_total",
                     "values.infected","values.infected_percentage")
write.csv(tests, file = "data-dashboards/percentage-positive-daily-safetyregion.csv",row.names = F)