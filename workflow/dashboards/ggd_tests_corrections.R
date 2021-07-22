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

temp = last(list.files(path = "data-rivm/tests/",pattern="*.csv.gz", full.names = T),1)
tests_growth = read.csv(temp)

tests_growth <- tests_growth %>%
  mutate(perc_positive = round(Tested_positive/Tested_with_result*100,2)) %>%
  mutate(perc_positive_7d = frollmean(perc_positive,7)) %>%
  mutate(tested_positive_7d = frollmean(Tested_positive,7)) %>%
  mutate(Tested_7d = frollmean(Tested_with_result,7)) %>%
  group_by(Security_region_code) %>%
  mutate(growth_pos = lead(tested_positive_7d)/tested_positive_7d) %>%
  filter(Date_of_statistics > Sys.Date()-14)


write.csv(tests.corr, file = "corrections/ggd_tests_corrections.csv",row.names=F)

tests_growth$Date_of_statistics <- as.Date(tests_growth$Date_of_statistics)

ggd_tests <- tests_growth %>%
  ggplot(aes(x=Date_of_statistics, y=growth_pos)) + 
  geom_line(aes(y = growth_pos, color = "Toename besmettingen per dag (incl. correcties)"), lwd=1.2) +
  #scale_x_date(expand = c(0, 2)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0.8, 1.3)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=12, hjust = 0.2),
        axis.text.y = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.pos = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Besmettingen per dag",
       subtitle = "Maandagen",
       color = "Legend") +
  geom_hline(yintercept=1) +
  ggtitle("Meldingen van geconstateerde besmettingen")


ggd_tests + facet_wrap(~Security_region_name, scales = "free_y")

