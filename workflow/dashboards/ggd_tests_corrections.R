temp = list.files(path = "data-rivm/tests/",pattern="*.csv.gz", full.names = T)

myfiles = lapply(temp, read.csv)

tests_per_day <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

tests_agg_day <- aggregate(Tested_with_result ~ Date_of_statistics + Date_of_report, data = tests_per_day, FUN = sum)
tests_agg_day <- tests_agg_day %>%
  group_by(Date_of_statistics) %>%
  mutate(diff_tests = Tested_with_result - lag(Tested_with_result))

tests.corr <- tests_agg_day %>%
  dplyr::filter(diff_tests > 0 | diff_tests < 0)


pos_tests_agg_day <- aggregate(Tested_positive ~ Date_of_statistics + Date_of_report, data = tests_per_day, FUN = sum)

pos_tests_agg_day <- pos_tests_agg_day %>%
  group_by(Date_of_statistics) %>%
  mutate(diff_pos_tests = Tested_positive - lag(Tested_positive))

tests_pos.corr <- pos_tests_agg_day %>%
  dplyr::filter(diff_pos_tests > 0 | diff_pos_tests < 0)

tests.ggd <- merge(tests_pos.corr, tests.corr, by = c("Date_of_statistics","Date_of_report"), all = T)

##

write.csv(tests.ggd, file = "corrections/ggd_tests_corrections.csv",row.names=F)

temp = last(list.files(path = "data-rivm/tests/",pattern="*.csv.gz", full.names = T),1)
tests_growth = read.csv(temp)

percentage_positive_daily_national <- read.csv("data-dashboards/percentage-positive-daily-national.csv")

tests_growth <- percentage_positive_daily_national %>%
  mutate(perc_positive = round(values.infected/values.tested_total*100,2)) %>%
  mutate(perc_positive_7d = frollmean(perc_positive,7)) %>%
  mutate(tested_positive_7d = frollmean(values.infected,7)) %>%
  mutate(Tested_7d = frollmean(values.tested_total,7)) %>%
  mutate(growth_pos = lead(tested_positive_7d)/tested_positive_7d) %>%
  mutate(growth_perc = lead(perc_positive_7d)/perc_positive_7d) %>%
  mutate(diff_growth = growth_pos - growth_perc)



tests_growth$Date_of_statistics <- as.Date(tests_growth$date)

ggd_tests <- tests_growth %>%
  ggplot(aes(x=Date_of_statistics, y=growth_pos)) + 
  geom_line(aes(y = growth_pos, color = "Groei in besmettingen"), lwd=1.0) +
  geom_line(aes(y = growth_perc, color = "Groei in % positief"), lwd=1.0) +
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
  geom_vline(xintercept= as.Date("2021-01-10")) +
  geom_vline(xintercept= as.Date("2021-05-10")) +
  ggtitle("Meldingen van geconstateerde besmettingen")
ggd_tests

#ggd_tests + facet_wrap(~Security_region_name, scales = "free_y")



tests_growth %>%
  ggplot(aes(x=Date_of_statistics, y=diff_growth)) + 
  geom_line(aes(y = diff_growth, color = "Verschil in groei tussen positieve tests en % positief"), lwd=1.0) +
  #geom_line(aes(y = growth_pos, color = "Groei in besmettingen"), lwd=1.0) +
  #geom_line(aes(y = growth_perc, color = "Groei in % positief"), lwd=1.0) +
  #scale_x_date(expand = c(0, 2)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 0.5)) +
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
       subtitle = ">0 = positieve testen groeien harder dan % positief \n Dikgedrukt = Piek in positieve tests \n Dotted = Dal in positieve tests",
       color = "Legend") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept= as.Date("2020-12-23")) +
  geom_vline(xintercept= as.Date("2021-02-13"), linetype = "dotted") +
  geom_vline(xintercept= as.Date("2021-03-29")) +
  geom_vline(xintercept= as.Date("2021-04-22")) +
  geom_vline(xintercept= as.Date("2021-06-28"), linetype = "dotted") +
  geom_vline(xintercept= as.Date("2021-07-19")) +
  ggtitle("Verschil in groei tussen besmettingen en % positief")
ggsave("plots/verschil_groei_postests_percentage.png")



git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

##Push to git
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily GGD test tracker"))
git2r::push(repo, credentials = git.auth)