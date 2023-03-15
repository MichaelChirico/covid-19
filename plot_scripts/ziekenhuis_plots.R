require(jsonlite)
require(rjson)

## ZIEKENHUIS

lcps <- fread("data/lcps_by_day.csv")

lcps %>%
  dplyr::filter(date >= "2022-01-01") %>%
  ggplot(aes(x = date)) + 
  geom_col(aes(y = kliniek_opnames_covid),width = 0.9, fill = "blue",color = "black") + 
  geom_line(aes(y = Kliniek_Opnames_7d), lwd = 1.2, color = "red") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 0.5)) + 
  labs(x = "Datum",
       y = "Opnames per dag",
       title = "Opnames per dag",
       color = "Legend",
       caption = paste("Bron data: LCPS  | Plot: @Ipie33 | ",Sys.Date()))
ggsave("plots/opnames_per_dag_kliniek.png", width = 16, height = 8)

lcps %>%
  dplyr::filter(date >= "2022-01-01") %>%
  ggplot(aes(x = date)) + 
  geom_col(aes(y = IC_opnames_covid),width = 0.9, fill = "blue",color = "black") + 
  geom_line(aes(y = IC_Opnames_7d), lwd = 1.2, color = "red") + 
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 0.5)) + 
  labs(x = "Datum",
       y = "IC-opnames per dag",
       title = "IC-opnames per dag",
       color = "Legend",
       caption = paste("Bron data: LCPS  | Plot: @Ipie33 | ",Sys.Date()))

ggsave("plots/opnames_per_dag_ic.png", width = 16, height = 8)


lcps.growth <- lcps[,c("date","OMT_Check_IC","OMT_Check_Kliniek")]
colnames(lcps.growth) <- c("date","Groei IC","Groei Kliniek")
lcps.growth <- lcps.growth %>%
  gather(indicator,growth,-date) %>%
  group_by(indicator) %>%
  mutate(growth_7d = frollmean(growth, 7)) %>%
  ungroup()

cols <- c("Groei IC" = "green", "Groei Kliniek" = "steelblue")

lcps.growth %>%
  dplyr::filter(date >= "2022-01-01") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = growth_7d/100, group = indicator, color = indicator), lwd = 1.2) +
  geom_hline(yintercept = 1) +
  theme_bw() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 0.5)) + 
  labs(x = "Datum",
       y = "Groei per dag",
       title = "Groei in opnames (week op week)",
       color = "Legend",
       caption = paste("Bron data: LCPS  | Plot: @Ipie33 | ",Sys.Date())) +
  scale_colour_manual(values = cols, name = cols)
ggsave("plots/groei_per_dag_opnames.png", width = 16, heigh = 8)  



git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) update LCPS update (figures)"))
push(repo, credentials = git.auth)

