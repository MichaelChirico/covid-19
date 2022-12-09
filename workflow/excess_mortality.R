require(stringr)
require(tidyr)
require(cbsodataR)
require(reshape2)
require(lubridate)
require(readxl)

table_mortality <- data.table(cbs_get_data("70895ned", 
                                Geslacht = "1100",
                                Perioden = has_substring("W") | has_substring("X")))

## find number of days of each week
week_labels <- data.frame(attributes(table_mortality$Perioden)$labels)
week_labels$lbl <- row.names(week_labels)
week_labels$w_length <- unlist(lapply(1:nrow(week_labels),
                                      function(x) {
                                        tmp <- gsub("[\\(\\)]", "",
                                                    regmatches(week_labels$lbl[x],
                                                               gregexpr("\\(.*?\\)", week_labels$lbl[x])
                                                    )[[1]]
                                        )
                                        if (length(tmp) > 0) {
                                          return(tmp)
                                        } else {
                                          return ('7 dagen')
                                        }
                                      }))
week_labels <- as.data.table(week_labels)

## add week length
setkeyv(week_labels, names(week_labels)[1])
setkey(table_mortality,Perioden)
table_mortality[week_labels,
                w_length := substr(i.w_length, 1, 1)
][,
  Perioden := gsub('X', 'W', Perioden)
]

table_mortality <- setorder(table_mortality, LeeftijdOp31December, Perioden)

## move deaths from week 53 or 0 to week 52 or 1 depending on week length
table_mortality[, ':=' (year = as.numeric(substr(Perioden, 1, 4)),
                        week = as.numeric(substr(Perioden, 7, 8))
)
][,
  ':=' (week_update1 = Overledenen_1 + data.table::shift(Overledenen_1),
        week_update2 = Overledenen_1 + data.table::shift(Overledenen_1, -1)
  )
][week == 1 & w_length < 7,
  Overledenen_1 := week_update1
][week == 52 & w_length < 7,
  Overledenen_1 := week_update2
][,
  ':=' (week_update1 = NULL, week_update2 = NULL)
]


table_mortality$Year <- substr(table_mortality$Perioden, 1, 4)

table_mortality$Week <- str_sub(table_mortality$Perioden, start = -2)

week.readfile <- as.numeric(last(table_mortality$Week))

table_mortality$LeeftijdOp31December <- factor(table_mortality$LeeftijdOp31December, levels = c(10000, 41700, 53950, 21700),
                                               labels = c("Totaal","0 tot 65", "65 tot 80", "80+"))

bevolking <- cbs_get_data("37296ned", Perioden = has_substring(c("2001JJ00","2002JJ00","2003JJ00","2004JJ00","2005JJ00","2006JJ00","2013JJ00","2014JJ00","2015JJ00", "2016JJ00","2017JJ00","2018JJ00","2019JJ00","2020JJ00","2021JJ00")))
bevolking <- bevolking[,c("Perioden","TotaleBevolking_1", "JongerDan20Jaar_10","k_20Tot40Jaar_11","k_40Tot65Jaar_12","k_65Tot80Jaar_13","k_80JaarOfOuder_14")]
colnames(bevolking) <- c("Jaar","Totaal","Jonger20","20tot40","40tot65","65tot80","80ouder")

bevolking$jonger65 <- bevolking$Jonger20 + bevolking$`20tot40` + bevolking$`40tot65`
bevolking <- bevolking[,c("Jaar","Totaal","jonger65","65tot80","80ouder")]


## Get 2022 data
bevolking2022 <- cbs_get_data("83482NED", Perioden = has_substring(c("2022MM01")),
                                           Generatie = has_substring(c("T001040")),
                                           Migratieachtergrond = has_substring(c("T001040")),
                                           Geslacht = has_substring(c("T001038")), typed = T)

bevolking2022 <- bevolking2022 %>%
  dplyr::filter(Leeftijd >= 70000 | Leeftijd == 22200)

bevolking2022 <- data.frame(cbind(2022,
                               sum(bevolking2022[1:21,"BevolkingOpDeEersteVanDeMaand_1"]),
                               sum(bevolking2022[1:13,"BevolkingOpDeEersteVanDeMaand_1"]),
                               sum(bevolking2022[14:16,"BevolkingOpDeEersteVanDeMaand_1"]),
                               sum(bevolking2022[17:21,"BevolkingOpDeEersteVanDeMaand_1"])))
colnames(bevolking2022) <- c("Jaar","Totaal","jonger65","65tot80","80ouder")

bevolking <- rbind(bevolking, bevolking2022)


colnames(bevolking) <- c("Year","Totaal","0 tot 65","65 tot 80","80+")
bevolking$Year <- substr(bevolking$Year, 1, 4)

bevolking.long <- gather(bevolking,"LeeftijdOp31December","Bevolking",2:5)

#bevolking$Bevolking <- as.numeric(bevolking$Bevolking)


## Select weeks
table_mortality <- subset(table_mortality, Week > 00 & Week != 53)
table_mortality <- table_mortality[!table_mortality$Week == '00',]

#& Week < weeknumber+2

mortality_full <- merge(table_mortality, bevolking.long, by=c("Year","LeeftijdOp31December"), all.x=TRUE)

# Reframe bevolking 2021 for merge
bevolking2022 <- t(bevolking2022[1,2:5])
bevolking2022 <- data.frame(bevolking2022,LeeftijdOp31December=c("Totaal","0 tot 65","65 tot 80","80+"), row.names = NULL)
colnames(bevolking2022) <- c("Bevolking2022","LeeftijdOp31December")


#bevolking2021.test <- data.frame(Bevolking2020=c(14017269, 2618728, 838696, 17474693),
#                          LeeftijdOp31December=c("0 tot 65","65 tot 80","80+","Totaal"))

mortality_full <- merge(mortality_full, bevolking2022, by=c("LeeftijdOp31December"))
mortality_full <- mortality_full %>%
  mutate(Overledenen_1 = Overledenen_1/Bevolking*Bevolking2022)

mortality_wide <- reshape2::dcast(mortality_full, LeeftijdOp31December + Week ~ Year, value.var = "Overledenen_1", sum)
mortality_wide$`2022` <- na_if(mortality_wide$`2022`, 0)

mortality_wide$Average20172021 <- rowMeans(mortality_wide[,c("2017","2018","2019","2020","2021")])
mortality_wide$Average20162020 <- rowMeans(mortality_wide[,c("2016","2017","2018","2019","2020")])
mortality_wide$Average20152019 <- rowMeans(mortality_wide[,c("2015","2016","2017","2018","2019")])

mortality_wide$excess_death2022 <- round(mortality_wide$`2022` - mortality_wide$Average20152019,0)
mortality_wide$excess_death2021 <- round(mortality_wide$`2021` - mortality_wide$Average20152019,0)
mortality_wide$excess_death2020 <- round(mortality_wide$`2020` - mortality_wide$Average20152019,0)

excess_deaths2020 <- aggregate(excess_death2020 ~ LeeftijdOp31December + Week, data = mortality_wide, FUN = sum)
excess_deaths2021 <- aggregate(excess_death2021 ~ LeeftijdOp31December + Week, data = mortality_wide, FUN = sum)
excess_deaths2022 <- aggregate(excess_death2022 ~ LeeftijdOp31December + Week, data = mortality_wide, FUN = sum)
excess_deaths2020$Year <- 2020
excess_deaths2021$Year <- 2021
excess_deaths2022$Year <- 2022
colnames(excess_deaths2020) <- c("LeeftijdOp31December","Week","excess_death","Year")
colnames(excess_deaths2021) <- c("LeeftijdOp31December","Week","excess_death","Year")
colnames(excess_deaths2022) <- c("LeeftijdOp31December","Week","excess_death","Year")

excess_deaths <- rbind(excess_deaths2020, excess_deaths2021, excess_deaths2022)


excess_deaths_wide <- spread(excess_deaths, key = LeeftijdOp31December, value = excess_death)
excess_deaths_wide$total_deaths_corrected <- excess_deaths_wide$`0 tot 65` + excess_deaths_wide$`65 tot 80` + excess_deaths_wide$`80+`
excess_deaths_wide$Week <- as.numeric(excess_deaths_wide$Week)

#griep <- subset(mortality_wide, Week > 13)
#excess_flu <- aggregate(excess_flu ~ LeeftijdOp31December, data = griep, FUN = sum)

#hittegolf2006 <- subset(mortality_wide, Week > 26 & Week < 31)
#excess_heatwave <- aggregate(excess_heatwave ~ LeeftijdOp31December, data = hittegolf2006, FUN = sum)

#alleen_ondersterfte <- subset(mortality_wide, Week > 19)
#less_deaths <- aggregate(excess_death ~ LeeftijdOp31December + Week, data = alleen_ondersterfte, FUN = sum)

#age_corrected_less <- round((sum(less_deaths$excess_death) - less_deaths[less_deaths$LeeftijdOp31December == "Totaal","excess_death"]),0)

#week.benchmark <- round(mortality_wide[which(mortality_wide$Week == weeknumber & mortality_wide$LeeftijdOp31December == "Totaal"),"Average20152019"],0)

#week.2020 <- round(mortality_wide[which(mortality_wide$Week == weeknumber & mortality_wide$LeeftijdOp31December == "Totaal"),"2020"],0)

## Calculate official death numbers

nl_dt <- read.csv("corrections/deaths_perweek.csv")[,c("Week","weekdeath_today","Year")]
nl_dt <- data.table(nl_dt)
nl_dt <- nl_dt[,c(1,3,2)]
colnames(nl_dt) <- c("Week","Year","covid_deaths")
nl_dt$covid_deaths <- as.numeric(nl_dt$covid_deaths)
nl_dt$Year <- as.numeric(nl_dt$Year)
#nl_dt <- nl_dt[c(1:(nrow(nl_dt)-1)),] ## Only use data up to week 30

excess_deaths_wide <- merge(excess_deaths_wide, nl_dt, by=c("Week","Year"), all.x=T)

deaths_2020 <- mortality_wide %>%
  dplyr::select(LeeftijdOp31December,Week,`2020`) %>%
  spread(LeeftijdOp31December, `2020`) %>%
  add_column(Year = 2020)

deaths_2021 <- mortality_wide %>%
  dplyr::select(LeeftijdOp31December,Week,`2021`) %>%
  spread(LeeftijdOp31December, `2021`) %>%
  add_column(Year = 2021)

deaths_2022 <- mortality_wide %>%
  dplyr::select(LeeftijdOp31December,Week,`2022`) %>%
  spread(LeeftijdOp31December, `2022`) %>%
  add_column(Year = 2022)


deaths_2020 <- rbind(deaths_2020,deaths_2021,deaths_2022)
deaths_2020$Week <- as.numeric(deaths_2020$Week)

deaths_weekly <- merge(deaths_2020, excess_deaths_wide, by = c("Week","Year"))

## Merge excess mortality with DLM

week.now <- week(Sys.Date())-2 ## Which week?

df_cbsmodel <- read.csv("data-misc/excess_mortality/excess_mortality_dlm_2021.csv")[,c(2:10)]

colnames(df_cbsmodel) <- c("Week","Year","DLModel_week_estimate","DLModel_upperbound95","DLModel_lowerbound95",
                           "Oversterfte_DLModel_cumul_low","Oversterfte_DLModel_cumul_mid",
                           "Oversterfte_DLModel_cumul_high","weekyear")

df_cbsmodel <- df_cbsmodel %>%
  mutate(DLModel_lowerbound95 = round(DLModel_lowerbound95,0)) %>%
  mutate(DLModel_upperbound95 = round(DLModel_upperbound95,0)) %>%
  mutate(DLModel_week_estimate = round(DLModel_week_estimate,0)) %>%
  mutate(Oversterfte_DLModel_cumul_low = round(Oversterfte_DLModel_cumul_low,0)) %>%
  mutate(Oversterfte_DLModel_cumul_mid = round(Oversterfte_DLModel_cumul_mid,0)) %>%
  mutate(Oversterfte_DLModel_cumul_high = round(Oversterfte_DLModel_cumul_high,0))

colnames(deaths_weekly) <- c("Week","Year","Overleden0_65","Overleden65_80","Overleden80+","Totaal_Overleden",
                                  "Oversterfte0_65","Oversterfte65_80","Oversterfte80+","Oversterfte_Totaal",
                                  "Oversterfte_Totaal_Gecorrigeerd","covid_sterfgevallen")

deaths_weekly <- merge(deaths_weekly, df_cbsmodel,by=c("Week","Year"),all.y=T)

## RIVM excess mortality
excess.mort.rivm <- read.csv("data-misc/excess_mortality/excess_mortality_rivm.csv")
colnames(excess.mort.rivm) <- c("Year","Week","start_week","end_week","lower_bound","upper_bound","mortality","excess_mortality_rivm","expected_mortality")
deaths_weekly <- merge(deaths_weekly, excess.mort.rivm[,c("Year","Week","excess_mortality_rivm")],by=c("Week","Year"),all.x=T)

## CBS death statistics
cbs_links <- read.csv("data-misc/excess_mortality/links_death_causes.csv")
cbs_url <- last(cbs_links)
page <- read_html(cbs_url[1,1])
page <- page %>% html_nodes("a") %>% html_attr('href')
page <- data.frame(page)

page$category <- grepl(".xlsx", page$page, fixed = TRUE)
page <- page %>%
  dplyr::filter(category == "TRUE")
u.cbs <- page[1,1]
#webpage.cbs <- read_html(u.cbs)

download.file(u.cbs,destfile = "cbs_deaths.xlsx", mode = "wb")
cbs.death.statistics <- data.table(read_excel("cbs_deaths.xlsx",sheet = 2))[5:57,c(1,5,6,8,9,11,12)]
unlink("cbs_deaths.xlsx")
colnames(cbs.death.statistics) <- c("Week","2020_covid","2020_non_covid","2021_covid","2021_non_covid", "2022_covid","2022_non_covid")

cbs.death.statistics <- cbs.death.statistics %>%
  mutate(Week = parse_number(Week))

cbs.covid <- gather(cbs.death.statistics[,c("Week","2020_covid","2021_covid","2022_covid")], key = "Year",value = "Covid_deaths_CBS_death_statistics",-Week)
cbs.covid$Year <- parse_number(cbs.covid$Year)
cbs.noncovid <- gather(cbs.death.statistics[,c("Week","2020_non_covid","2021_non_covid","2022_non_covid")], key = "Year",value = "Mortality_without_covid_CBS",-Week)
cbs.noncovid$Year <- parse_number(cbs.noncovid$Year)

cbs.death.statistics <- merge(cbs.covid, cbs.noncovid, by = c("Week","Year"))

deaths_weekly <- merge(deaths_weekly, cbs.death.statistics, by = c("Week","Year"), all.x=T)

deaths_weekly <- deaths_weekly %>%
  mutate(Totaal_Overleden = round(Totaal_Overleden,0)) %>%
  mutate(Overleden0_65 = round(Overleden0_65,0)) %>%
  mutate(Overleden65_80 = round(Overleden65_80,0)) %>%
  mutate(`Overleden80+` = round(`Overleden80+`,0))

## CBS method excess mortality

cbs_links <- read.csv("data-misc/excess_mortality/links_cbs_mortality.csv")
cbs_url <- last(cbs_links)
page <- read_html(cbs_url[1,1])
page <- page %>% html_nodes("a") %>% html_attr('href')
page <- data.frame(page)

page$category <- grepl(".xlsx", page$page, fixed = TRUE)
page <- page %>%
  dplyr::filter(category == "TRUE")
cbs_url <- page[1,1]

download.file(cbs_url,destfile = "cbs_deaths.xlsx", mode = "wb")
cbs_oversterfte <- data.table(read_excel("cbs_deaths.xlsx", sheet = 7))[,1:10]
unlink("cbs_deaths.xlsx")
cbs_oversterfte <- cbs_oversterfte[8:nrow(cbs_oversterfte),]
cbs_oversterfte <- cbs_oversterfte[-c(54,107),]
cbs_oversterfte <- cbs_oversterfte[-c(158:163),]
colnames(cbs_oversterfte) <- c("Periode","deaths_expected_cbs","verwacht_lb","Verwacht_ub","Verwacht_WLZ","verwacht_WLZ_lb","Verwacht_WLZ_ub",
                               "Verwacht_other","Verwacht_other_lb","Verwacht_other_ub")

cbs_oversterfte$Year <- parse_number(cbs_oversterfte$Periode)
cbs_oversterfte$Week <- c(1:53,1:52,1:52)

cbs.death.statistics <- cbs_oversterfte[,c("deaths_expected_cbs","Week","Year")]
cbs.death.statistics$deaths_expected_cbs <- parse_number(cbs.death.statistics$deaths_expected_cbs)

deaths_weekly <- merge(deaths_weekly, cbs.death.statistics, by = c("Week","Year"), all.x=T)
deaths_weekly$excess_cbs_method <- deaths_weekly$Totaal_Overleden-deaths_weekly$deaths_expected_cbs

# Arrange and write file
deaths_weekly <- arrange(deaths_weekly, Year, Week)

deaths_weekly <- deaths_weekly %>%
  mutate(percent_excess = round((Totaal_Overleden - deaths_expected_cbs)/deaths_expected_cbs*100,1))

write.csv(deaths_weekly, file = "data-misc/excess_mortality/excess_mortality.csv", row.names = F)

cbp2 <- c("#009E73", "#87109A","#E6830C",
          "red", "#2231C5","#000000")

mortality_wide <- mortality_wide %>%
  dplyr::filter(LeeftijdOp31December == "Totaal")


mortality_wide %>%
  ggplot(aes(x=Week, y=`2017`, group = 1)) + 
  geom_line(aes(y = `2017`, color = "2017"), linewidth=1.0, linetype = "dashed") +
  geom_line(aes(y = `2018`, color = "2018"), linewidth=1.0, linetype = "dashed") +
  geom_line(aes(y = `2019`, color = "2019"), linewidth=1.0, linetype = "dashed") +
  geom_line(aes(y = `2020`, color = "2020"), linewidth=1.0) +
  geom_line(aes(y = `2021`, color = "2021"), linewidth=1.0) +
  geom_line(aes(y = `2022`, color = "2022"), linewidth=1.2) +
  scale_y_continuous(limits = c(2000, 5500)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x.bottom = element_text(size=12),
        axis.text.y = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.pos = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank()) +
  labs(x = "Datum",
       y = "Besmettingen per dag",
       color = "Legend",
       caption = paste("Bron data: CBS  | Plot: @mzelst | ",Sys.Date())) + 
  ggtitle("Overledenen") + 
  scale_colour_manual(values=cbp2) +
  annotate("text", x = 9, y = 4300, label = "Griepgolf (2018)", color="#87109A") +
  annotate("text", x = 14, y = 5300, label = "Eerste golf", color = "red") +
  annotate("text", x = 32, y = 3400, label = "Hittegolf (2020)", color ="red") +
  annotate("text", x = 44, y = 3300, label = "Tweede golf", color = "red") +
  annotate("text", x = 14, y = 3400, label = "Derde golf", color = "#2231C5") +
  annotate("text", x = 43, y = 4000, label = "Vijfde golf", color = "#2231C5") +
  annotate("text", x = 14, y = 3700, label = "Zesde golf", color = "#000000") +
  annotate("text", x = 24, y = 3200, label = "Zevende golf", color = "#000000")
ggsave("data-misc/excess_mortality/plots_weekly_update/sterfte_perweek.png", width = 16, height = 8)

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("Excess mortality analyses - Week ", week.readfile))
git2r::push(repo, credentials = git.auth)
