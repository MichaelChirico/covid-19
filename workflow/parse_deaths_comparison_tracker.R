require(readxl)
require(sjmisc)

## Deaths NICE ##
temp = tail(list.files(path = "data-nice/exit/Clinical_Beds",pattern="*.csv", full.names = T),1)
deaths_clinic <- fread(temp)[,c("date","Overleden")]
temp = tail(list.files(path = "data-nice/exit/IC",pattern="*.csv", full.names = T),1)
deaths_IC <- fread(temp)[,c("date","Overleden")]

deaths_nice <- merge(deaths_clinic, deaths_IC, by = "date")
deaths_nice$deaths_nice <- round((deaths_nice$Overleden.x+deaths_nice$Overleden.y)*0.95,0)
deaths_nice <- deaths_nice %>%
  mutate(deaths_nice = c(0,diff(deaths_nice))) %>%
  mutate(Week = isoweek(date)) %>%
  mutate(Year = isoyear(date))

deaths_nice <- aggregate(deaths_nice ~ Week + Year, data = deaths_nice, FUN = sum)

## Deaths nursing homes
temp = tail(list.files(path = "data-rivm/nursing-homes-datasets/",pattern="*.csv.gz", full.names = T),1)
nursing.homes <- fread(temp)

nursing.homes$Date_of_statistic_reported <- as.Date(nursing.homes$Date_of_statistic_reported)
nursing.homes.deaths.wide <- aggregate(Total_deceased_reported ~ Date_of_statistic_reported, data = nursing.homes, FUN = sum)
nursing.homes.deaths.wide <- nursing.homes.deaths.wide %>%
  mutate(Week = isoweek(Date_of_statistic_reported)) %>%
  mutate(Year = isoyear(Date_of_statistic_reported))

week_deaths_nursery <- aggregate(Total_deceased_reported ~ Week + Year, data = nursing.homes.deaths.wide, FUN = sum)
colnames(week_deaths_nursery) <- c("Week","Year","deaths_nursing")
deaths_total <- merge(deaths_nice,week_deaths_nursery, by = c("Week","Year"))

## 70 plus living at home

## Deaths nursing homes
temp = tail(list.files(path = "data-rivm/70plus-living-at-home-per-day/",pattern="*.csv.gz", full.names = T),1)
living.home_70 <- fread(temp)

living.home_70$Date_of_statistic_reported <- as.Date(living.home_70$Date_of_statistic_reported)
living.home_70.wide <- aggregate(Total_deceased_reported ~ Date_of_statistic_reported, data = living.home_70, FUN = sum)
living.home_70.wide <- living.home_70.wide %>%
  mutate(Week = isoweek(Date_of_statistic_reported)) %>%
  mutate(Year = isoyear(Date_of_statistic_reported))

week_deaths_living_home_70 <- aggregate(Total_deceased_reported ~ Week + Year, data = living.home_70.wide, FUN = sum)
colnames(week_deaths_living_home_70) <- c("Week","Year","deaths_living_home_70")

deaths_total <- merge(deaths_total,week_deaths_living_home_70, by = c("Week","Year"), all.x=T)

## RIVM all deaths

df_deaths_rivm <- read.csv("corrections/deaths_perweek.csv")[,c("Week","Year","weekdeath_today")]
deaths_total <- merge(deaths_total, df_deaths_rivm, by = c("Week","Year"), all.x=T)
colnames(deaths_total) <- c("Week","Year","deaths_nice","deaths_nursing","deaths_living_home_70","deaths_rivm")
setorder(deaths_total, Year,Week)

deaths_total$deaths_nonnursing_RIVM <- deaths_total$deaths_rivm-deaths_total$deaths_nursing
deaths_total$deaths_nice_nursing <- deaths_total$deaths_nice + deaths_total$deaths_nursing

## Deaths excess DLM / CBS

excess_dlm <- read.csv("data-misc/excess_mortality/excess_mortality_dlm_2021.csv")[,c("week","year","deaths_week_mid")]
excess_dlm$deaths_week_mid <- round(excess_dlm$deaths_week_mid)
colnames(excess_dlm) <- c("Week","Year","total_covid_mortality")
deaths_total <- merge(deaths_total,excess_dlm,by=c("Week","Year"), all.x=T)

## Deaths WLZ vs. other / CBS
cbs_links <- read.csv("data-misc/excess_mortality/links_death_causes.csv")
cbs_url <- last(cbs_links)
page <- read_html(cbs_url[1,1])
page <- page %>% html_nodes("a") %>% html_attr('href')
page <- data.frame(page)

page$category <- grepl(".xlsx", page$page, fixed = TRUE)
page <- page %>%
  dplyr::filter(category == "TRUE")
cbs_url <- page[1,1]

download.file(cbs_url,destfile = "cbs_deaths.xlsx", mode = "wb")
cbs_oversterfte <- data.table(read_excel("cbs_deaths.xlsx", sheet = 4))
unlink("cbs_deaths.xlsx")
cbs_oversterfte <- cbs_oversterfte[5:(nrow(cbs_oversterfte)-5),c(1:2,4:5)]
colnames(cbs_oversterfte) <- c("Year","Week","other_deaths_perc","wlz_deaths_perc")
cbs_oversterfte <- mutate_all(cbs_oversterfte, function(x) parse_number(as.character(x)))
cbs_oversterfte <- cbs_oversterfte[complete.cases(cbs_oversterfte$wlz_deaths_perc)]

cbs_oversterfte[1:44,"Year"] <- 2020
cbs_oversterfte[45:96,"Year"] <- 2021
cbs_oversterfte[97:nrow(cbs_oversterfte),"Year"] <- 2022
cbs.death.statistics <- cbs_oversterfte



urls <- read.csv("data-misc/excess_mortality/links_cbs_mortality.csv")

cbs_url <- last(urls)
page <- read_html(cbs_url[1,1])
page <- page %>% html_nodes("a") %>% html_attr('href')
page <- data.frame(page)

page$category <- grepl(".xlsx", page$page, fixed = TRUE)
page <- page %>%
  dplyr::filter(category == "TRUE")
cbs_url <- page[1,1]

download.file(cbs_url,destfile = "cbs_deaths.xlsx", mode = "wb")

#### Parse WLZ mortality data ####
urls <- read.csv("data-misc/excess_mortality/links_cbs_mortality.csv")

cbs_url <- last(urls)
page <- read_html(cbs_url[1,1])
page <- page %>% html_nodes("a") %>% html_attr('href')
page <- data.frame(page)

page$category <- grepl(".xlsx", page$page, fixed = TRUE)
page <- page %>%
  dplyr::filter(category == "TRUE")
cbs_url <- page[1,1]

download.file(cbs_url,destfile = "cbs_deaths.xlsx", mode = "wb")


cbs_sterfte <- data.table(read_excel("cbs_deaths.xlsx", sheet = 6))[,c(1,2,5,10)]
cbs_sterfte <- cbs_sterfte[6:nrow(cbs_sterfte),]
#cbs_sterfte <- cbs_sterfte[c(1:4)]

## Select indices ##
test <- row_count(cbs_sterfte, count = NA)
rows.2020 <- which(test$rowcount == 4)[3]
rows.2021 <- which(test$rowcount == 4)[2]
rows.2022 <- which(test$rowcount == 4)[1]

sterfte.2022 <- cbs_sterfte[(rows.2022+1):(rows.2021-1),]
sterfte.2022 <- sterfte.2022[-((rows.2021-4):(rows.2021-3)),]
colnames(sterfte.2022) <- c("Jaar","Week","Sterfte_Wlz","Sterfte_Other")
sterfte.2022 <- sterfte.2022 %>%
  mutate(Week = (1:nrow(sterfte.2022))) %>%
  mutate(Jaar = 2022)
setDF(sterfte.2022)

sterfte.2021 <- cbs_sterfte[(rows.2021+1):(rows.2020-1),]
colnames(sterfte.2021) <- c("Jaar","Week","Sterfte_Wlz","Sterfte_Other")
sterfte.2021 <- sterfte.2021 %>%
  mutate(Week = parse_number(Week)) %>%
  mutate(Jaar = 2021)
setDF(sterfte.2021)

sterfte.2020 <- cbs_sterfte[(rows.2020+2):(nrow(cbs_sterfte)-7),]
colnames(sterfte.2020) <- c("Jaar","Week","Sterfte_Wlz","Sterfte_Other")
sterfte.2020 <- sterfte.2020 %>%
  mutate(Week = parse_number(Week)) %>%
  mutate(Jaar = 2020)
setDF(sterfte.2020)
sterfte.2020["Week"][sterfte.2020["Week"] == 533] <- 53

sterfte_wlz_other <- rbind(sterfte.2020,sterfte.2021,sterfte.2022)




#### Sterfte verwacht ####
cbs_sterfte <- data.table(read_excel("cbs_deaths.xlsx", sheet = 7))[8:166,c(1,5,8)]
colnames(cbs_sterfte) <- c("week_year","wlz_verwacht","other_verwacht")
unlink("cbs_deaths.xlsx")

cbs_sterfte$Jaar <- parse_number(substr(cbs_sterfte$week_year, 1, 4))
cbs_sterfte$Week <- parse_number(substr(cbs_sterfte$week_year, 11, 12))
cbs_sterfte$wlz_verwacht <- parse_number(cbs_sterfte$wlz_verwacht)
cbs_sterfte$other_verwacht <- parse_number(cbs_sterfte$other_verwacht)
setDF(cbs_sterfte)

cbs.death.statistics.week <- merge(sterfte_wlz_other, cbs_sterfte,by=c("Week","Jaar"))
cbs.death.statistics.week <- cbs.death.statistics.week %>%
  dplyr::select(Week, Jaar, Sterfte_Wlz, Sterfte_Other)

#### Merge CBS mortality data ####

colnames(cbs.death.statistics.week) <- c("Week","Year","wlz_deaths","other_deaths")
cbs.death.statistics.week <- data.frame(cbs.death.statistics.week)
cbs.death.statistics.week["Week"][cbs.death.statistics.week["Week"] == 532] <- 53

cbs.df <- merge(cbs.death.statistics,cbs.death.statistics.week, by = c("Week","Year"))
cbs.df <- cbs.df %>%
  mutate(wlz_deaths = parse_number(wlz_deaths)) %>%
  mutate(other_deaths = parse_number(other_deaths)) %>%
  mutate(wlz_covid = round(wlz_deaths*wlz_deaths_perc,0)) %>%
  mutate(other_covid = round(other_deaths*other_deaths_perc,0)) %>%
  dplyr::select(Year, Week, wlz_covid, other_covid) %>%
  arrange(Year, Week)
rm(cbs.death.statistics, cbs.death.statistics.week,urls)

deaths_total <- merge(deaths_total,cbs.df,by=c("Week","Year"), all.x=T)

setorder(deaths_total, Year, Week)

deaths_total$excess_rivm_nice <- deaths_total$deaths_nice-deaths_total$deaths_nonnursing_RIVM
deaths_total$week_year <- ifelse(deaths_total$Week<10,
                                 paste0(deaths_total$Year,"-",0,deaths_total$Week),
                                 paste0(deaths_total$Year,"-",deaths_total$Week))

deaths_total <- deaths_total %>% 
  mutate(deaths_home = total_covid_mortality - deaths_nice - wlz_covid) %>%
  mutate(deaths_home_perc = round(deaths_home/total_covid_mortality*100,3)) %>%
  mutate(deaths_nice_perc = round(deaths_nice/total_covid_mortality*100,3)) %>%
  mutate(deaths_wlz_perc = round(wlz_covid/total_covid_mortality*100,3)) %>%
  mutate(deaths_estimate = round(deaths_nonnursing_RIVM*1.8+deaths_nursing*1.8,0)) %>%
  mutate(deaths_estimate_2 = round((deaths_nice + deaths_nursing*1.8)*1.1,0)) %>%
  mutate(deaths_estimate_3 = round(deaths_nice + (deaths_nice/3) + deaths_nursing*1.8,0)) %>%
  mutate(cbs_rivm_factor = round(total_covid_mortality/deaths_rivm,2)) %>%
  mutate(factor_wlz = round(wlz_covid/deaths_nursing,2)) %>%
  mutate(factor_other = round(other_covid/deaths_nonnursing_RIVM,2)) %>%
  mutate(deviation_estim = round((deaths_estimate_3-total_covid_mortality)/total_covid_mortality*100,0)) %>%
  mutate(average_covid_mortality = (total_covid_mortality + deaths_estimate_3)/2)

write.csv(deaths_total, file = "corrections/death_week_comparisons.csv", row.names = F)

rm(deaths_clinic, deaths_IC,deaths_nice,df_deaths_rivm,excess_dlm,nursing.homes,nursing.homes.deaths.wide,
   week_deaths_nursery, living.home_70, living.home_70.wide,week_deaths_living_home_70,temp, cbs.df)

#### Week filter for CBS death causes ####

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

week.number.cbs <- cbs.death.statistics %>%
  dplyr::filter(!is.na(`2022_covid`))

period.death_causes.filter <- paste0("2022-",max(week.number.cbs$Week))

## Determine last month ##

month.deaths <- sub(".*doodsoorzaken-", "", cbs_url[1,1])

month.deaths <- gsub("[[:digit:]]", "", month.deaths) 
month.deaths <- gsub("-", "", month.deaths) 

#### PLOTS ####

cols <- c("#009E73", "#87109A","#E6830C","#D96DEA", "#2231C5","#000000")


plot <- deaths_total %>%
  dplyr::filter(week_year <= period.death_causes.filter) %>%
  ggplot(aes(x=factor(week_year), y=deaths_rivm, group = 1)) + 
  geom_line(aes(y = deaths_rivm, color = "RIVM"), lwd=1.2) +
  geom_line(aes(y = total_covid_mortality, color = "CBS"), lwd=1.2) +
  theme_classic()+
  xlab("")+
  ylab("")+
  labs(title = "Sterfte per week",
       subtitle = paste0("CBS data beschikbaar t/m ",month.deaths," 2022"),
       caption = paste("Bron: CBS/RIVM | Plot: @mzelst  | ",Sys.Date())) +
  theme(
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"),
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title =     element_text(hjust = 0.5 ,size = 24 ,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5   ,size = 12 ,color = "black", face = "italic"),
    axis.text.x = element_text(size=10,color = "black",face = "bold", angle = 90),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())


plot + scale_colour_manual(values = cols)
ggsave("plots/sterfte_per_week_30K.png", width = 12, height=8)

## Percentages

plot <- deaths_total %>%
  dplyr::filter(week_year >= "2020-39") %>%
  dplyr::filter(week_year <= period.death_causes.filter) %>%
  ggplot(aes(x=factor(week_year), y=deaths_wlz_perc, group = 1)) + 
  geom_line(aes(y = deaths_wlz_perc, color = "Verpleeghuis"), lwd=1.2) +
  geom_line(aes(y = deaths_home_perc, color = "Thuis"), lwd=1.2) +
  geom_line(aes(y = deaths_nice_perc, color = "Ziekenhuis"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), n.breaks = 10) +
  theme_classic()+
  xlab("")+
  ylab("")+
  labs(title = "Sterfte per plaats van overlijden",
       subtitle = "Percentage van totale sterfte",
       caption = paste("Bron: CBS/RIVM/NICE | Plot: @mzelst  | ",Sys.Date())) +
  theme(
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"),
    legend.text = element_text(size = 12),
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title =     element_text(hjust = 0.5 ,size = 24 ,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5   ,size = 12 ,color = "black", face = "italic"),
    axis.text.x = element_text(size=10,color = "black",face = "bold", angle = 90),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

plot + scale_colour_manual(values = cols)
ggsave("plots/sterfte_per_week_30K_percentage.png", width = 12, height=8)

## Percentages

deaths_total <- deaths_total[-c(nrow(deaths_total)),]

plot <- deaths_total %>%
  dplyr::filter(week_year >= "2020-39") %>%
  ggplot(aes(x=factor(week_year), y=deaths_nursing, group = 1)) + 
  geom_line(aes(y = deaths_nursing, color = "Verpleeghuis"), lwd=1.2) +
  geom_line(aes(y = deaths_nice, color = "Ziekenhuis"), lwd=1.2) +
  geom_line(aes(y = deaths_rivm, color = "Totaal (RIVM)"), lwd=1.2) +
  geom_line(aes(y = average_covid_mortality, color = "Totaal (Schatting)"), lwd=1.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1200), n.breaks = 10) +
  theme_classic()+
  xlab("")+
  ylab("")+
  labs(title = "Sterfte per groep",
       subtitle = "Geschatte sterfte is een mix van meerdere methoden",
       caption = paste("Bron: CBS/RIVM/NICE | Plot: @mzelst  | ",Sys.Date())) +
  theme(
    legend.title = element_blank(),  ## legend title
    legend.position="top",
    legend.direction = "horizontal",
    legend.background = element_rect(fill="#f5f5f5", size=0.5, linetype="solid"),
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "#F5F5F5"), #background color/size (border color and size)
    panel.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
    plot.title =     element_text(hjust = 0.5 ,size = 24 ,face = "bold"),
    plot.subtitle =  element_text(hjust=0.5   ,size = 12 ,color = "black", face = "italic"),
    axis.text.x = element_text(size=10,color = "black",face = "bold", angle = 90),
    axis.ticks = element_line(colour = "#F5F5F5", size = 1, linetype = "solid"),
    axis.ticks.length = unit(0.5, "cm"),
    axis.line = element_line(colour = "#F5F5F5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

plot + scale_colour_manual(values = cols)
ggsave("plots/sterfte_per_week_30K_totalen.png", width = 12, height=8)


##cbs.deaths
total.covid.mortality.cbs <- deaths_total %>%
  dplyr::filter(week_year <= period.death_causes.filter)
cbs.deaths <- sum(total.covid.mortality.cbs$total_covid_mortality,na.rm=T)


cbs.filter <- deaths_total %>%
  dplyr::filter(week_year > period.death_causes.filter)
cbs.filter$cumulative_deaths <- cumsum(cbs.filter$deaths_estimate_3) + cbs.deaths
deaths_total <- merge(deaths_total, cbs.filter[,c("Week","Year","cumulative_deaths")], by = c("Week","Year"),all.x=T)
setorder(deaths_total, Year, Week)



write.csv(deaths_total, file = "corrections/death_week_comparisons.csv", row.names = F)

rm(plot, cols, cbs.deaths, cbs.death.statistics,webpage.cbs, u.cbs)

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Update death comparison tracker for Twitter thread (Friday update - part 2)"))
push(repo, credentials = git.auth)