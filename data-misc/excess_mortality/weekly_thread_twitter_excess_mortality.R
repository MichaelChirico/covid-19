source("workflow/twitter/token_mzelst.R")
#source("workflow/excess_mortality_cbsmodel_2021.R")
#source("workflow/excess_mortality.R")

thisweek <- isoweek(Sys.Date())-1
startday.week <- substr(Sys.Date()-11,9,10)
endday.week <- substr(Sys.Date()-5,9,10)
rivm.startday <- substr(Sys.Date()-15,9,10)
rivm.endday <- substr(Sys.Date()-9,9,10)

excess_mortality <- read.csv("data-misc/excess_mortality/excess_mortality.csv")

meer.minder <- ifelse(last(excess_mortality$percent_excess)>0,"meer","minder")

perc_excess_text <- paste0(last(excess_mortality$percent_excess),"% ",meer.minder," dan verwacht.")

## Build main tweet
tweet.main <- paste0("CBS heeft het aantal overlijdensgevallen bijgewerkt t/m week ",thisweek," van 2022. In week ",thisweek," overleden er ",last(excess_mortality$Totaal_Overleden)," mensen. Dat is ",perc_excess_text,"

Een korte draad over de sterfte per week en sterfte door corona.")

posted_tweet <- post_tweet (
  tweet.main,
  token = token.mzelst,
  media = "data-misc/excess_mortality/plots_weekly_update/sterfte_perweek.png",
  media_alt_text = "Oversterfte")

posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.main.id <- posted_tweet$id_str
tweet.last_id <- tweet.main.id

## Build mededeling tweet ##

#tweet.mededeling <- paste0("1/

#De sterfte is weer op het normale niveau en de sterfte door covid is op dit moment vrij laag. Dit is dus voorlopig de laatste oversterfte update. Mocht de sterfte weer sterk afwijken van de verwachting, plaats ik de update opnieuw.")

#posted_tweet <- post_tweet (
#  tweet.mededeling,
#  token = token.mzelst,
#  in_reply_to_status_id = tweet.main.id,
#  auto_populate_reply_metadata = TRUE
#)
#posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
#tweet.last_id <- posted_tweet$id_str

## Build information tweet
#3) Sterfte door corona met model gebaseerd op plaats van overlijden (tweet 6)
#tweet.information <- paste0("2/ 
#
#Ik kijk naar:
#
#1) Oversterfte adhv historische gemiddeldes (tweet 3/4)
#2) Sterfte door corona met een dynamisch linear model (DLM) (tweet 5)
#2) Sterfte door corona met een fitted model obv CBS, NICE, en RIVM (tweet 6)
#
#
#De uitleg kun je hier vinden: https://github.com/mzelst/covid-19/blob/master/data-misc/excess_mortality/REMARKS.md")
#
#posted_tweet <- post_tweet (
#  tweet.information,
#  token = token.mzelst,
#  in_reply_to_status_id = tweet.last_id,
#  auto_populate_reply_metadata = TRUE
#)
#posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
#tweet.last_id <- posted_tweet$id_str

## Build excess mortality (historical) tweet
excess_mortality <- read.csv("data-misc/excess_mortality/excess_mortality.csv")
month.names <- date_names_lang(language = "nl")
which.month <- month(ymd(Sys.Date()))
this.month <- month.names$mon[which.month]




tweet.excess.historical <- paste0("2/ De oversterfte in week ",thisweek," (",startday.week," ",this.month," - ",endday.week," ",this.month,"):

1) Methode CBS: ",last(excess_mortality$excess_cbs_method),"
2) Methode RIVM (",rivm.startday," ",this.month," - ",rivm.endday," ",this.month,"): ",round(last(excess_mortality$excess_mortality_rivm)),"

(grafieken CBS / RIVM)
")

posted_tweet <- post_tweet (
  tweet.excess.historical,
  token = token.mzelst,
  media = c("data-misc/excess_mortality/plots_weekly_update/overledenen-per-week.png","data-misc/excess_mortality/plots_weekly_update/sterfte_perweek_rivm.jpeg"),
  media_alt_text = c("Oversterfte - Leeftijdsgroep","Oversterfte - RIVM"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

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

cbs_sterfte <- cbs_sterfte %>%
  mutate(Jaar = parse_number(substr(week_year, 1, 4))) %>%
  mutate(Week = parse_number(substr(week_year, 11, 12))) %>%
  mutate(week_year = NULL) %>%
  mutate_all(function(x) parse_number(as.character(x)))

cbs_deaths <- merge(sterfte_wlz_other, cbs_sterfte,by=c("Week","Jaar"))
wlz.table <- cbs_deaths %>%
  arrange(Jaar,Week) %>%
  mutate_all(function(x) parse_number(as.character(x)))

excess_wlz_perc <- round(last(wlz.table$Sterfte_Wlz)/last(wlz.table$wlz_verwacht)*100-100,1)
excess_other_perc <- round(last(wlz.table$Sterfte_Other)/last(wlz.table$other_verwacht)*100-100,1)


wlz.text <- ifelse(excess_wlz_perc<0,"minder","meer")
other.text <- ifelse(excess_other_perc<0,"minder","meer")

#source("data-misc/excess_mortality/plots_weekly_update/plots_excess_mortality_wlz_age.R")

tweet.wlz <- paste0("3/ Oversterfte Wlz en overige bevolking (CBS)

De sterfte bij Wlz-gebruikers (mensen in zorginstellingen) is ",abs(excess_wlz_perc),"% ",wlz.text," dan verwacht.

De sterfte in de overige bevolking is ",abs(excess_other_perc),"% ",other.text," dan verwacht.
")

posted_tweet <- post_tweet (
  tweet.wlz,
  token = token.mzelst,
  media = c("data-misc/excess_mortality/plots_weekly_update/overledenen-per-week-wlz.png"),
  media_alt_text = "Oversterfte - WLZ-gebruikers",
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

thisweek <- isoweek(Sys.Date())-1

## Tweet DLM analyses

tweet.dlm <- paste0("4/ Het officiële aantal sterfgevallen voor week ",thisweek," is op dit moment ",last(excess_mortality$covid_sterfgevallen),".

De DLM methode schat het aantal corona-overledenen voor week ",thisweek," op ",last(excess_mortality$DLModel_week_estimate)," (95% betrouwbaarheidsinterval: ",last(excess_mortality$DLModel_lowerbound95),"-",last(excess_mortality$DLModel_upperbound95),").

De sterfte door corona tot nu toe is ",last(excess_mortality$Oversterfte_DLModel_cumul_mid)," (95% betrouwbaarheidsinterval: ",last(excess_mortality$Oversterfte_DLModel_cumul_low),"-",last(excess_mortality$Oversterfte_DLModel_cumul_high),").
")

posted_tweet <- post_tweet (
  tweet.dlm,
  token = token.mzelst,
  media = c("plots/2021_fig4.2.1.png"),
  media_alt_text = "Sterfte door corona (DLM)",
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Tweet Methode Marino analyses

deaths.comparison.tracker <- read.csv("corrections/death_week_comparisons.csv")

tweet.newmodel <- paste0("5/ Een andere methode om de sterfte door corona te schatten beschreef ik in dit draadje: https://twitter.com/mzelst/status/1390682590105985026

Het aantal sterfgevallen in week ",thisweek," aan de hand van deze methode is ",last(deaths.comparison.tracker$deaths_estimate_3),".

De sterfte door corona tot nu toe is dan ",last(deaths.comparison.tracker$cumulative_deaths), ".
")

posted_tweet <- post_tweet (
  tweet.newmodel,
  token = token.mzelst,
  media = c("plots/sterfte_per_week_30K_totalen.png"),
  media_alt_text = "Sterfte door corona (schatting)",
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Provincie - oversterfte
excess_mort_province <- read.csv("data-misc/excess_mortality/excess_mortality_provinces_clean.csv")

excess_mort_province_filtered <- excess_mort_province %>%
  dplyr::filter(Week == thisweek) %>%
  dplyr::filter(Jaar == 2022)

excess_province_long <- gather(excess_mort_province_filtered, "statnaam","excess_mortality",3:14)
excess_province_long$excess_mortality <- round(excess_province_long$excess_mortality,0)
excess_province_long$statnaam <- recode(excess_province_long$statnaam, "Noord.Holland" = "Noord-Holland",
                                        "Zuid.Holland" = "Zuid-Holland",
                                        "Noord.Brabant" = "Noord-Brabant",
                                        "Fryslan" = "Fryslân")

high.prov.mort <- max(excess_province_long$excess_mortality)
highest.province <- excess_province_long %>%
  dplyr::filter(excess_mortality == high.prov.mort)

tweet.provincie <- paste0("6/ De relatieve oversterfte was afgelopen week het hoogste in ",highest.province[,"statnaam"],": ",high.prov.mort,"%.")

posted_tweet <- post_tweet (
  tweet.provincie,
  token = token.mzelst,
  media = c("data-misc/excess_mortality/plots_weekly_update/oversterfte_provincie.png"),
  media_alt_text = "Oversterfte per provincie",
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str


## Conclusie tweet

conclusie.tweet <- paste0("Conclusie: De hele maand juni was er significante oversterfte, vooral bij de groep 65+ (laatste week ook bij 0-64).

Waarschijnlijk wordt dit mede veroorzaakt door de huidige coronagolf, uitgestelde zorg, en mogelijk door een paar zeer warme dagen.")

posted_tweet <- post_tweet (
  conclusie.tweet,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Verwachting - deel 2

verwachting.tweet_2 <- paste0("Het is lastig om te bepalen waar het nu heen gaat maar voorlopig is de verwachting dat de oversterfte wat zal dalen, in lijn met de dalende Deltagolf.

In welke mate de boosters en/of de Omicron-variant hier op korte termijn veranderingen in zullen brengen is nog even afwachten.")

posted_tweet <- post_tweet (
  verwachting.tweet_2,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Disclaimer

#disclaimer.tweet <- "Dit moet ik er blijkbaar elke keer weer bij zetten: ja, de vaccins werken goed tegen sterfte. Nee, ze zijn niet perfect en dus voorkomen ze niet alle sterfgevallen.

#Maar met tienduizenden besmettingen per dag sterven er alsnog vele mensen, zelfs met een uitermate goed vaccin."

#posted_tweet <- post_tweet (
#  disclaimer.tweet,
#  token = token.mzelst,
#  in_reply_to_status_id = tweet.last_id,
#  auto_populate_reply_metadata = TRUE
#)
#posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
#tweet.last_id <- posted_tweet$id_str

## Eindnoot tweet

eindnoot.tweet <- paste0("Eindnoot

In onze repo houden we wekelijks deze analyses allemaal bij, zie hier: https://github.com/mzelst/covid-19/tree/master/workflow/excess_mortality")

posted_tweet <- post_tweet (
  eindnoot.tweet,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Eindnoot tweet 2

eindnoot.tweet2 <- paste0("Eindnoot 2

De verschillen tussen de oversterfte cijfers van het CBS en RIVM leg ik hieronder uit. We spraken hier ook over met @rubenivangaalen die dat in detail uitlegt: https://signaalwaarde.nl/aflevering-6/

https://twitter.com/mzelst/status/1365703708579872775")

posted_tweet <- post_tweet (
  eindnoot.tweet2,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str