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
tweet.main <- paste0("CBS heeft het aantal overlijdensgevallen bijgewerkt t/m week ",thisweek," van 2021. In week 44 overleden er ",last(excess_mortality$Totaal_Overleden)," mensen. Dat is ",perc_excess_text,"

In dit draadje duid ik de sterfte per week + het aantal mensen dat is overleden door corona.")

posted_tweet <- post_tweet (
  tweet.main,
  token = token.mzelst,
  media = "data-misc/excess_mortality/plots_weekly_update/sterfte_perweek.png"
) ## Post tweet
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

tweet.information <- paste0("2/ 

Ik kijk naar:

1) Oversterfte adhv historische gemiddeldes (tweet 3/4)
2) Sterfte door corona met een dynamisch linear model (DLM) (tweet 5)
3) Sterfte door corona met model gebaseerd op plaats van overlijden (tweet 6)

De uitleg kun je hier vinden: https://github.com/mzelst/covid-19/blob/master/data-misc/excess_mortality/REMARKS.md")

posted_tweet <- post_tweet (
  tweet.information,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Build excess mortality (historical) tweet
excess_mortality <- read.csv("data-misc/excess_mortality/excess_mortality.csv")

tweet.excess.historical <- paste0("3/ De oversterfte in week ",thisweek," (",startday.week, " november"," - ",endday.week," november):

1) Methode CBS: ",last(excess_mortality$excess_cbs_method),"
2) Methode RIVM (",rivm.startday," oktober - ",rivm.endday," november): ",last(excess_mortality$excess_mortality_rivm),"

(grafieken CBS / RIVM)
")

posted_tweet <- post_tweet (
  tweet.excess.historical,
  token = token.mzelst,
  media = c("data-misc/excess_mortality/plots_weekly_update/overledenen-per-week.png","data-misc/excess_mortality/plots_weekly_update/sterfte_perweek_rivm.jpeg"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Tweet WLZ mortality
cbs_links <- read.csv("data-misc/excess_mortality/links_cbs_mortality.csv")
cbs_url <- last(cbs_links)
page <- read_html(cbs_url[1,1])
page <- page %>% html_nodes("a") %>% html_attr('href')
page <- data.frame(page)

page$category <- grepl(".xlsx", page$page, fixed = TRUE)
page <- page %>%
  filter(category == "TRUE")
cbs_url <- page[1,1]

download.file(cbs_url,destfile = "cbs_deaths.xlsx", mode = "wb")
cbs_oversterfte <- data.table(read_excel("cbs_deaths.xlsx", sheet = 7))[,1:10]
unlink("cbs_deaths.xlsx")
cbs_oversterfte <- cbs_oversterfte[8:nrow(cbs_oversterfte),]
cbs_oversterfte <- cbs_oversterfte[-54,]
cbs_oversterfte <- cbs_oversterfte[-c(106:109),]
colnames(cbs_oversterfte) <- c("Periode","deaths_expected_cbs","verwacht_lb","Verwacht_ub","Verwacht_WLZ","verwacht_WLZ_lb","Verwacht_WLZ_ub",
                               "Verwacht_other","Verwacht_other_lb","Verwacht_other_ub")

cbs_oversterfte$Year <- parse_number(cbs_oversterfte$Periode)
cbs_oversterfte$Week <- c(1:53,1:52)

download.file(cbs_url,destfile = "cbs_deaths.xlsx", mode = "wb")
wlz_sterfte <- data.table(read_excel("cbs_deaths.xlsx", sheet = 6))[-c(1:6),c(1:2,5,10)]
unlink("cbs_deaths.xlsx")

colnames(wlz_sterfte) <- c("Year","Week","WLZ_total","Other_total")

wlz_sterfte <- wlz_sterfte %>%
  mutate(Year = 2021) %>%
  mutate(Week = parse_number(Week)) %>%
  mutate(WLZ_total = parse_number(WLZ_total)) %>%
  mutate(Other_total = parse_number(Other_total))

thisweek <- isoweek(Sys.Date())-2
end_file_week <- isoweek(Sys.Date())+1

wlz_sterfte <- wlz_sterfte[c(1:thisweek,end_file_week),]
wlz_sterfte[nrow(wlz_sterfte),"Week"] <- isoweek(Sys.Date())-1

sterfte_wlz_other <- merge(cbs_oversterfte, wlz_sterfte, by = c("Year","Week"), all.x=T)

sterfte_wlz_other <- sterfte_wlz_other %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(excess_wlz_perc = round((WLZ_total-Verwacht_WLZ)/Verwacht_WLZ*100,1)) %>%
  mutate(excess_other_perc = round((Other_total-Verwacht_other)/Verwacht_other*100,1)) %>%
  select(-c("Periode"))

table.wlz <- sterfte_wlz_other[complete.cases(sterfte_wlz_other),]

wlz.text <- ifelse(last(table.wlz$excess_wlz_perc)<0,"minder","meer")
other.text <- ifelse(last(table.wlz$excess_other_perc)<0,"minder","meer")

tweet.wlz <- paste0("4/ Oversterfte Wlz en overige bevolking (CBS)

De sterfte bij Wlz-gebruikers (mensen in zorginstellingen) is ",abs(last(table.wlz$excess_wlz_perc)),"% ",wlz.text," dan verwacht.

De sterfte in de overige bevolking is ",abs(last(table.wlz$excess_other_perc)),"% ",other.text," dan verwacht.
")

posted_tweet <- post_tweet (
  tweet.wlz,
  token = token.mzelst,
  media = c("data-misc/excess_mortality/plots_weekly_update/overledenen-per-week-wlz.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

thisweek <- isoweek(Sys.Date())-1

## Tweet DLM analyses

tweet.dlm <- paste0("5/ Het officiële aantal sterfgevallen voor week ",thisweek," is op dit moment ",last(excess_mortality$covid_sterfgevallen),".

De DLM methode schat het aantal corona-overledenen voor week ",thisweek," op ",last(excess_mortality$DLModel_week_estimate)," (95% betrouwbaarheidsinterval: ",last(excess_mortality$DLModel_lowerbound95),"-",last(excess_mortality$DLModel_upperbound95),").

De sterfte door corona tot nu toe is ",last(excess_mortality$Oversterfte_DLModel_cumul_mid)," (95% betrouwbaarheidsinterval: ",last(excess_mortality$Oversterfte_DLModel_cumul_low),"-",last(excess_mortality$Oversterfte_DLModel_cumul_high),").
")

posted_tweet <- post_tweet (
  tweet.dlm,
  token = token.mzelst,
  media = c("plots/2021_fig4.2.1.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Tweet Methode Marino analyses

deaths.comparison.tracker <- read.csv("corrections/death_week_comparisons.csv")
deaths.comparison.tracker <- deaths.comparison.tracker %>%
  filter(Year == 2021 & Week > 25 & Week <= thisweek)

cbs.deaths <- sum(excess_mortality$Covid_deaths_CBS_death_statistics,na.rm=T)
est.deaths <- sum(deaths.comparison.tracker$deaths_estimate_3)

tweet.newmodel <- paste0("6/ Een andere methode om de sterfte door corona te schatten beschreef ik in dit draadje: https://twitter.com/mzelst/status/1390682590105985026

Het aantal sterfgevallen in week ",thisweek," aan de hand van deze methode is ",last(deaths.comparison.tracker$deaths_estimate_3),".

De sterfte door corona tot nu toe is dan ",cbs.deaths+est.deaths, ".
")

posted_tweet <- post_tweet (
  tweet.newmodel,
  token = token.mzelst,
  media = c("plots/sterfte_per_week_30K_totalen.png"),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str


## Conclusie tweet

conclusie.tweet <- paste0("Conclusie: De sterfte is de afgelopen weken hoger dan verwacht en ligt bijna volledig in lijn met de sterfte in 2020 in dezelfde periode. De oversterfte komt vrijwel compleet door de twee oudste leeftijdsgroepen. Een deel wordt waarschijnlijk ook verklaard door uitgestelde zorg.")

posted_tweet <- post_tweet (
  conclusie.tweet,
  token = token.mzelst,
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

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