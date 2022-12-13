memory.limit(size = 64000)

remove(list = ls())
source("workflow/twitter/token_mzelst.R")

#time.start <- ymd_hms(paste0(Sys.Date()+1," 14:00:00"))
time.start <- ymd_hms(paste0(Sys.Date()," 14:00:00"))

## Put in double date breaker for NICE update
repeat {
  Sys.sleep(30)
  time.now <- ymd_hms(Sys.time())
  if (time.start < time.now){
    message <- "GO GO GO GO GO"
    break
  }
}

pull(repo)

# Generate Banner
source("workflow/generate_banner.R")
source("workflow/parse_nice-data.R")
source("workflow/dashboards/age-distribution-date-NICE.R")
source("workflow/dashboards/nice_bezetting_onder20.R")
source("plot_scripts/reden_opname.R")
source("workflow/parse_opnameduur_ntvg.R")

## Put in double date breaker for LCPS update
repeat {
  Sys.sleep(2)
  time.start <- ymd_hms(paste0(Sys.Date()," 14:01:00"))
  time.now <- ymd_hms(Sys.time())
  
  if (time.start < time.now){
    message <- "GO GO GO GO GO"
    break
  }
}

repo <- git2r::init()
pull(repo)
source("workflow/parse_lcps-data.R")
source("plot_scripts/ziekenhuis_plots.R")

## Put in double date breaker for RIVM update
repeat {
  Sys.sleep(3)
  date.check <- fread("https://data.rivm.nl/covid-19/COVID-19_uitgevoerde_testen.csv")
  
  date.check <- date.check %>%
    mutate(
      date = as.Date(Date_of_report, tryFormats = c('%d-%m-%Y')),
      .before = Date_of_report
    ) %>%
    mutate(
      Date_of_report = NULL
    )
  
  date.check.update <- last(date.check$date)
  if (date.check.update == as.Date(Sys.Date())){
    message <- "GO GO GO GO GO"
    break
  }
}


#rivm.by_day <- read.csv("data/rivm_by_day.csv")

# Verify RIVM data has been downloaded, otherwise stop script.
#condition <- Sys.Date()!=as.Date(last(rivm.by_day$date))

#if (condition) {stop("The value is TRUE, so the script must end here")    
#} else { 
tic()
# Parse RIVM, NICE and corrections data
source("workflow/parse_rivm-data.R")
source("workflow/parse_nursing-homes.R")
source("workflow/parse_tests.R")
source("workflow/parse_corrections.R")

## Set locale
Sys.setlocale("LC_TIME", "nl_NL")

## Merge RIVM, NICE and corrections data

rivm.by_day <- fread("data/rivm_by_day.csv")  
nice.by_day <- fread("data-nice/nice-today.csv")
lcps.by_day <- fread("data/lcps_by_day.csv")
corr.by_day <- fread("corrections/corrections_perday.csv")
nursery.by_day <- fread("data/nursery_by_day.csv")
testrate.by_day <- fread("data-dashboards/percentage-positive-daily-national.csv")[,c("values.tested_total","values.infected","values.infected_percentage","date","pos.rate.3d.avg","pos.rate.7d.avg")]
#vaccines.by_day <- read.csv("data/vaccines_by_day.csv") , vaccines.by_day

daily_datalist <- list(lcps.by_day,nice.by_day,rivm.by_day,corr.by_day,nursery.by_day, testrate.by_day)

all.data <- Reduce(
  function(x, y, ...) merge(x, y, by="date",all.x = TRUE, ...),
  daily_datalist
)

write.csv(all.data, file = "data/all_data.csv",row.names = F)

# get tokens
source("workflow/twitter/token_mzelst.R")
#source("workflow/twitter/token_edwinveldhuizen.R")

LCPS_klinisch_two_days <- last(all.data$Kliniek_Bedden_Nederland,2)
LCPS_Verpleeg_Huidig_Toename <- LCPS_klinisch_two_days[2] - LCPS_klinisch_two_days[1]
LCPS_IC_two_days <- last(all.data$IC_Bedden_COVID_Nederland,2)
LCPS_IC_Huidig_Toename <- LCPS_IC_two_days[2] - LCPS_IC_two_days[1]
LCPS_IC_Int_two_days <- last(all.data$IC_Bedden_COVID_Internationaal,2)
LCPS_IC_Int_Huidig_Toename <- LCPS_IC_Int_two_days[2] - LCPS_IC_Int_two_days[1]

sign.hosp.lcps <- paste0(ifelse(LCPS_Verpleeg_Huidig_Toename>=0," (+"," ("))
sign.ic.lcps <- paste0(ifelse(LCPS_IC_Huidig_Toename>=0," (+"," ("))
sign.ic.int.lcps <- paste0(ifelse(LCPS_IC_Int_Huidig_Toename>=0," (+"," ("))

Kliniek_Nieuwe_Opnames <- ifelse(is.na(last(all.data$kliniek_opnames_covid)),"Onbekend",last(all.data$kliniek_opnames_covid))
Kliniek_Aanwezig <- ifelse(is.na(last(all.data$Kliniek_Bedden_Nederland)),"Onbekend",paste0(format(last(all.data$Kliniek_Bedden_Nederland),decimal.mark = ",",big.mark =".",big.interval = 3),sign.hosp.lcps,LCPS_Verpleeg_Huidig_Toename))
IC_Nieuwe_Opnames <- ifelse(is.na(last(all.data$IC_opnames_covid)),"Onbekend",last(all.data$IC_opnames_covid))
IC_Aanwezig <- ifelse(is.na(last(all.data$IC_Bedden_COVID_Nederland)),"Onbekend",paste0(last(all.data$IC_Bedden_COVID_Nederland),sign.ic.lcps,LCPS_IC_Huidig_Toename))
IC_Aanwezig_Int <- ifelse(is.na(last(all.data$IC_bezetting_covid_internationaal)),"Onbekend",paste0(last(all.data$IC_bezetting_covid_internationaal),sign.ic.int.lcps,LCPS_IC_Int_Huidig_Toename))

total.deaths <- last(all.data$deaths,5)
total.deaths <- last(total.deaths[!is.na(total.deaths)],2)

tweet.main <- paste0("#COVID19NL

Opgenomen: ",Kliniek_Nieuwe_Opnames,"
Huidig: ",Kliniek_Aanwezig,")

Opgenomen op IC: ",IC_Nieuwe_Opnames,"
Huidig IC Nederland: ",IC_Aanwezig,")

Overleden: ",total.deaths[2]-total.deaths[1],"
Totaal: ",format(last(all.data$deaths),decimal.mark = ",",big.mark =".",big.interval = 3),"")

tweet.main

posted_tweet <- post_tweet (
  tweet.main,
  token = token.mzelst,
  media = c("plots/opnames_per_dag_kliniek.png",
            "plots/opnames_per_dag_ic.png",
            "plots/groei_per_dag_opnames.png",
            "plots/reden_van_opname.png")) ## Post tweet
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.main.id <- posted_tweet$id_str
tweet.last_id <- tweet.main.id

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) covid-19 - workweek update - part 1.1"))
push(repo, credentials = git.auth)

########
# Tweet - nursery homes
########
all.data <- read.csv("data/all_data.csv")

source("plot_scripts/nursery_homes.R")

total.nursing.locations <- last(all.data$total.current.locations.nursery,5)
total.nursing.locations <- last(total.nursing.locations[!is.na(total.nursing.locations)],2)


new.locations.nursery <- total.nursing.locations[2] - total.nursing.locations[1]

tweet.nurseryhomes <- paste0("#Verpleeghuis t.o.v. laatste update: 

Positief getest: ",last(all.data$infections.today.nursery),"
Totaal: ",last(all.data$infections.total.nursery),"

Overleden: ",last(all.data$deaths.today.nursery),"
Totaal: ",last(all.data$deaths.total.nursery),"

Nieuwe locaties met besmettingen: ",new.locations.nursery,"
Huidig aantal locaties met besmettingen:* ",last(all.data$total.current.locations.nursery),"
*Locaties waar in de afgelopen 28 dagen minstens één COVID-19 besmetting is gemeld.")

# Tweet for nursery homes ####
posted_tweet <- post_tweet (
  tweet.nurseryhomes,
  token = token.mzelst,
  media = c("plots/nursery_homes_vr_map.png",
            "plots/verpleeghuizen_bewoners.png",
            "plots/verpleeghuizen_locaties.png"
  ),
  in_reply_to_status_id = tweet.last_id,
  auto_populate_reply_metadata = TRUE
)
posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str


## Vroeg surveillance

##### Generate municipality images ####
source("workflow/parse_nice-municipalities-data.R")
source("workflow/parse_municipalities.R")
source("workflow/generate_municipality_images.R")

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])

## Push to git
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) covid-19 - workweek update - part 2"))
push(repo, credentials = git.auth)


# Infectieradar

infectieradar <- rjson::fromJSON(file = "https://data.rivm.nl/covid-19/COVID-19_Infectieradar_symptomen_per_dag.json",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  mutate(date = as.Date(Date_of_statistics)) %>%
  mutate(infectieradar_7d = frollmean(Perc_covid_symptoms,7)) %>%
  mutate(groei_infectieradar = Perc_covid_symptoms/(dplyr::lag(Perc_covid_symptoms,7))) %>%
  mutate(groei_infectieradar_7d = frollmean(groei_infectieradar,7))

infectieradar %>%
  dplyr::filter(date >= "2022-01-01") %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = MA_perc_covid_symptoms), linewidth = 1.2, color = "red") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 0.5)) + 
  labs(x = "Datum",
       y = "% met klachten",
       title = "Infectieradar: % deelnemers met Covid-19-achtige klachten",
       color = "Legend",
       caption = paste("Bron data: RIVM  | Plot: @mzelst | ",Sys.Date()))
ggsave("plots/infectieradar.png", width = 16, height = 8)

## Put in date breaker for dashboard data download ##

repeat {
  Sys.sleep(15)
  date.now <- Sys.Date()
  dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
  date.dashboard <- as.Date(as.POSIXct(parse_number(dat$last_generated), origin = "1970-01-01"))
  
  if (date.now == date.dashboard){
    message <- "GO GO GO GO GO"
    break
  }
}

dat <- fromJSON(txt = "https://coronadashboard.rijksoverheid.nl/json/NL.json")
sewer.data <- dat$sewer$values

sewer.data <- sewer.data %>%
  mutate(date = as.Date(as.POSIXct(date_unix, origin = "1970-01-01"))) %>%
  mutate(sewer_7d = frollmean(average, 7))  %>%
  mutate(groei_riool = sewer_7d/(dplyr::lag(sewer_7d,7))) %>%
  mutate(groei_riool_7d = frollmean(groei_riool,7)) %>%
  dplyr::filter(date >= "2022-01-01")

sewer.max <- max(sewer.data$sewer_7d,na.rm=T)
y.max.sewer <- ceiling(sewer.max/1000)*1000

sewer.data %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = sewer_7d), lwd = 1.2, color = "red") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, vjust = 0.5)) + 
  scale_y_continuous(limits = c(0, y.max.sewer), breaks = seq(0,y.max.sewer,500)) +
  labs(x = "Datum",
       y = "Virusvracht per 100.000 inwoners",
       title = "Rioolwater: Gemiddelde aantal virusdeeltjes per 100.000 inwoners",
       color = "Legend",
       caption = paste("Bron data: RIVM  | Plot: @mzelst | ",Sys.Date()))
ggsave("plots/rioolwater.png", width = 16, height = 8)

source("plot_scripts/rioolwater.R")

sign.riool <- paste0(ifelse(last(sewer.data$groei_riool_7d)*100-100>=0,"+",""))
sign.radar <- paste0(ifelse(last(infectieradar$infectieradar_7d)>=0,"+",""))
sign.radar.change <- paste0(ifelse(last(infectieradar$groei_infectieradar_7d)*100-100>=0,"+",""))

vroegsurveillance.tweet <- paste0("Vroege signalen

Rioolwater: ",round(last(sewer.data$sewer_7d),1)," RNA flow per 100.000 inwoners
Verandering (week op week): ",paste0(sign.riool,round(last(sewer.data$groei_riool_7d)*100-100,1)),"%

Infectieradar: ",paste0(sign.radar,round(last(infectieradar$infectieradar_7d),1)),"% covid-19-achtige klachten
Verandering (week op week): ",paste0(sign.radar.change,round(last(infectieradar$groei_infectieradar_7d)*100-100,1)),"%

Overzicht data per gemeente: https://raw.githack.com/mzelst/covid-19/master/workflow/daily_municipality.html")


posted_tweet <- post_tweet (
  vroegsurveillance.tweet,
  token = token.mzelst,
  media = c("plots/infectieradar.png",
            "plots/rioolwater.png",
            "plots/rioolwater_veiligheidsregio.png"),
  in_reply_to_status_id = tweet.main.id,
  auto_populate_reply_metadata = TRUE)

posted_tweet <- fromJSON(rawToChar(posted_tweet$content))
tweet.last_id <- posted_tweet$id_str

## Git Vroegsurveillance

git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])
## Push to git
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) covid-19 - workweek update - Early surveillance"))
push(repo, credentials = git.auth)

##### Download case file
rivm.data <- fread("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv", sep =";") ## Read in data with all cases until today
rivm.data.archive <- fread("data-rivm/casus-datasets/COVID-19_casus_landelijk_2021_until_03102021.csv.gz")
rivm.data <- rbind(rivm.data.archive,rivm.data)

last_date <- as.Date(last(rivm.data$Date_statistics))

filename.compressed <- paste0("data-rivm/casus-datasets/COVID-19_casus_landelijk_",last_date,".csv.gz")
fwrite(rivm.data, file=filename.compressed,row.names = F) ## Write file with all cases until today

#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc"); rmarkdown::render('reports/daily_report.Rmd') ## Render daily report
#file.copy(from = list.files('reports', pattern="*.pdf",full.names = TRUE), 
#          to = paste0("reports/daily_reports/Epidemiologische situatie COVID-19 in Nederland - ",
#                      format((last_date),'%d')," ",format((last_date),'%B')," 2022.pdf")) ## Save daily file in archive

## Workflows for databases
rm(list=ls())
source("workflow/parse_data_download_rivm.R")
source("workflow/dashboards/cases_ggd_agegroups.R")
source("workflow/dashboards/date_statistics_mutations.R")
source("workflow/parse_age-data.R")
source("workflow/dashboards/rivm-date-corrections.R")
source("workflow/dashboards/heatmap-age-week.R")
source("workflow/dashboards/ggd_tests_corrections.R")
## Download data coronadashboard ##
filename.dashboard <- paste0("data-rivm/dashboard-data/data-coronadashboard_",Sys.Date(),".zip")
download.file("https://coronadashboard.rijksoverheid.nl/json/latest-data.zip",filename.dashboard)
#source("workflow/estimate_R.R")
#source("workflow/excess_mortality_cbsmodel_2021.R")
source("workflow/parse_deaths_comparison_tracker.R")
source("workflow/parse_vaccines_ecdc.R")
source("workflow/parse_vaccination_neighborhood.R")



remove(list = ls())
source("workflow/twitter/token_mzelst.R")
POST(url = deploy.netlify.url)
