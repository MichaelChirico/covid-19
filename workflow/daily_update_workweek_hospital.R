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



rivm.by_day <- fread("data/rivm_by_day.csv")  
nice.by_day <- fread("data-nice/nice-today.csv")
lcps.by_day <- fread("data/lcps_by_day.csv")
corr.by_day <- fread("corrections/corrections_perday.csv")
nursery.by_day <- fread("data/nursery_by_day.csv")
testrate.by_day <- fread("data-dashboards/percentage-positive-daily-national.csv")[,c("values.tested_total","values.infected","values.infected_percentage","date","pos.rate.3d.avg","pos.rate.7d.avg")]
#vaccines.by_day <- read.csv("data/vaccines_by_day.csv") , vaccines.by_day

daily_datalist <- list(nice.by_day,lcps.by_day,corr.by_day,nursery.by_day, testrate.by_day, rivm.by_day)

#

all.data <- Reduce(
  function(x, y, ...) merge(x, y, by="date",all.x = TRUE, ...),
  daily_datalist
)

write.csv(all.data, file = "data/all_data.csv",row.names = F)

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

Kliniek_Nieuwe_Opnames <- ifelse(is.na(last(all.data$Kliniek_Nieuwe_Opnames_COVID_Nederland)),"Onbekend",last(all.data$Kliniek_Nieuwe_Opnames_COVID_Nederland))
Kliniek_Aanwezig <- ifelse(is.na(last(all.data$Kliniek_Bedden_Nederland)),"Onbekend",paste0(format(last(all.data$Kliniek_Bedden_Nederland),decimal.mark = ",",big.mark =".",big.interval = 3),sign.hosp.lcps,LCPS_Verpleeg_Huidig_Toename))
IC_Nieuwe_Opnames <- ifelse(is.na(last(all.data$IC_Nieuwe_Opnames_COVID_Nederland)),"Onbekend",last(all.data$IC_Nieuwe_Opnames_COVID_Nederland))
IC_Aanwezig <- ifelse(is.na(last(all.data$IC_Bedden_COVID_Nederland)),"Onbekend",paste0(last(all.data$IC_Bedden_COVID_Nederland),sign.ic.lcps,LCPS_IC_Huidig_Toename))
IC_Aanwezig_Int <- ifelse(is.na(last(all.data$IC_Bedden_COVID_Internationaal)),"Onbekend",paste0(last(all.data$IC_Bedden_COVID_Internationaal),sign.ic.int.lcps,LCPS_IC_Int_Huidig_Toename))

tweet.main <- paste0("#COVID19NL

Opgenomen: ",Kliniek_Nieuwe_Opnames,"
Huidig: ",Kliniek_Aanwezig,")

Opgenomen op IC: ",IC_Nieuwe_Opnames,"
Huidig IC Nederland: ",IC_Aanwezig,")")

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
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) covid-19 - workweek update (hospital only)"))
push(repo, credentials = git.auth)


remove(list = ls())
source("workflow/twitter/token_mzelst.R")