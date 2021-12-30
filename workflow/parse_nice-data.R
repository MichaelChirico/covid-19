## Stichting NICE data
# ICs used
ics.used <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/ic-count",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# IC patients died, discharged, discharged to other department (cumulative)
ic.death_survive <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/died-and-survivors-cumulative", simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE) %>%
  slice(2,4,6) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rename(ic_deaths = V1,ic_discharge = V2, ic_discharge_inhosp = V3)

# New patients at IC 
ic_intake <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE) %>%
  slice(2,4) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rename(ic_intake_proven = V1,ic_intake_suspected = V2)

# IC patients currently
ic_current <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# IC patients cumulative
ic.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Number of patients currently in hospital (non-IC) 
zkh_current <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Intake per day of patients in hospital (non-IC) with suspected and/or proven covid-19
zkh_new <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE) %>%
  slice(1,2,4) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rename(date = V1,new_hosp_proven = V2, new_hosp_suspected = V3)

## Cumulative intake (non-IC)
hospital.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Merge all data
df <- data.frame(zkh_new,ic_intake,ic_current$value,ics.used$value,ic.cumulative$value,zkh_current$value,ic.death_survive, hospital.cumulative$value)
names(df) <- c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","IC_Intake_Suspected","IC_Current","ICs_Used","IC_Cumulative","Hospital_Currently","IC_Deaths_Cumulative","IC_Discharge_Cumulative","IC_Discharge_InHospital","Hospital_Cumulative")
df <- df %>%
  mutate(Hospital_Intake = Hospital_Intake_Proven + Hospital_Intake_Suspected) %>%
  mutate(IC_Intake = IC_Intake_Proven + IC_Intake_Suspected) %>%
  mutate(Hosp_Intake_Suspec_Cumul = cumsum(Hospital_Intake_Suspected))%>% 
  mutate(IC_Intake_Suspected_Cumul = cumsum(IC_Intake_Suspected)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(IC_Intake_Proven_Cumsum = cumsum(IC_Intake_Proven))

write.csv(df, "data-nice/nice-today.csv", row.names = F) ## Write file with all NICE data until today
filename.nice.perday <- paste0("data-nice/data-nice-json/",Sys.Date(),".csv")
write.csv(df, filename.nice.perday, row.names = F) ## Save daily NICE data - JSON parsed - downloaded around 14:30 PM (CET)

## Daily NICE data
nice.dailydata <- last(df)
filename.daily.nice <- paste0("data-nice/data-per-day/nice_daily_",Sys.Date(),".csv") ## Filename for daily data
write.csv(nice.dailydata, file = filename.daily.nice, row.names = F) ## Write file with daily data

temp = list.files(path = "data-nice/data-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, read.csv) ## Load all day files

nice_by_day <- myfiles %>%
  map_dfr(~{.x}) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(ic_intake_nice = c(0,diff(IC_Cumulative))) %>%
  mutate(ic_intake_nice = replace(ic_intake_nice, ic_intake_nice<0, 0))

write.csv(nice_by_day, file = "data/nice_by_day.csv", row.names = F)

## Leeftijd op IC
leeftijd.ic <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/age-distribution-status/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  slice(1,2,4,6,8) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rowwise() %>% mutate(Totaal = sum(c(V2,V3,V4,V5))) %>%
  rename(Leeftijd = V1, IC_naar_Klinisch = V2, IC_aanwezig = V3, Verlaten_Levend = V4, Verlaten_Overleden = V5) %>%
  mutate(Datum = as.Date(Sys.Date()))

filename.IC <- paste0("data-nice/age/IC/nice_daily_age_IC_",Sys.Date(),".csv")
write.csv(leeftijd.ic, file = filename.IC, row.names = F)

## Leeftijd op Klinische afdeling
leeftijd.klinisch <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/age-distribution-status/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  slice(1,2,4,6) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rowwise() %>% mutate(Totaal = sum(c(V2,V3,V4))) %>%
  rename(Leeftijd = V1, Klinisch_aanwezig = V2, Verlaten_Levend = V3, Verlaten_Overleden = V4) %>%
  mutate(Datum = as.Date(Sys.Date()))

filename.klinisch <- paste0("data-nice/age/Clinical_Beds/nice_daily_age_clinical_",Sys.Date(),".csv")
write.csv(leeftijd.klinisch, file = filename.klinisch, row.names = F)

## Exit IC
ic.died.left <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/died-and-survivors-cumulative/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  slice(1,2,4,6) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rename(date = V1, Overleden = V2, Ontslagen = V3, IC_to_Clinical_Alive = V4)

filename.IC.exit <- paste0("data-nice/exit/IC/nice_daily_exit_IC_",Sys.Date(),".csv")
write.csv(ic.died.left, file = filename.IC.exit, row.names = F)

## Exit Clinical beds
zkh.died.left <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/died-and-survivors-cumulative/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  slice(1,2,4) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rename(date = V1, Overleden = V2, Ontslagen = V3) %>%
  mutate(Overleden_pdag = c(0,diff(Overleden))) %>%
  mutate(Ontslagen_pdag = c(0,diff(Ontslagen)))

filename.klinisch.exit <- paste0("data-nice/exit/Clinical_Beds/nice_daily_exit_clinical_",Sys.Date(),".csv")
write.csv(zkh.died.left, file = filename.klinisch.exit, row.names = F)

#### Treatment Time IC ####
ligduur.ic <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/behandelduur-distribution/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  slice(1,2,4,6,8) %>%
  t() %>% as.data.frame() %>%
  rename(dagen = V1, IC_to_clinical = V2, Treatment_time_hospitalized = V3, Treatment_time_to_exit = V4, Treatment_time_to_death = V5)

filename.ligduur.ic <- paste0("data-nice/treatment-time/IC/nice_daily_treatment-time_IC_",Sys.Date(),".csv")
write.csv(ligduur.ic, file = filename.ligduur.ic, row.names = F)

#### Treatment Time clinical beds ####
ligduur.clinical <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/behandelduur-distribution/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE) %>%
  slice(1,2,4,6) %>%
  t() %>% as.data.frame() %>%
  rename(dagen = V1, Treatment_time_hospitalized = V2, Treatment_time_to_exit = V3, Treatment_time_to_death = V4)

filename.ligduur.clinical <- paste0("data-nice/treatment-time/Clinical_Beds/nice_daily_treatment-time_clinical_",Sys.Date(),".csv")
write.csv(ligduur.clinical, file = filename.ligduur.clinical, row.names = F)

rm("df","filename.daily.nice","filename.nice.perday","hospital.cumulative","ic.cumulative","ic.death_survive",
   "ic.died_survivors","ic_current","ic_intake","ics.used","json_zkh_df","nice.dailydata","zkh_current","zkh_new","temp","myfiles",
   "nice_by_day", "ic.died.left","leeftijd.ic","leeftijd.klinisch","ligduur.clinical","ligduur.ic","zkh.died.left",
   "filename.IC","filename.IC.exit","filename.klinisch","filename.klinisch.exit","filename.ligduur.ic",
   "filename.ligduur.clinical")