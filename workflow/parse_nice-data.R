## Stichting NICE data
# ICs used
ics.used <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/ic-count",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

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


# Number of patients currently in hospital (non-IC) 
zkh_current <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Intake per day of patients in hospital (non-IC) with suspected and/or proven covid-19
zkh_new <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE) %>%
  slice(1,2,4) %>%
  t() %>% as.data.frame() %>%
  mutate_all(unlist) %>%
  rename(date = V1,new_hosp_proven = V2, new_hosp_suspected = V3)


# Merge all data
df <- data.frame(zkh_new,ic_intake,ic_current$value,ics.used$value,zkh_current$value)
names(df) <- c("date","Hospital_Intake_Proven","Hospital_Intake_Suspected","IC_Intake_Proven","IC_Intake_Suspected","IC_Current","ICs_Used","Hospital_Currently")
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


rm("df","filename.daily.nice","filename.nice.perday","hospital.cumulative","ic.cumulative","ic.death_survive",
   "ic_current","ic_intake","ics.used","nice.dailydata","zkh_current","zkh_new","temp","myfiles",
   "nice_by_day", "ic.died.left","leeftijd.ic","leeftijd.klinisch","ligduur.clinical","ligduur.ic","zkh.died.left",
   "filename.IC","filename.IC.exit","filename.klinisch","filename.klinisch.exit","filename.ligduur.ic",
   "filename.ligduur.clinical")
