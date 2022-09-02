rivm.mun.perday <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv", sep=";")
rivm.mun.perday.archive <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag_tm_03102021.csv")
rivm.mun.perday <- rbind(rivm.mun.perday.archive,rivm.mun.perday)
# Verify that new data has been uploaded
#condition <- Sys.Date()!=as.Date(last(rivm.mun.perday$Date_of_report))

#if (condition) {stop("The value is TRUE, so the script must end here")    
#} else {

# Parse data municipality per day 
last_date <- as.Date(last(rivm.mun.perday$Date_of_report))
filename.mun.perday <- paste0("raw-data-archive/municipal-datasets-per-day/rivm_municipality_perday_", last_date, ".csv") ## Filename for daily data municipalities
fwrite(rivm.mun.perday, file=filename.mun.perday,row.names = F)

filename.mun.perday.compressed <- paste0("data-rivm/municipal-datasets-per-day/rivm_municipality_perday_", last_date, ".csv.gz") ## Filename for daily data municipalities
fwrite(rivm.mun.perday, file=filename.mun.perday.compressed,row.names = F)

rivm.mun.perday$ROAZ_region <- NULL

rivm.mun.cum <- rivm.mun.perday[, lapply(.SD,sum), .SDcols = c("Total_reported","Deceased"),by = list(Version,
                                                                                                      Municipality_name,
                                                                                                      Municipality_code, 
                                                                                                      Security_region_code,
                                                                                                      Security_region_name,
                                                                                                      Municipal_health_service,
                                                                                                      Province,
                                                                                                      Date_of_publication,
                                                                                                      Date_of_report)
][, Total_reported_cum := cumsum(Total_reported), by = list(Municipality_code,Security_region_code,Province)
][, Deceased_cum := cumsum(Deceased), by = list(Municipality_code,Security_region_code,Province)
]

fwrite(rivm.mun.cum, file = "data-rivm/COVID-19_aantallen_gemeente_per_dag.csv.gz", row.names = F)

## Parse RIVM Daily data
rivm.dailydata <- data.frame(as.Date(Sys.Date()),sum(rivm.mun.cum$Total_reported),0,sum(rivm.mun.cum$Deceased)) ## Calculate totals for cases, hospitalizations, deaths
names(rivm.dailydata) <- c("date","cases","hospitalization","deaths")

filename.daily <- paste0("data-rivm/data-per-day/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(rivm.dailydata, file = filename.daily,row.names = F) ## Write file with daily data

temp = list.files(path = "data-rivm/data-per-day/",pattern="*.csv", full.names = T) ## Fetch all day files
rivm_by_day = rbindlist(lapply(temp, fread)) ## Load all day files

rivm_by_day <- rivm_by_day[order(date),
][, positivetests := c(0,diff(cases))
][, hospital_intake_rivm := c(0,diff(hospitalization))]

fwrite(rivm_by_day, file = "data/rivm_by_day.csv",row.names = F) ## Write file with aggregate data per day


## Download nursing homes

nursing.homes <- fread("https://data.rivm.nl/covid-19/COVID-19_verpleeghuizen.csv", sep = ";")
nursing.homes.archive <- fread("https://data.rivm.nl/covid-19/COVID-19_verpleeghuizen_tm_03102021.csv")
nursing.homes <- rbind(nursing.homes.archive,nursing.homes)

filename.nursinghomes.raw <- paste0("raw-data-archive/nursing-home-datasets/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(nursing.homes, file = filename.nursinghomes.raw,row.names = F)

filename.nursinghomes.compressed <- paste0("data-rivm/nursing-homes-datasets/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
fwrite(nursing.homes, file = filename.nursinghomes.compressed,row.names = F)

## Download tests

tests <- fread("https://data.rivm.nl/covid-19/COVID-19_uitgevoerde_testen.csv", sep = ";")
tests.archive <- fread("https://data.rivm.nl/covid-19/COVID-19_uitgevoerde_testen_tm_03102021.csv")
tests <- rbind(tests.archive, tests)

filename.tests.raw <- paste0("raw-data-archive/tests/rivm_daily_",Sys.Date(),".csv") ## Filename for daily data
fwrite(tests, file = filename.tests.raw,row.names = F)

filename.tests.compressed <- paste0("data-rivm/tests/rivm_daily_",Sys.Date(),".csv.gz") ## Filename for daily data
fwrite(tests, file = filename.tests.compressed,row.names = F)


#continue the script
print("Script did NOT end!")   
#}