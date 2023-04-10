## Download data disabled people 
#disabled.people <- fread("https://data.rivm.nl/covid-19/COVID-19_gehandicaptenzorg.csv", sep = ";")
#disabled.people.archive <- fread("https://data.rivm.nl/covid-19/COVID-19_gehandicaptenzorg_tm_03102021.csv")
#disabled.people <- rbind(disabled.people.archive, disabled.people)
#last_date <- as.Date(max(disabled.people$Date_of_statistic_reported))

#filename.disabledpeople.compressed  <- paste0("data-rivm/disabled-people-per-day/rivm_daily_",last_date,".csv.gz") ## Filename for daily data
#fwrite(disabled.people, file = filename.disabledpeople.compressed,row.names = F) 

## Download data 70+ living at home 
#living.home.70plus <- fread("https://data.rivm.nl/covid-19/COVID-19_thuiswonend_70plus.csv", sep = ";")
#living.home.70plus.archive <- fread("https://data.rivm.nl/covid-19/COVID-19_thuiswonend_70plus_tm_03102021.csv")
#living.home.70plus <- rbind(living.home.70plus.archive, living.home.70plus)
#last_date <- as.Date(max(living.home.70plus$Date_of_statistic_reported))

#filename.living.home.70plus.compressed <- paste0("data-rivm/70plus-living-at-home-per-day/rivm_daily_",last_date,".csv.gz") ## Filename for daily data
#fwrite(living.home.70plus, file = filename.living.home.70plus.compressed,row.names = F) 

## Download behavior
behavior <- fread("https://data.rivm.nl/covid-19/COVID-19_gedrag.csv", sep = ";")
last_date <- as.Date(max(behavior$Date_of_measurement))

filename.behavior.compressed <- paste0("data-rivm/behavior/rivm_daily_",last_date,"_of_data.csv.gz") ## Filename for daily data
fwrite(behavior, file = filename.behavior.compressed,row.names = F) 

## Download IC data (NICE)

ic.nice.data <- fread("https://data.rivm.nl/covid-19/COVID-19_ic_opnames.csv", sep = ";")
ic.nice.data.archive <- fread("https://data.rivm.nl/covid-19/COVID-19_ic_opnames_tm_03102021.csv")
ic.nice.data <- rbind(ic.nice.data.archive, ic.nice.data)
filename.ic.nice <- paste0("data-rivm/ic-datasets/ic_daily_",last(ic.nice.data$Date_of_statistics),".csv") ## Filename for daily data
fwrite(ic.nice.data, file = filename.ic.nice,row.names = F)

## Download IC data stratified by age and week (NICE)

ic.nice.age.data <- fread("https://data.rivm.nl/covid-19/COVID-19_ziekenhuis_ic_opnames_per_leeftijdsgroep.csv", sep = ";")
ic.nice.age.data.archive <- fread("https://data.rivm.nl/covid-19/COVID-19_ziekenhuis_ic_opnames_per_leeftijdsgroep_tm_03102021.csv")
ic.nice.age.data <- rbind(ic.nice.age.data.archive, ic.nice.age.data)
filename.ic.age.nice <- paste0("data-rivm/ic-age-datasets/ic_daily_",last(ic.nice.age.data$Date_of_statistics),".csv") ## Filename for daily data
fwrite(ic.nice.age.data, file = filename.ic.age.nice,row.names = F)

## Download contact tracing data 

#settings <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_settings_per_dag.csv", sep = ";")
#filename.bco.settings <- paste0("data-rivm/bco-settings/bco_settings_daily_",last(settings$Date_of_publication),".csv") ## Filename for daily data
#fwrite(settings, file = filename.bco.settings,row.names = F)

## Download vaccine rate per municipality

vaccine.municipality <- fread("https://data.rivm.nl/data/covid-19/COVID-19_vaccinatiegraad_per_gemeente_per_week_leeftijd.csv")
filename.vaccine.municipality <- paste0("data-rivm/vaccine-municipality/vaccine_municipality_weekly_",as.Date(last(vaccine.municipality$Date_of_statistics)),"_ofdata.csv") ## Filename for daily data
fwrite(vaccine.municipality, file = filename.vaccine.municipality,row.names = F)

## Download vaccine rate per neighborhood per week

vaccine.neighborhood <- fread("https://data.rivm.nl/covid-19/COVID-19_vaccinatiegraad_per_wijk_per_week.csv")
filename.vaccine.neighborhood <- paste0("data-rivm/vaccine-neighborhood/vaccine_neighborhood_weekly_",as.Date(last(vaccine.neighborhood$Date_of_statistics)),"_ofdata.csv.gz") ## Filename for daily data
fwrite(vaccine.neighborhood, file = filename.vaccine.neighborhood,row.names = F)


## Download Infectieradar

infectieradar <- fread("https://data.rivm.nl/covid-19/COVID-19_Infectieradar_symptomen_per_dag.csv")
filename.infectieradar <- paste0("data-rivm/infectieradar/infectieradar_daily_",as.Date(last(infectieradar$Date_of_statistics)),".csv") ## Filename for daily data
fwrite(infectieradar, file = filename.infectieradar,row.names = F)

## Download variant data 

variants <- fread("https://data.rivm.nl/covid-19/COVID-19_varianten.csv", sep = ";")
filename.variants <- paste0("data-misc/variants-rivm/data-variants-rivm-open-data/variants_rivm_",ymd(last(variants$Date_of_statistics_week_start)),".csv") ## Filename for daily data
fwrite(variants, file = filename.variants,row.names = F)

