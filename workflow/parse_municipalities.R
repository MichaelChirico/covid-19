# For emo:ji please install the following
# install.packages("devtools")
# devtools::install_github("hadley/emo")

## Data for municipalities

# Cumulative dataset 
rivm.municipalities <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv", sep=";")
last_date <- as.Date(last(rivm.municipalities$Date_of_report))

filename.municipality.compressed <- paste0("data-rivm/municipal-datasets/rivm_municipality_", last_date ,".csv.gz") ## Filename for daily data municipalities
fwrite(rivm.municipalities, file = filename.municipality.compressed)

rm(rivm.municipalities, last_date, filename.municipality.compressed)


# const.date <- as.Date('2020-09-10') ## Change when you want to see a specific date
const.use_daily_dataset <- FALSE # Use COVID-19_aantallen_gemeente_per_dag.csv instead of COVID-19_aantallen_gemeente_cumulatief.csv
const.use_hospital_dataset <- FALSE # Use the dedicated COVID-19_ziekenhuisopnames.csv instead of the combined set

# set emoji's for unix and windows
emoji.up <- intToUtf8(0x279A)
emoji.down <- intToUtf8(0x2798)
emoji.black <- intToUtf8(0x26A1)
emoji.purple <- intToUtf8(0x1F7E3)
emoji.red <- intToUtf8(0x1F6D1)
emoji.orange <- intToUtf8(0x1F7E7)
emoji.yellow <- intToUtf8(0x1F7E1)
emoji.green <- intToUtf8(0x2705)
emoji.new <- intToUtf8(0x1F4A5)

if (.Platform$OS.type == "windows") {
  emoji.up <- "&#x279A;"
  emoji.down <- "&#x2798;"
  emoji.black <- "&#x26a1;"
  emoji.purple <- "&#x1F7E3;"
  emoji.red <- "&#128721;"
  emoji.orange <- "&#128999;"
  emoji.yellow <- "&#128993;"
  emoji.green <- "&#9989;"
  emoji.new <- "&#128165;"
}

emoji.up_double = paste(emoji.up, emoji.up, sep="")
emoji.down_double = paste(emoji.down, emoji.down, sep="")


# methods
convert_to_trafficlight <- function(rel_increase) {
  trafficlight <- 
    ifelse( rel_increase >= 250, emoji.black,
            ifelse( rel_increase >= 100, emoji.purple,
                    ifelse( rel_increase >= 35, emoji.red,
                            ifelse( rel_increase > 5,   emoji.orange,
                                    ifelse( rel_increase > 0,   emoji.yellow,
                                            emoji.green
                                    )))))
  return(trafficlight)
}

convert_to_hosp_trafficlight <- function(rel_increase_1w, rel_increase_2w) {
  trafficlight <- 
    ifelse( rel_increase_1w >= 27/10, emoji.black,
            ifelse( rel_increase_2w >= 27/10*2, emoji.black,
                    ifelse( rel_increase_1w >= 16/10, emoji.purple,
                            ifelse( rel_increase_2w >= 16/10*2, emoji.purple,
                                    ifelse( rel_increase_1w >= 4/10, emoji.red,
                                            ifelse( rel_increase_2w >= 4/10*2, emoji.red,
                                                    ifelse( rel_increase_2w > 0,   emoji.orange,
                                                            emoji.green
                                                    )))))))
  return(trafficlight)
}

calc_growth_increase <- function(increase_7d, increase_14d){
  growth <- 
    ifelse( 
      increase_14d-increase_7d <= 0,
      increase_7d * 100,
      ((increase_7d / (increase_14d - increase_7d)) - 1 ) * 100
    )
  return(growth) 
}

increase_growth_to_arrows <- function(increase_growth, allow_double = TRUE) {
  arrows <- 
    ifelse( allow_double & increase_growth > 100, emoji.up_double,
            ifelse( increase_growth >= 5, emoji.up,
                    ifelse( allow_double & increase_growth <= -50, emoji.down_double,
                            ifelse( increase_growth <= -5, emoji.down,
                                    "-"
                            ))))
  return(arrows)
}

# Parse and cleanup data
if (const.use_daily_dataset) {
  dat <- read.csv("data-rivm/COVID-19_aantallen_gemeente_per_dag.csv.gz", encoding = "UTF-8")
  dat <- dat %>%
    group_by(
      Municipality_code, 
      Province, 
      Date_of_publication
    ) %>%
    summarise(
      Date_of_report = Date_of_publication,
      Municipality_code = Municipality_code,
      Municipality_name = Municipality_name,
      Province = Province,
      Total_reported = sum(Total_reported_cum),
      Hospital_admission = sum(Hospital_admission_cum),
      Deceased = sum(Deceased_cum),
      .groups = 'drop'
    ) %>%
    arrange(Date_of_report, Municipality_code == "", Municipality_code, Province)
} else {
  temp = list.files(path = "data-rivm/municipal-datasets/",pattern="*.csv.gz", full.names = T) ## Pull names of all available datafiles
  dat <- read.csv(last(temp), fileEncoding = "UTF-8") ## Take last filename from the folder, load csv
  rm(temp)
}

dat$date <- as.Date(dat$Date_of_publication) ## character into Date class
last_date <- as.Date(last(dat$Date_of_publication))
if(!exists("const.date")){ 
  const.date <- last_date
}
const.date_hosp <- const.date



## Add hospital admissions from NICE
nice.hosp <- fread("https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames.csv")
nice.hosp.archive <- fread("https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames_tm_03102021.csv")
nice.hosp <- rbind(nice.hosp.archive, nice.hosp)
nice.hosp$date <- nice.hosp$Date_of_statistics+1

nice.hosp.cumsum <- nice.hosp %>%
  group_by(
    Municipality_code, 
    Security_region_code) %>%
  mutate(Hospital_admission = cumsum(Hospital_admission)) %>%
  ungroup() %>%
  dplyr::select(Municipality_code,Municipality_name,Hospital_admission,Date_of_statistics) %>%
  rename(date = Date_of_statistics) %>%
  mutate(Municipality_name = recode(Municipality_name, "Noardeast-FryslÃ¢n" = "Noardeast-Fryslân",
                                    "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân")) %>%
  mutate(date = date + 1)

dat <- dat %>%
  mutate(Hospital_admission = NULL) %>%
  mutate(Municipality_name = recode(Municipality_name, "Noardeast-FryslÃ¢n" = "Noardeast-Fryslân",
                                    "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân"))

dat <- merge(dat,nice.hosp.cumsum,by=c("date","Municipality_code","Municipality_name"),all.x=T)

## Filter Eemsdelta

dat[dat$Municipality_code=="GM0164", "Municipality_name"] <- "Hengelo"
dat.eemsdelta.codes <- c("GM0010", "GM0003", "GM0024", "GM1979")
dat.eemsdelta <- dat %>%
  dplyr::filter(Municipality_code %in% dat.eemsdelta.codes) %>%
  group_by(date) %>%
  mutate(
    Municipality_name = "Eemsdelta", 
    Municipality_code = "GM1979",
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission),
    Deceased = sum(Deceased),
  )

dat <- dat %>%
  dplyr::filter(!Municipality_code %in% dat.eemsdelta.codes) %>%
  dplyr::filter(Municipality_code != "GM0788") %>%
  rbind(dat.eemsdelta)

rm(dat.eemsdelta.codes, dat.eemsdelta)


## Purmerend

dat.purmerend.codes <- c("GM0370", "GM0439")
dat.purmerend <- dat %>%
  dplyr::filter(Municipality_code %in% dat.purmerend.codes) %>%
  group_by(date) %>%
  mutate(
    Municipality_name = "Purmerend", 
    Municipality_code = "GM0439",
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission),
    Deceased = sum(Deceased),
  ) %>%
  distinct()

dat <- dat %>%
  dplyr::filter(!Municipality_code %in% dat.purmerend.codes) %>%
  dplyr::filter(Municipality_code != "0370") %>%
  rbind(dat.purmerend)
rm(dat.purmerend.codes, dat.purmerend)

# Filter Dijk en Waard

dat.dijkenwaard.codes <- c("GM0398", "GM0416")
dat.dijkenwaard <- dat %>%
  dplyr::filter(Municipality_code %in% dat.dijkenwaard.codes) %>%
  group_by(date) %>%
  mutate(
    Municipality_name = "Dijk en Waard", 
    Municipality_code = "GM1980",
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission),
    Deceased = sum(Deceased),
  ) %>%
  distinct()

dat <- dat %>%
  dplyr::filter(!Municipality_code %in% dat.dijkenwaard.codes) %>%
  dplyr::filter(Municipality_code != "GM0398" | Municipality_code != "GM0416" ) %>%
  rbind(dat.dijkenwaard)
rm(dat.dijkenwaard.codes, dat.dijkenwaard)


# Filter Maashorst

dat.maashorst.codes <- c("GM1685", "GM0856")
dat.maashorst <- dat %>%
  dplyr::filter(Municipality_code %in% dat.maashorst.codes) %>%
  group_by(date) %>%
  mutate(
    Municipality_name = "Maashorst", 
    Municipality_code = "GM1991",
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission),
    Deceased = sum(Deceased),
  ) %>%
  distinct()

dat <- dat %>%
  dplyr::filter(!Municipality_code %in% dat.maashorst.codes) %>%
  dplyr::filter(Municipality_code != "GM1685") %>%
  dplyr::filter(Municipality_code != "GM0856") %>%
  rbind(dat.maashorst)
rm(dat.maashorst.codes, dat.maashorst)


# Filter Land van Cuijk

dat.landvancuijk.codes <- c("GM0756", "GM1684","GM0786","GM0815","GM1702")
dat.landvancuijk <- dat %>%
  dplyr::filter(Municipality_code %in% dat.landvancuijk.codes) %>%
  group_by(date) %>%
  mutate(
    Municipality_name = "Land van Cuijk", 
    Municipality_code = "GM1982",
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission),
    Deceased = sum(Deceased),
  ) %>%
  distinct()

dat <- dat %>%
  dplyr::filter(!Municipality_code %in% dat.landvancuijk.codes) %>%
  dplyr::filter(Municipality_code != "GM0756") %>%
  dplyr::filter(Municipality_code != "GM1684") %>%
  dplyr::filter(Municipality_code != "GM0786") %>%
  dplyr::filter(Municipality_code != "GM0815") %>%
  dplyr::filter(Municipality_code != "GM1702") %>%
  rbind(dat.landvancuijk)
rm(dat.landvancuijk.codes, dat.landvancuijk)


## Amsterdam - Weesp

dat.weesp.codes <- c("GM0457", "GM0363")
dat.weesp <- dat %>%
  dplyr::filter(Municipality_code %in% dat.weesp.codes) %>%
  group_by(date) %>%
  mutate(
    Municipality_name = "Amsterdam", 
    Municipality_code = "GM0363",
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission),
    Deceased = sum(Deceased),
  ) %>%
  distinct()

dat <- dat %>%
  dplyr::filter(!Municipality_code %in% dat.weesp.codes) %>%
  dplyr::filter(Municipality_code != "0457") %>%
  rbind(dat.weesp)
rm(dat.weesp.codes, dat.weesp)

dat.unknown <- dat %>%
  dplyr::filter(Municipality_code == "")  %>%
  group_by(date) %>%
  summarise(
    Municipality_name = 'Unknown',
    Municipality_code = '',
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission,na.rm=T),
    Deceased = sum(Deceased),
    .groups = 'drop_last'
  )

dat.total <- dat %>%
  dplyr::filter(Municipality_name != "") %>%
  group_by(date) %>%
  summarise(
    Municipality_name = 'Netherlands',
    Municipality_code = '',
    Total_reported = sum(Total_reported),
    Hospital_admission = sum(Hospital_admission,na.rm=T),
    Deceased = sum(Deceased),
    #.groups = 'drop_last'
  )

dat <- dat %>%
  dplyr::filter(Municipality_code != "") %>% # Filter observations without municipal name
  dplyr::select(
    Municipality_name, 
    Municipality_code,
    date, 
    Total_reported,
    Hospital_admission,
    Deceased
  ) %>% 
  rbind(dat.total) %>%
  rbind(dat.unknown)

rm(dat.unknown, dat.total)

dat.cases <- dat %>%
  dplyr::select(
    Municipality_name, 
    Municipality_code,
    date, 
    Total_reported
  )

dat.hosp <- dat %>%
  dplyr::select(
    Municipality_name, 
    Municipality_code,
    date, 
    Hospital_admission
  )

dat.deaths <- dat %>%
  dplyr::select(
    Municipality_name, 
    Municipality_code,
    date, 
    Deceased
  )

# Reshape file into wide format -- columns will be dates which report total cases on date
dat.cases <- dat.cases %>%
  distinct() %>%
  pivot_wider(
    values_from = Total_reported,
    names_from = date,
    values_fill = 0
  )

dat.hosp <- dat.hosp %>%
  distinct() %>%
  pivot_wider(
    values_from = Hospital_admission,
    names_from = date,
    values_fill = 0
  )

dat.deaths <- dat.deaths %>%
  distinct() %>%
  pivot_wider(
    values_from = Deceased,
    names_from = date,
    values_fill = 0
  )

date_diff <- ncol(dat.cases)-grep(const.date, colnames(dat.cases))

if (const.use_hospital_dataset) {
  dat.hosp <- read.csv("data/nice_by_municipality.csv", fileEncoding = "UTF-8", check.names = FALSE) ## Take last filename from the folder, load csv
  dat.hosp[dat.hosp$Municipality_code=="", "Municipality_name"] <- "Unknown"
  
  dat.total <- dat.hosp %>%
    summarise_if(
      is.numeric, 
      sum, 
      na.rm = TRUE)
  dat.total$Municipality_code <- ""
  dat.total$Municipality_name <- "Netherlands"
  
  dat.hosp <- dat.hosp %>%
    rbind(dat.total) %>%
    arrange(match(Municipality_name, c("Total", "Nederland", "Netherlands")), Municipality_code)
  
  const.date_hosp <- const.date_hosp - 1
}

# Add population
dat.pop <- read.csv("https://raw.githubusercontent.com/mzelst/covid-19/master/misc/municipalities-population.csv",
                    encoding = "UTF-8") %>%
  dplyr::select(Municipality_code, population)

dat.cases <- merge(dat.pop, dat.cases, by = "Municipality_code", all.y=TRUE)
dat.cases[dat.cases$Municipality_name=="Netherlands", "population"] <- 17443797
dat.cases[dat.cases$Municipality_name=="Unknown", "population"] <- 17443797
write.csv(dat.cases, file = "data/municipality-totals.csv",
          fileEncoding = "UTF-8")

dat.hosp <- merge(dat.pop, dat.hosp, by = "Municipality_code", all.y=TRUE)
dat.hosp[dat.hosp$Municipality_name=="Netherlands", "population"] <- 17443797
dat.hosp[dat.hosp$Municipality_name=="Unknown", "population"] <- 17443797
write.csv(dat.hosp, file = "data/municipality-hospitalisations.csv",
          fileEncoding = "UTF-8")

dat.deaths <- merge(dat.pop, dat.deaths, by = "Municipality_code", all.y=TRUE)
dat.deaths[dat.deaths$Municipality_name=="Netherlands", "population"] <- 17443797
dat.deaths[dat.deaths$Municipality_name=="Unknown", "population"] <- 17443797
write.csv(dat.deaths, file = "data/municipality-deaths.csv",
          fileEncoding = "UTF-8")

# Calculate zero point
dat.zeropoint <- dat %>%
  dplyr::filter(date >= as.Date('2021-01-01')) %>%
  group_by(Municipality_name)

dat.cases.lowest <- dat.zeropoint %>%
  slice(which.min(Total_reported)) %>%
  arrange(match(Municipality_name, c("Total", "Nederland", "Netherlands")), Municipality_code) %>%
  arrange(desc(date))


dat.hosp.lowest <- dat.zeropoint %>%
  slice(which.min(Hospital_admission)) %>%
  arrange(match(Municipality_name, c("Total", "Nederland", "Netherlands")), Municipality_code)

if (const.use_hospital_dataset){
  dat.hosp.lowest <- dat.hosp %>%
    pivot_longer( 
      !Municipality_name & !Municipality_code & !population,
      names_to = "date",
      values_to = "Hospital_admission",
    )
  dat.hosp.lowest$date <- as.Date(dat.hosp.lowest$date)
  dat.hosp.lowest <- dat.hosp.lowest %>%
    group_by(Municipality_name) %>%
    dplyr::filter(date >= as.Date('2021-01-01')) %>%
    slice(which.min(Hospital_admission)) %>%
    arrange(match(Municipality_name, c("Total", "Nederland", "Netherlands")), Municipality_code)
}

dat.deaths.lowest <- dat.zeropoint %>%
  slice(which.min(Deceased)) %>%
  arrange(match(Municipality_name, c("Total", "Nederland", "Netherlands")), Municipality_code)

rm(dat.zeropoint)

# Parse today lists
dat.cases.today <-transmute(dat.cases,
                            municipality = Municipality_name,
                            Municipality_code = Municipality_code, 
                            date = const.date,
                            d0  = dat.cases[,ncol(dat.cases)-date_diff], # today
                            d1  = dat.cases[,ncol(dat.cases)-date_diff-1], # yesterday
                            d7  = dat.cases[,ncol(dat.cases)-date_diff-7], # last week
                            d8  = dat.cases[,ncol(dat.cases)-date_diff-8], # yesterday's last week
                            d14 = dat.cases[,ncol(dat.cases)-date_diff-14], # 2 weeks back
                            jan1 = dat.cases$`2021-01-01`, # Jan 1st, 2021
                            lowest_since_jan1 = dat.cases.lowest$`Total_reported`,
                            lowest_since_jan1_date = dat.cases.lowest$`date`,
                            current = d0-lowest_since_jan1,
                            increase_1d = d0-d1, # Calculate increase since last day
                            increase_7d = d0-d7, # Calculate increase in 7 days
                            increase_14d = d0-d14, # Calculate increase in 14 days
                            increase_growth = calc_growth_increase(increase_7d, increase_14d), # Compare growth of last 7 days vs 7 days before,
                            growth = increase_growth_to_arrows(increase_growth),
                            population,
                            rel_increase_1d = increase_1d / population * 100000,
                            rel_increase_7d = increase_7d / population * 100000,
                            rel_increase_14d = increase_14d / population * 100000,
                            color = convert_to_trafficlight(rel_increase_7d),
                            color_incl_new = ifelse(
                              ((d1 - d8) <= 0 & (d0 - d1) > 0)
                              | ((d0 - d7) <= 0 & (d0 - d1) > 0),  
                              emoji.new, color),
                            color_yesterday = convert_to_trafficlight( (d1 - d8)/ population * 100000),
                            color_lastweek = convert_to_trafficlight( (d7 - d14)/ population * 100000)
)

dat.cases.today.simple <- dat.cases.today %>%
  dplyr::filter(Municipality_code != "") %>%
  arrange(municipality) %>%
  dplyr::select(
    current,
    color_incl_new,
    municipality,
    increase_1d,
    increase_7d,
    growth,
  )

temp = list.files(path = "data-rivm/municipal-hospital-datasets/",pattern="*.csv.gz", full.names = T) ## Pull names of all available datafiles

dat.hosp <- fread(temp[length(temp)])
dat.hosp.d1 <- fread(temp[length(temp)-1])
dat.hosp.d7 <- fread(temp[length(temp)-7])
dat.hosp.d8 <- fread(temp[length(temp)-8])
dat.hosp.d14 <- fread(temp[length(temp)-14])

last.date.hosp <- last(dat.hosp$Date_of_statistics)


dat.hosp.d0 <- dat.hosp %>%
  group_by(Municipality_name) %>%
  arrange(Date_of_statistics) %>%
  mutate(d0 = cumsum(Hospital_admission)) %>%
  dplyr::filter(Date_of_statistics == last.date.hosp) %>%
  dplyr::select(Municipality_name, Municipality_code, d0) %>%
  mutate(Municipality_name = replace(Municipality_name, Municipality_name == "","Unknown")) %>%
  mutate(date = last.date.hosp)
dat.hosp.d0 <- rbind(dat.hosp.d0, data.frame("Municipality_name" = "Netherlands","Municipality_code" = "", "d0" = sum(dat.hosp.d0$d0), "date" = last.date.hosp))

dat.hosp.d1 <- dat.hosp %>%
  group_by(Municipality_name) %>%
  arrange(Date_of_statistics) %>%
  mutate(d1 = cumsum(Hospital_admission)) %>%
  dplyr::filter(Date_of_statistics == last.date.hosp-1) %>%
  dplyr::select(Municipality_name, Municipality_code, d1) %>%
  mutate(Municipality_name = replace(Municipality_name, Municipality_name == "","Unknown")) %>%
  mutate(date = last.date.hosp-1)
dat.hosp.d1 <- rbind(dat.hosp.d1, data.frame("Municipality_name" = "Netherlands","Municipality_code" = "", "d1" = sum(dat.hosp.d1$d1), "date" = last.date.hosp))

dat.hosp.d7 <- dat.hosp %>%
  group_by(Municipality_name) %>%
  arrange(Date_of_statistics) %>%
  mutate(d7 = cumsum(Hospital_admission)) %>%
  dplyr::filter(Date_of_statistics == last.date.hosp-7) %>%
  dplyr::select(Municipality_name, Municipality_code, d7) %>%
  mutate(Municipality_name = replace(Municipality_name, Municipality_name == "","Unknown")) %>%
  mutate(date = last.date.hosp-7)
dat.hosp.d7 <- rbind(dat.hosp.d7, data.frame("Municipality_name" = "Netherlands","Municipality_code" = "", "d7" = sum(dat.hosp.d7$d7), "date" = last.date.hosp))

dat.hosp.d8 <- dat.hosp %>%
  group_by(Municipality_name) %>%
  arrange(Date_of_statistics) %>%
  mutate(d8 = cumsum(Hospital_admission)) %>%
  dplyr::filter(Date_of_statistics == last.date.hosp-8) %>%
  dplyr::select(Municipality_name, Municipality_code, d8) %>%
  mutate(Municipality_name = replace(Municipality_name, Municipality_name == "","Unknown")) %>%
  mutate(date = last.date.hosp-8)
dat.hosp.d8 <- rbind(dat.hosp.d8, data.frame("Municipality_name" = "Netherlands","Municipality_code" = "", "d8" = sum(dat.hosp.d8$d8), "date" = last.date.hosp))


dat.hosp.d14 <- dat.hosp %>%
  group_by(Municipality_name) %>%
  arrange(Date_of_statistics) %>%
  mutate(d14 = cumsum(Hospital_admission)) %>%
  dplyr::filter(Date_of_statistics == last.date.hosp-14) %>%
  dplyr::select(Municipality_name, Municipality_code, d14) %>%
  mutate(Municipality_name = replace(Municipality_name, Municipality_name == "","Unknown")) %>%
  mutate(date = last.date.hosp-14) 
dat.hosp.d14 <- rbind(dat.hosp.d14, data.frame("Municipality_name" = "Netherlands","Municipality_code" = "", "d14" = sum(dat.hosp.d14$d14), "date" = last.date.hosp))


daily_datalist_hosp <- list(dat.hosp.d0,dat.hosp.d1,dat.hosp.d7,dat.hosp.d8,dat.hosp.d14)

dat.hosp.all <- Reduce(
  function(x, y, ...) merge(x, y, by=c("Municipality_name","Municipality_code"),all.x = TRUE, ...),
  daily_datalist_hosp
)

dat.hosp.all <- dat.hosp.all %>%
  dplyr::select(1:5, 7,9,11) %>%
  rename(date = date.x) %>%
  mutate(Municipality_name = recode(Municipality_name, "Noardeast-FryslÃ¢n" = "Noardeast-Fryslân",
                                    "SÃºdwest-FryslÃ¢n" = "Súdwest-Fryslân"))
dat.hosp.all <- merge(dat.hosp.all,dat.pop, by = c("Municipality_code"), all.x=T)
dat.hosp.all[dat.hosp.all$Municipality_name=="Netherlands", "population"] <- 17443797
dat.hosp.all[dat.hosp.all$Municipality_name=="Unknown", "population"] <- 17443797

dat.hosp.lowest <- dat.hosp.lowest %>%
  rename(lowest_since_jan1 = Hospital_admission) %>%
  rename(lowest_since_jan1_date = date) %>%
  dplyr::select(Municipality_name, lowest_since_jan1, lowest_since_jan1_date)


dat.hosp.all <- merge(dat.hosp.all, dat.hosp.lowest, by = c("Municipality_name"), all.x=T)

dat.hosp.today <- dat.hosp.all %>%
  rename(municipality = Municipality_name) %>%
  mutate(date = date + 1) %>%
  mutate(jan = lowest_since_jan1) %>%
  mutate(current = d0-lowest_since_jan1) %>%
  mutate(increase_1d = d0 - d1) %>%
  mutate(increase_7d = d0 - d7) %>%
  mutate(increase_14d = d0- d14) %>%
  mutate(increase_growth = calc_growth_increase(increase_7d, increase_14d)) %>%
  mutate(growth = increase_growth_to_arrows(increase_growth, FALSE)) %>%
  mutate(rel_increase_1d = increase_1d / population * 100000) %>%
  mutate(rel_increase_7d = increase_7d / population * 100000) %>%
  mutate(rel_increase_14d = increase_14d / population * 100000) %>%
  mutate(color = convert_to_hosp_trafficlight(rel_increase_7d, rel_increase_14d)) %>%
  mutate(color_incl_new = ifelse(
    ((d1 - d8) <= 0 & (d0 - d1) > 0)
    | ((d0 - d7) <= 0 & (d0 - d1) > 0)
    | ((d1 - d14) <= 0 & (d0 - d1) > 0),  
    emoji.new, color))


dat.hosp.today.simple <- dat.hosp.today %>%
  dplyr::filter(Municipality_code != "") %>%
  arrange(municipality) %>%
  dplyr::select(
    current,
    color_incl_new,
    municipality,
    increase_1d,
    increase_7d,
    growth,
  )

dat.deaths.today <- transmute(dat.deaths,
                              municipality = Municipality_name,
                              Municipality_code = Municipality_code, 
                              date = const.date,
                              d0 = dat.deaths[,ncol(dat.deaths)-date_diff], # today
                              d1 = dat.deaths[,ncol(dat.deaths)-date_diff-1], # yesterday
                              d7 = dat.deaths[,ncol(dat.deaths)-date_diff-7], # last week
                              d8 = dat.deaths[,ncol(dat.deaths)-date_diff-8], # yesterday's last week
                              d14 = dat.deaths[,ncol(dat.deaths)-date_diff-14], # 2 weeks back
                              jan1 = dat.deaths$`2021-01-01`, # Jan 1st, 2021
                              lowest_since_jan1 = dat.deaths.lowest$`Deceased`,
                              lowest_since_jan1_date = dat.deaths.lowest$`date`,
                              current = d0,
                              increase_1d = d0-d1, # Calculate increase since last day
                              increase_7d = d0-d7, # Calculate increase in 7 days
                              increase_14d = d0-d14, # Calculate increase in 14 days
                              increase_growth = calc_growth_increase(increase_7d, increase_14d), # Compare growth of last 7 days vs 7 days before,
                              growth = increase_growth_to_arrows(increase_growth, FALSE),
                              population,
                              rel_increase_1d = increase_1d / population * 100000,
                              rel_increase_7d = increase_7d / population * 100000,
                              rel_increase_14d = increase_14d / population * 100000
)

dat.deaths.today.simple <- dat.deaths.today %>%
  dplyr::filter(Municipality_code != "") %>%
  arrange(municipality) %>%
  dplyr::select(
    current,
    municipality,
    increase_1d,
    increase_7d,
    growth,
  )


# Calculate totals
dat.cases.totals.growth <- dat.cases.today %>%
  dplyr::filter(Municipality_code != "") %>%
  group_by(growth) %>%
  summarise(d0 = n(), .groups = 'drop_last') %>%
  arrange(match(growth, c(emoji.up_double, emoji.up, "-", emoji.down, emoji.down_double)))

dat.cases.totals.color <- dat.cases.today %>%
  dplyr::filter(Municipality_code != "") %>%
  group_by(color) %>%
  summarise(d0 = n(), .groups = 'drop_last')

dat.cases.totals.color_yesterday <- dat.cases.today %>%
  dplyr::filter(Municipality_code != "") %>%
  group_by(color_yesterday) %>%
  summarise(d1 = n(), .groups = 'drop_last') %>%
  rename(color = color_yesterday)

dat.cases.totals.color_lastweek <- dat.cases.today %>%
  dplyr::filter(Municipality_code != "") %>%
  group_by(color_lastweek) %>%
  summarise(d7 = n(), .groups = 'drop_last') %>%
  rename(color = color_lastweek)

colors <- c(emoji.green, emoji.yellow, emoji.orange, emoji.red, emoji.purple, emoji.black)
dat.cases.totals.color <- data.frame("color" = colors)  %>%
  merge(dat.cases.totals.color, by = "color", all.x = TRUE) %>%
  merge(dat.cases.totals.color_yesterday, by = "color", all.x = TRUE) %>%
  merge(dat.cases.totals.color_lastweek, by = "color", all.x = TRUE) %>%
  arrange(match(color, colors))
dat.cases.totals.color[is.na(dat.cases.totals.color)] <- 0

rm(colors, dat.cases.totals.color_yesterday, dat.cases.totals.color_lastweek)

dat.cases.totals.color <- mutate(dat.cases.totals.color,
                                 increase_1d = d0-d1, # Calculate increase since last day
                                 increase_7d = d0-d7, # Calculate increase in 7 days
)

dat.cases_per_death <-transmute(dat.cases,
                                municipality = Municipality_name,
                                Municipality_code = Municipality_code, 
                                date = const.date,
                                population = population,
                                cases_d0  = dat.cases[,ncol(dat.cases)-date_diff],
                                cases_d7 = dat.cases[,ncol(dat.cases)-date_diff-7],
                                cases_d14 = dat.cases[,ncol(dat.cases)-date_diff-14], 
                                rel_cases_7d = (cases_d0 - cases_d7) / population * 100000,
                                rel_cases_14d = (cases_d0 - cases_d14) / population * 100000,
                                rel_cases_total = cases_d0 / population * 100000,
                                color = convert_to_trafficlight(rel_cases_7d),
                                hosp_d0  = dat.hosp[,ncol(dat.hosp)-date_diff], 
                                hosp_d7  = dat.hosp[,ncol(dat.hosp)-date_diff-7], 
                                hosp_d14  = dat.hosp[,ncol(dat.hosp)-date_diff-14], 
                                rel_hosp_7d = (hosp_d0 - hosp_d7) / population * 100000,
                                rel_hosp_14d = (hosp_d0 - hosp_d14) / population * 100000,
                                rel_hosp_total = hosp_d0 / population * 100000,
                                color = convert_to_hosp_trafficlight(rel_hosp_7d, rel_hosp_14d),
                                deaths_d0  = dat.deaths[,ncol(dat.deaths)-date_diff],
                                deaths_d7  = dat.deaths[,ncol(dat.deaths)-date_diff-7],
                                deaths_d14  = dat.deaths[,ncol(dat.deaths)-date_diff-14],
                                rel_deaths_7d = (deaths_d0 - deaths_d7) / population * 100000,
                                rel_deaths_14d = (deaths_d0 - deaths_d14) / population * 100000,
                                rel_deaths_total = deaths_d0 / population * 100000,
                                perc_cases = cases_d0 / population * 100,
                                perc_hosp = hosp_d0 / population * 100,
                                perc_deaths = deaths_d0 / population * 100, 
                                perc_deaths_per_case = deaths_d0 / cases_d0 * 100
) %>% mutate(across(where(is.numeric), round, 1))


# Write to csv
write.csv(dat.cases_per_death,    file = "data/municipality-combined.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.cases.today,        file = "data/municipality-today-detailed.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.cases.today.simple, file = "data/municipality-today.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.hosp.today,         file = "data/municipality-hospitalisations-today-detailed.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.hosp.today.simple,  file = "data/municipality-hospitalisations-today.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.deaths.today,       file = "data/municipality-deaths-today-detailed.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.deaths.today.simple,file = "data/municipality-deaths-today.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.cases.totals.growth,file = "data/municipality-totals-growth.csv",row.names = F, fileEncoding = "UTF-8")
write.csv(dat.cases.totals.color, file = "data/municipality-totals-color.csv",row.names = F, fileEncoding = "UTF-8")

rm(const.date, const.date_hosp)
