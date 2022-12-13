repeat {
  Sys.sleep(5)
  lcps.data.original <- utils::read.csv('https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv', sep =',')
  
  lcps.data <- lcps.data.original %>%
    mutate(
      date = as.Date(datum, tryFormats = c('%d-%m-%Y')),
      .before = datum
    ) %>%
    mutate(
      datum = NULL
    )
  
  date.lcps <- first(lcps.data$date)
  date.now <- as.Date(Sys.Date())
  if (date.lcps == date.now){
    message <- "GO GO GO GO GO"
    break
  }
}

rm(lcps.data, lcps.data.original, date.lcps, date.now, message)

lcps.data.original <- utils::read.csv('https://lcps.nu/wp-content/uploads/covid-19-datafeed.csv', sep =',')

#lcps_old <- read.csv("data-lcps/total/covid-19_2022-12-07.csv")
#lcps_old[943,] <- c("08-12-2022",35,0, 705, 593, 3, 80)
#lcps_old[944,] <- c("09-12-2022",31,0, 738, 595, 3, 92)

# Order numbers: IC_Bedden_COVID, IC_Bedden_Non_COVID, Kliniek_Bedden, IC_Nieuwe_Opnames_COVID, Kliniek_Nieuwe_Opnames_COVID
lcps.data <- lcps.data.original %>%
  mutate(
    date = as.Date(datum, tryFormats = c('%d-%m-%Y')),
    .before = datum
  ) %>%
  mutate(
    datum = NULL
  )

#lcps.data <- lcps.data %>%
#  mutate_at(c("IC_Bedden_COVID_Nederland","IC_Bedden_COVID_Internationaal","IC_Bedden_Non_COVID_Nederland","Kliniek_Bedden_Nederland","IC_Nieuwe_Opnames_COVID_Nederland","Kliniek_Nieuwe_Opnames_COVID_Nederland"), as.numeric)


lcps.condition <- head(lcps.data$kliniek_opnames_covid,1) < head(lcps.data$IC_opnames_covid,1)
# Verify clinical beds and IC beds are correctly reported (not swapped around)
if (lcps.condition) {stop("The value is TRUE, so the script must end here")    
} else {
  
  lcps.dailydata <- lcps.data %>%
    head(1)
  lcps.date <- lcps.dailydata[['date']]
  
  filename <- paste0('data-lcps/total/covid-19_', lcps.date, '.csv')
  filename.daily <- paste0('data-lcps/data-per-day/covid-19_', lcps.date, '.csv')
  filename.common <- 'data/lcps_by_day.csv'
  
  lcps.data <- lcps.data[order(lcps.data$date),]
  
  lcps.data <- pad(lcps.data)
  
  lcps.data <- lcps.data %>%
    mutate(Kliniek_Bedden_Nederland = kliniek_bezetting_covid + kliniek_bezetting_ontlabeld) %>%
    mutate(IC_Bedden_COVID_Nederland = IC_bezetting_covid + IC_bezetting_ontlabeld) %>%
    mutate(Totaal_Bezetting = Kliniek_Bedden_Nederland + IC_Bedden_COVID_Nederland) %>%
    mutate(IC_Opnames_7d = frollmean(IC_opnames_covid,7, na.rm=T)) %>%
    mutate(Kliniek_Opnames_7d = frollmean(kliniek_opnames_covid,7, na.rm=T)) %>%
    mutate(Totaal_opnames = IC_opnames_covid + kliniek_opnames_covid) %>%
    mutate(Totaal_opnames_7d = IC_Opnames_7d + Kliniek_Opnames_7d) %>%
    mutate(Totaal_IC = IC_Bedden_COVID_Nederland + IC_bezetting_covid_internationaal) %>%
    mutate(IC_opnames_14d = dplyr::lag(IC_Opnames_7d,7)) %>%
    mutate(Kliniek_opnames_14d = dplyr::lag(Kliniek_Opnames_7d,7)) %>%
    mutate(OMT_Check_IC = round(IC_Opnames_7d/IC_opnames_14d*100,1)) %>%
    mutate(OMT_Check_Kliniek = round(Kliniek_Opnames_7d/Kliniek_opnames_14d*100,1)) %>%
    mutate(Kliniek_Bedden_7d = frollmean(Kliniek_Bedden_Nederland,7, na.rm=T)) %>%
    mutate(IC_Bedden_7d = frollmean(IC_Bedden_COVID_Nederland,7, na.rm=T)) %>%
    mutate(Totaal_Bedden_7d = frollmean(Totaal_Bezetting,7, na.rm=T)) %>%
    mutate(IC_Bedden_14d = dplyr::lag(IC_Bedden_7d,7)) %>%
    mutate(Kliniek_Bedden_14d = dplyr::lag(Kliniek_Bedden_7d,7)) %>%
    mutate(Totaal_Bedden_14d = dplyr::lag(Totaal_Bedden_7d,7)) %>%
    mutate(OMT_Check_IC_Bezetting = round(IC_Bedden_7d/IC_Bedden_14d*100,1)) %>%
    mutate(OMT_Check_Kliniek_Bezetting = round(Kliniek_Bedden_7d/Kliniek_Bedden_14d*100,1)) %>%
    mutate(OMT_Check_Totaal_Bezetting = round(Totaal_Bedden_7d/Totaal_Bedden_14d*100,1))
    
  
  
  lcps.data <- lcps.data[order(lcps.data$date, decreasing = T),]
  
  write.csv(lcps.data.original, file=filename, row.names = F)
  write.csv(lcps.dailydata, file = filename.daily, row.names = F)
  write.csv(lcps.data, file = filename.common, row.names = F)
  
}

bot <- TGBot$new(token = bot_token('RBot'))
bot$set_default_chat_id(user_id('me'))
bot$sendMessage((lcps.data$kliniek_opnames_covid[1]))

rm(filename, filename.common, filename.daily, lcps.condition, lcps.date, lcps.dailydata, lcps.data.original,bot)


