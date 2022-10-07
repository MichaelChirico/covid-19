vaccines <- fread("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv")

vaccines.nl <- vaccines %>%
  dplyr::filter(Region == "NL") %>%
  dplyr::select(YearWeekISO,Vaccine,NumberDosesReceived, FirstDose, SecondDose,DoseAdditional1,DoseAdditional2, UnknownDose,TargetGroup)
 
vaccines.nl <- aggregate(cbind(NumberDosesReceived, FirstDose, SecondDose,DoseAdditional1,DoseAdditional2, UnknownDose) ~ YearWeekISO + Vaccine, data = vaccines.nl, FUN = sum)
 
vaccines.nl.long <- vaccines.nl %>%
  gather(key = "dose_number", value = "total_administered",
         NumberDosesReceived, FirstDose, SecondDose,DoseAdditional1,DoseAdditional2, UnknownDose)


vaccines.nl.long <- vaccines.nl.long %>%
  mutate(vaccine = recode(Vaccine, "JANSS" = "Johnson&Johnson",
                          "COM" = "Pfizer/BioNTech",
                          "MOD" = "Moderna",
                          "AZ" = "Oxford/AstraZeneca",
                          "NVXD" = "Novavax",
                          "UNK" = "Unknown")) %>%
  mutate(dose_number = parse_number(recode(dose_number, "FirstDose" = "1",
                              "SecondDose" = "2",
                              "DoseAdditional1" = "3",
                              "DoseAdditional2" = "4",
                              "NumberDosesReceived" = "99",
                              "UnknownDose" = "6"))) %>%  
  mutate(week = parse_number(str_sub(YearWeekISO, 7, 8))) %>%
  mutate(year = parse_number(str_sub(YearWeekISO, 1,4))) %>%
  dplyr::select(week, year, vaccine, dose_number, total_administered)

vaccines.ndr <- vaccines.nl.long %>%
  dplyr::filter(dose_number == 99)

write.csv(vaccines.nl.long, file = "data-rivm/vaccines-ecdc/vaccines_administered_nl.csv",row.names=F)

add(repo, path = "data-rivm/vaccines-ecdc/vaccines_administered_nl.csv")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) update vaccine data - ECDC"))
push(repo, credentials = git.auth)
