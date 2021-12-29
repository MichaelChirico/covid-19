vaccines <- fread("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv")

vaccines.nl <- vaccines %>%
  dplyr::filter(Region == "NL") %>%
  select(YearWeekISO,Vaccine,FirstDose, SecondDose,DoseAdditional1,TargetGroup)
 
vaccines.nl <- aggregate(cbind(FirstDose, SecondDose,DoseAdditional1) ~ YearWeekISO + Vaccine, data = vaccines.nl, FUN = sum)
 
vaccines.nl.long <- vaccines.nl %>%
  gather(key = "dose_number", value = "total_administered",
         FirstDose, SecondDose,DoseAdditional1)


vaccines.nl.long <- vaccines.nl.long %>%
  mutate(vaccine = recode(Vaccine, "JANSS" = "Johnson&Johnson",
                          "COM" = "Pfizer/BioNTech",
                          "MOD" = "Moderna",
                          "AZ" = "Oxford/AstraZeneca")) %>%
  mutate(dose_number = parse_number(recode(dose_number, "FirstDose" = "1",
                              "SecondDose" = "2",
                              "DoseAdditional1" = "3"))) %>%  
  mutate(week = parse_number(str_sub(YearWeekISO, 7, 8))) %>%
  mutate(year = parse_number(str_sub(YearWeekISO, 1,4))) %>%
  select(week, year, vaccine, dose_number, total_administered)

write.csv(vaccines.nl.long, file = "data-rivm/vaccines-ecdc/vaccines_administered_nl.csv",row.names=F)

add(repo, path = "data-rivm/vaccines-ecdc/vaccines_administered_nl.csv")
commit(repo, all = T, paste0("[", Sys.Date(), "] Daily (automated) update vaccine data - ECDC"))
push(repo, credentials = git.auth)
