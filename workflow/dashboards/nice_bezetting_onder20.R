temp = list.files(path = "data-nice/age/Clinical_Beds/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, read.csv) ## Load all day files

nice_by_day_clinical <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nice_by_day_clinical <- nice_by_day_clinical %>%
  dplyr::filter(Leeftijd == "<20")


temp = list.files(path = "data-nice/age/IC/",pattern="*.csv", full.names = T) ## Fetch all day files
myfiles = lapply(temp, read.csv) ## Load all day files


nice_by_day_ic <- map_dfr(myfiles, ~{ ## Write dataframe of all day files
  .x
})

nice_by_day_ic <- nice_by_day_ic %>%
  dplyr::filter(Leeftijd == "<20")


write.csv(nice_by_day_clinical,"data-dashboards/bezetting_klinisch_onder20.csv", row.names = F)
write.csv(nice_by_day_ic,"data-dashboards/bezetting_ic_onder20.csv", row.names = F)

## Push to github
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("Update NICE age-distribution in hospital (regular and IC)",Sys.Date()))
git2r::push(repo, credentials = git.auth)
