temp = list.files(path = "data-rivm/vaccine-neighborhood/",pattern="*.csv.gz", full.names = T) ## Pull names of all available datafiles

vaxx.wijk <- fread(last(temp))

vaxx.wijk <- vaxx.wijk %>%
  dplyr::filter(Region_code != "") %>% # Filter observations without municipal name
  dplyr::select(
    Region_name, 
    Region_code,
    Date_of_statistics, 
    Populatie_merged,
    Coverage_primary_partly,
    Coverage_primary_completed
  )

vaxx.wijk.partial <- vaxx.wijk %>%
  dplyr::select(
    Region_name, 
    Region_code,
    Date_of_statistics,
    Coverage_primary_partly,
    Populatie_merged
  ) %>%
  mutate(Coverage_primary_partly = parse_number(Coverage_primary_partly))

vaxx.wijk.completed <- vaxx.wijk %>%
  dplyr::select(
    Region_name, 
    Region_code,
    Date_of_statistics,
    Coverage_primary_completed,
    Populatie_merged
  ) %>%
  mutate(Coverage_primary_completed = parse_number(Coverage_primary_completed))

vaxx.wijk.partial <- vaxx.wijk.partial %>%
  distinct() %>%
  pivot_wider(
    values_from = Coverage_primary_partly,
    names_from = Date_of_statistics,
    values_fill = 0
  )

vaxx.wijk.completed <- vaxx.wijk.completed %>%
  distinct() %>%
  pivot_wider(
    values_from = Coverage_primary_completed,
    names_from = Date_of_statistics,
    values_fill = 0
  )

fwrite(vaxx.wijk.partial,"data/vaccination-neighborhood-partly.csv",row.names=F)
fwrite(vaxx.wijk.completed,"data/vaccination-neighborhood-completed.csv",row.names=F)

## Push to git
repo <- git2r::init()
add(repo, path = "*")
commit(repo, all = T, paste0("[", Sys.Date(), "] Weekly update vaccination rate per neighborhood"))
git2r::push(repo, credentials = git.auth)