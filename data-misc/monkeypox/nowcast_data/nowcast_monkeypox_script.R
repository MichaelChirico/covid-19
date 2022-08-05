monkeypox.nowcast <- fread("data-misc/monkeypox/nowcast_data_monkeypox.csv")


monkeypox.nowcast <- monkeypox.nowcast %>%
  mutate(date_symptoms = as.Date(`Eerste ziektedag`, tryFormats = c('%d/%m/%Y'))) %>%
  mutate(date_report = as.Date(`Meldingsdatum`, tryFormats = c('%d/%m/%Y'))) %>%
  mutate(`Eerste ziektedag` = NULL) %>%
  mutate(Meldingsdatum = NULL) %>%
  mutate(Report_round = NULL) %>%
  mutate(Infecties_toename = ifelse(Toename < 0, 0, Toename)) %>%
  mutate(Toename = NULL)
str(monkeypox.nowcast)

d11 <- monkeypox.nowcast[rep(1:.N, Infecties_toename)][, Infecties_toename := rowidv(.SD), .SDcols = !'Infecties_toename'][]

d12 <- d11 %>%
  dplyr::select(date_symptoms,date_report) %>%
  mutate(dates_delay = as.numeric(date_report - date_symptoms))

fwrite(d12, "data-misc/monkeypox/nowcast_data/nowcast_data_parsed.csv")
