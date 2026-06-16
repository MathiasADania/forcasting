# Upload af pakker
pacman::p_load(tidyverse, gghighlight, hrbrthemes, tsibble, lubridate, 
               ggplot2, feasts, fable, tsibbledata, forecast, fpp3,
               fabletools, knitr, ggpubr, gridExtra, GGally, slider,
               forecast, car, shiny, glue, broom, kableExtra, gt, seasonal, 
               latex2exp, ggfortify, janitor, readxl)

# Indhentning af data
jernbanedata <- read_excel("data/jernbanetransport af passagerer.xlsx",
                           sheet = "BANE25"
                           )

# Dataforberedelse --------------------------------------------------------

# Her forberedes datasættet - Der vælgea index, key og respons bliver
# passagerer. Derudover udvælges relevant key-kategoerier

jernbanedata <- jernbanedata %>%
  clean_names() %>%
  mutate(kvartal = str_replace(kvartal, "K", "Q")) %>%
  mutate(kvartal = yearquarter(kvartal)) %>%
  filter(key %in% c("International trafik i alt", "Over Storebælt")) %>%
  as_tsibble(index = kvartal, key = key)

# Der laves også et datasat, som ikke er inkl. coronaperiode, da de ikke regnes
# med at være et gentagende fænomen - her anvendes forcast::na.interp() til
# at udfylde NA-felterne med punkter ud fra en lineær punkt mellem før perioden
# og til efter.
jernbanedata_corona <- jernbanedata %>%
  filter_index(. ~ "2019 Q4", "2022 Q2" ~ .) %>%
  tsibble::fill_gaps() %>%
  group_by_key() %>%
  mutate(x1000_passagerer = forecast::na.interp(x1000_passagerer)) %>%
  ungroup()


# Tidsserieplot -----------------------------------------------------------

# Her anvendes der autoplot() til at automatisk skabe et visuelt billede af 
# tidsserien

# Autoplot af passagertal inkl. corona
jernbanedata %>%
  autoplot(x1000_passagerer) +
  labs(title = "Jernbanepassagerer inkl. corona",
       y = "1.000 passagerer", x = NULL)
# Her ses det hvordan der er et sæsonbetonet mønster ved International trafik
# i alt. og ved kategorien "Over Storebælt"


# Rå passagertal uden corona
jernbanedata_corona %>%
  autoplot(x1000_passagerer) +
  labs(title = "Jernbanepassagerer uden corona",
       y = "1.000 passagerer", x = NULL)

# Log-transformeret (stabiliserer varians)
# Her anvendes der LOG() funktionen til at stabilisere variansen. Dette kan være
# vanskligt i modeller der antager konstant varians(homoskedasticitet)
jernbanedata %>%
  autoplot(log(x1000_passagerer)) +
  labs(title = "Log-transformerede jernbanepassagerer inkl. corona",
       y = "log(1.000 passagerer)", x = NULL)

jernbanedata_corona %>%
autoplot(log(x1000_passagerer)) +
  labs(title = "Log-transformerede jernbanepassagerer uden corona",
       y = "log(1.000 passagerer)", x = NULL)


# Sæsonplot ---------------------------------------------------------------

#gg_season() undersøges de underliggende sæson mønster og man kan se udviklingen
# på årsbasis.

# Sæsonplot inkl. corona
jernbanedata %>%
  gg_season(x1000_passagerer) +
  labs(title = "Sæsonplot inkl. corona",
       y = "1.000 passagerer", x = NULL)

# Sæsonplot uden corona
jernbanedata_corona %>%
  gg_season(x1000_passagerer) +
  labs(title = "Sæsonplot uden corona",
       y = "1.000 passagerer", x = NULL)

# gg_subseries() anvendes til at se det gernerelle mønster der sker pr. kvartal

# Subserie-plot inkl. corona
jernbanedata %>%
  gg_subseries(x1000_passagerer) +
  labs(title = "Subserie-plot inkl. corona",
       y = "1.000 passagerer", x = NULL)

# Subserie-plot uden corona
jernbanedata_corona %>%
  gg_subseries(x1000_passagerer) +
  labs(title = "Subserie-plot uden corona",
       y = "1.000 passagerer", x = NULL)

# ACF + PACF (gg_tdisplay) ------------------------------------------------
# Er der grund til kun at anvende ACF når begge er inkluderet lige her?

# ACF måler korrelationen mellem en tidsserie og lag-værdierne. Funktionen
# fortæller om de tidligere observationer kan hjælpe med at forklare de
# nuværende.

# PACF viser, hvor mange tidligere værdier der direkte påvirker den nuværende
# obseration - Når PACF ikke er signifikant finder man AR_orden p.

# International trafik i alt - log-skala
jernbanedata %>%
  filter(key == "International trafik i alt") %>%
  mutate(x1000_passagerer = log(x1000_passagerer)) %>%
  gg_tsdisplay(x1000_passagerer, plot_type = "partial", lag_max = 24) +
  labs(title = "ACF + PACF – International trafik i alt (log, inkl. corona)")

# Over Storebælt - log-skala
jernbanedata %>%
  filter(key == "Over Storebælt") %>%
  mutate(x1000_passagerer = log(x1000_passagerer)) %>%
  gg_tsdisplay(x1000_passagerer, plot_type = "partial", lag_max = 24) +
  labs(title = "ACF + PACF – Over Storebælt (log, inkl. corona)")

# International trafik i alt - uden corona
jernbanedata_corona %>%
  filter(key == "International trafik i alt") %>%
  mutate(x1000_passagerer = log(x1000_passagerer)) %>%
  gg_tsdisplay(x1000_passagerer, plot_type = "partial", lag_max = 24) +
  labs(title = "ACF + PACF – International trafik i alt (log, uden corona)")

# Over Storebælt - uden corona
jernbanedata_corona %>%
  filter(key == "Over Storebælt") %>%
  mutate(x1000_passagerer = log(x1000_passagerer)) %>%
  gg_tsdisplay(x1000_passagerer, plot_type = "partial", lag_max = 24) +
  labs(title = "ACF + PACF – Over Storebælt (log, uden corona)")

# gg_lag() viser et scatterplot, hvor man kan aflæse om der er lineær sammenhæng
# mellem en obseration og de tidligere værdier. jo tydligere lineær sammenhæng
# der er desto større auto korrelation er der.


# coronaperiode
jernbanedata %>%
  filter(key == "Over Storebælt") %>%
  gg_lag(x1000_passagerer, geom = "point") +
  labs(x = "lag(x1000_passagerer, k)")

jernbanedata %>%
  filter(key == "International trafik i alt") %>%
  gg_lag(x1000_passagerer, geom = "point") +
  labs(x = "lag(x1000_passagerer, k)")

# med coronaperiode
jernbanedata_corona %>%
  filter(key == "Over Storebælt") %>%
  gg_lag(x1000_passagerer, geom = "point") +
  labs(x = "lag(x1000_passagerer, k)")

jernbanedata_corona %>%
  filter(key == "International trafik i alt") %>%
  gg_lag(x1000_passagerer, geom = "point") +
  labs(x = "lag(x1000_passagerer, k)")

# Deskriptive statistikker ------------------------------------------------

# Der anvendes følgende metrikker til at hurtigt skabe et overblik over
# fordelingen af passagertal for hver serie

# Med corona
jernbanedata %>%
  as_tibble() %>%
  group_by(key) %>%
  summarise(
    N      = sum(!is.na(x1000_passagerer)),
    mean   = mean(x1000_passagerer, na.rm = TRUE),
    median = median(x1000_passagerer, na.rm = TRUE),
    sd     = sd(x1000_passagerer, na.rm = TRUE),
    Q1     = quantile(x1000_passagerer, 0.25, na.rm = TRUE),
    Q3     = quantile(x1000_passagerer, 0.75, na.rm = TRUE),                  
    min    = min(x1000_passagerer, na.rm = TRUE),
    max    = max(x1000_passagerer, na.rm = TRUE)
  ) %>%
  kbl(caption = "Deskriptive statistikker inkl. corona", digits = 1) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Uden corona
jernbanedata_corona %>%
  as_tibble() %>%
  group_by(key) %>%
  summarise(
    N      = sum(!is.na(x1000_passagerer)),
    mean   = mean(x1000_passagerer, na.rm = TRUE),
    median = median(x1000_passagerer, na.rm = TRUE),
    sd     = sd(x1000_passagerer, na.rm = TRUE),
    Q1     = quantile(x1000_passagerer, 0.25, na.rm = TRUE),
    Q3     = quantile(x1000_passagerer, 0.75, na.rm = TRUE),                  
    min    = min(x1000_passagerer, na.rm = TRUE),
    max    = max(x1000_passagerer, na.rm = TRUE)
  ) %>%
  kbl(caption = "Deskriptive statistikker uden corona", digits = 1) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# STL features - trend og sæsonstyrke
# Her beregnes trend-styrken og sæson-styrken
# De hjælper med at argumentere for hvilke modelkomponenter der er nødvendige.
# En høj sæsonstyrke betyder eksempelvis at en sæsonel model (SARIMA eller ETS 
# med sæson) er påkrævet.
jernbanedata %>%
  features(x1000_passagerer, feat_stl) %>%
  kbl(caption = "Trend- og sæsonstyrke inkl. corona", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

jernbanedata_corona %>%
  features(x1000_passagerer, feat_stl) %>%
  kbl(caption = "Trend- og sæsonstyrke uden corona", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Transformation ----------------------------------------------------------

# Box-Cox / Guerrero

# Guerrero finder automatisk den optimale lambda værdi

# Box-Cox er en transformation der stabiliserer variansen i serien, og er styret
# af lamda. Her vil udslagene være lige store gennem hele perioden.

# Guerrero optimal lambda per serie
jernbanedata %>%
  features(x1000_passagerer, features = guerrero) %>%
  rename(Lambda_Guerrero = lambda_guerrero) %>%
  kbl(caption = "Guerrero-optimeret Box-Cox lambda inkl. corona",
      digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

jernbanedata_corona %>%
  features(x1000_passagerer, features = guerrero) %>%
  rename(Lambda_Guerrero = lambda_guerrero) %>%
  kbl(caption = "Guerrero-optimeret Box-Cox lambda uden corona",
      digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Rå vs. log-transformeret - inkl. corona
bind_rows(
  jernbanedata %>%
    as_tibble() %>%
    mutate(type = "Rå passagertal"),
  jernbanedata %>%
    as_tibble() %>%
    mutate(x1000_passagerer = log(x1000_passagerer),
           type = "log(passagerer)")
) %>%
  ggplot(aes(x = kvartal, y = x1000_passagerer)) +
  geom_line() +
  facet_grid(type ~ key, scales = "free_y") +
  labs(title = "Rå vs. log-transformeret inkl. corona",
       y = NULL, x = NULL)

# Rå vs. log-transformeret - uden corona 
# Dette bør ikke være med (Guerrero viser det ikke skal kommenteres på)
bind_rows(
  jernbanedata_corona %>%
    as_tibble() %>%
    mutate(type = "Rå passagertal"),
  jernbanedata_corona %>%
    as_tibble() %>%
    mutate(x1000_passagerer = log(x1000_passagerer),
           type = "log(passagerer)")
) %>%
  ggplot(aes(x = kvartal, y = x1000_passagerer)) +
  geom_line() +
  facet_grid(type ~ key, scales = "free_y") +
  labs(title = "Rå vs. log-transformeret uden corona",
       y = NULL, x = NULL)


# Stationaritet og differentiering ----------------------------------------
# KPSS er en statistisk test der afgør om en tidsserie er stationær eller ej.
# KPSS-test: H0 = stationær, p < 0.05 → differentiering nødvendig
jernbanedata %>%
  features(log(x1000_passagerer), unitroot_kpss) %>%
  kbl(caption = "KPSS-test på log(passagerer) — H0: stationær",
      digits = 4) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# nsdiffs fortæller hvor mange sæsondifferentieringerne(D) der er nødvendige for
# at gøre serien stationær. Returnerer den 1, skal der differentieres for at
# fjerne sæsonmønsteret.

# nsdiffs: anbefalet antal sæsondifferentieringer (D) - lag=4 ved kvartalsdata
jernbanedata %>%
  features(log(x1000_passagerer), unitroot_nsdiffs) %>%
  kbl(caption = "Anbefalet antal sæsondifferentieringer (D)",
      digits = 0) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# ndiffs: anbefalet antal ordinære differentieringer (d)
jernbanedata %>%
  features(log(x1000_passagerer), unitroot_ndiffs) %>%
  kbl(caption = "Anbefalet antal ordinære differentieringer (d)",
      digits = 0) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# ndiffs efter sæsondifferentiering
jernbanedata %>%
  mutate(sdiff_log = difference(log(x1000_passagerer), lag = 4)) %>%
  features(sdiff_log, unitroot_ndiffs) %>%
  kbl(caption = "Anbefalet d efter sæsondifferentiering (D=1)",
      digits = 0) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Figur 17: Alle differentieringstrin samlet - International
jernbanedata %>%
  filter(key == "International trafik i alt") %>%
  as_tibble() %>%
  transmute(
    kvartal,
    `log(passagerer)`     = log(x1000_passagerer),
    `Δlog (d=1)`           = difference(log(x1000_passagerer), 1),
    `Δ₄log (D=1, lag=4)`  = difference(log(x1000_passagerer), 4),
    `ΔΔ₄log (d=1, D=1)`   = difference(difference(log(x1000_passagerer), 4), 1)
  ) %>%
  pivot_longer(-kvartal, names_to = "Type", values_to = "Værdi") %>%
  mutate(Type = factor(Type, levels = c(
    "log(passagerer)", "Δlog (d=1)", "Δ₄log (D=1, lag=4)", "ΔΔ₄log (d=1, D=1)"
  ))) %>%
  ggplot(aes(x = kvartal, y = Værdi)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Figur 17: Differentieringstrin – International trafik i alt",
       y = NULL, x = NULL)

# ACF/PACF efter sæsondifferentiering - International
jernbanedata %>%
  filter(key == "International trafik i alt") %>%
  gg_tsdisplay(difference(log(x1000_passagerer), lag = 4),
               plot_type = "partial", lag_max = 24) +
  labs(title = "ACF/PACF – sæsondiff (D=1, lag=4) – International")

# ACF/PACF efter sæsondifferentiering - Over Storebælt
jernbanedata %>%
  filter(key == "Over Storebælt") %>%
  gg_tsdisplay(difference(log(x1000_passagerer), lag = 4),
               plot_type = "partial", lag_max = 24) +
  labs(title = "ACF/PACF – sæsondiff (D=1, lag=4) – Over Storebælt")

# Dobbelt differentiering udelades da unitroot_ndiffs returnerer d=0 efter D=1
# Overdifferentiering forværrer modellen - derfor kun D=1

# STL-dekomposition -------------------------------------------------------

# STL inkl. corona
stl_comp <- jernbanedata %>%
  model(
    STL(log(x1000_passagerer) ~ trend(window = 7) + season(window = "periodic"),
        robust = TRUE)
  ) %>%
  components()

# STL uden corona
stl_comp_corona <- jernbanedata_corona %>%
  model(
    STL(log(x1000_passagerer) ~ trend(window = 7) + season(window = "periodic"),
        robust = TRUE)
  ) %>%
  components()

# STL Visualisering
stl_comp %>% 
  as_tsibble() %>% 
  autoplot(`log(x1000_passagerer)`, color = 'gray') +
  geom_line(aes(y = season_adjust), colour = '#0072B2')

stl_comp_corona %>% 
  as_tsibble() %>% 
  autoplot(`log(x1000_passagerer)`, color = 'gray') +
  geom_line(aes(y = season_adjust), colour = '#0072B2')

# STL 4-panel dekompositionsplot – inkl. corona
stl_comp %>%
  autoplot() +
  scale_color_discrete(labels = c("International trafik i alt", "Over Storebælt")) +
  labs(title = "STL-dekomposition – inkl. corona (log-skala)",
       color = "Serie")

# STL 4-panel dekompositionsplot – uden corona
stl_comp_corona %>%
  autoplot() +
  scale_color_discrete(labels = c("International trafik i alt", "Over Storebælt")) +
  labs(title = "STL-dekomposition – uden corona (log-skala)",
       color = "Serie")

# Augment
# Uden coronaperiode
model_jernbane <- jernbanedata %>%
  model(
    STL = STL(log(x1000_passagerer) ~ trend(window = 7) + season(window = "periodic"),
        robust = TRUE)
  ) %>%
  augment()

# Med coronaperiode
model_jernbane_corona <- jernbanedata_corona %>%
  model(
    STL = STL(log(x1000_passagerer) ~ trend(window = 7) + season(window = "periodic"),
        robust = TRUE)
  ) %>%
  augment()

# Dette kan man gå ind og kommentere - tjek hurtig på google og kommenter med
# 2 linjer

# Ændre navne på plot
model_jernbane%>% 
  autoplot()

# Ændre navne på plot
model_jernbane_corona %>% 
  autoplot()

# Nu er det muligt at beregtne RMSE - Vær forsigtig at kører det hvis det
# tager lang tid

# EDA opsummering / del konklussion

# Andre feauteres???

# Tjek evt .innov med graf(er)

# Nu er det muligt at beregtne RMSE - Vær forsigtig at kører det hvis det
# tager lang tid

# Opsummering / Delkonklussion
# Guerrero estimerer lambda ved at minimere variationskoefficenten på tværs af sæsoner
# et lambda tæt på 0 bekræfter at log er det rettevalg mens lambda tæt på 1
# Betyder ingen transformation er nødvendig.

# Træningssplit -----------------------------------------------------------

# Her deler man 
# Træningsdata: 2006 Q1 til 2024 Q4
jernbane_train <- jernbanedata %>%
  filter_index(. ~ "2024 Q4")

# Testdata: 2025 Q1 til slutningen
jernbane_test <- jernbanedata %>%
  filter_index("2025 Q1" ~ .)

jernbane_corona_train <- jernbanedata_corona %>%
  filter_index(. ~ "2024 Q4")

# Testdata: 2025 Q1 til slutningen
jernbane_corona_test <- jernbanedata_corona %>%
  filter_index("2025 Q1" ~ .)

# Benchmark ---------------------------------------------------------------
# Benchmark model
jernbanestretch <- jernbanedata %>%
  stretch_tsibble(.init = 32, .step = 1)

jernbanestretch_corona <- jernbanedata_corona %>%
  stretch_tsibble(.init = 32, .step = 1)
# Nu er det muligt at beregtne RMSE - Vær forsigtig at køre det hvis det
# tager lang tid

jernbanestretch %>% 
  model(SNAIVE(log(x1000_passagerer))) %>%
  forecast(h = 4) %>%
  accuracy(jernbanedata)

# Nu er det muligt at beregtne RMSE - Vær forsigtig at kører det hvis det
# tager lang tid

# Benchmark model
jernbanestretch_corona %>%
  model(SNAIVE(log(x1000_passagerer))) %>%
  forecast(h = 4) %>%
  accuracy(jernbanedata_corona)

# ETS modeller
fit_ets <- jernbane_train %>%
  model(
    Auto   = ETS(log(x1000_passagerer)),
    AAA    = ETS(log(x1000_passagerer) ~ error("A") + trend("A")  + season("A")),
    AAdA   = ETS(log(x1000_passagerer) ~ error("A") + trend("Ad") + season("A")),
    MAM    = ETS(log(x1000_passagerer) ~ error("M") + trend("A")  + season("M"))
  )

fit_ets_corona <- jernbane_corona_train %>%
  model(
    Auto   = ETS(log(x1000_passagerer)),
    AAA    = ETS(log(x1000_passagerer) ~ error("A") + trend("A")  + season("A")),
    AAdA   = ETS(log(x1000_passagerer) ~ error("A") + trend("Ad") + season("A")),
    MAM    = ETS(log(x1000_passagerer) ~ error("M") + trend("A")  + season("M"))
  )

report(fit_ets)
report(fit_ets_corona)

# Tabel: ETS sammenligning inkl. corona
glance(fit_ets) %>%
  arrange(AICc) %>%
  kbl(caption = "Tabel 7a: ETS-modelsammenligning inkl. corona (AICc)", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel: ETS sammenligning uden corona
glance(fit_ets_corona) %>%
  arrange(AICc) %>%
  kbl(caption = "Tabel 7b: ETS-modelsammenligning uden corona (AICc)", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Delkonklusion - kommenter på performance

# Her må vi gerne kommenter kort på nogle af tingene - Hvis det mer forklaring
# gerne alpha, beta, gamma - evt. phi

# Residualanalyse – inkl. corona
fit_ets %>%
  filter(key == "International trafik i alt") %>%
  select(Auto) %>%
  gg_tsresiduals(lag_max = 16)

fit_ets %>%
  filter(key == "Over Storebælt") %>%
  select(Auto) %>%
  gg_tsresiduals(lag_max = 16)

# Residualanalyse – uden corona
fit_ets_corona %>%
  filter(key == "International trafik i alt") %>%
  select(Auto) %>%
  gg_tsresiduals(lag_max = 16)

fit_ets_corona %>%
  filter(key == "Over Storebælt") %>%
  select(Auto) %>%
  gg_tsresiduals(lag_max = 16)

# Ljung-Box – inkl. corona
augment(fit_ets) %>%
  filter(.model == "Auto", key == "International trafik i alt") %>%
  features(.innov, ljung_box, lag = 8, dof = 0)

augment(fit_ets) %>%
  filter(.model == "Auto", key == "Over Storebælt") %>%
  features(.innov, ljung_box, lag = 8, dof = 0)

# Ljung-Box – uden corona
augment(fit_ets_corona) %>%
  filter(.model == "Auto", key == "International trafik i alt") %>%
  features(.innov, ljung_box, lag = 8, dof = 0)

augment(fit_ets_corona) %>%
  filter(.model == "Auto", key == "Over Storebælt") %>%
  features(.innov, ljung_box, lag = 8, dof = 0) # Altid .innov for at tjekke

# forslag til hvordan vi kan forbedre forcast ift - Corona (Hvad kan vi gøre
# anderledes)

# Arima modeller
# Uden corona
fit_arima <- jernbane_train %>%
  model(
    Auto = ARIMA(log(x1000_passagerer)),
    # Klassisk "airline model" – MA(1) ordinær + sæsonel
    M1   = ARIMA(log(x1000_passagerer) ~ 0 + pdq(0,1,1) + PDQ(0,1,1)),
    # AR(1) ordinær + sæsonel AR(1)
    M2   = ARIMA(log(x1000_passagerer) ~ 0 + pdq(1,1,0) + PDQ(1,1,0)),
    # Blandet ARMA + sæsonel MA(1)
    M3   = ARIMA(log(x1000_passagerer) ~ 0 + pdq(1,1,1) + PDQ(0,1,1)),
    # AR(2) + sæsonel MA(1)
    M4   = ARIMA(log(x1000_passagerer) ~ 0 + pdq(2,1,0) + PDQ(0,1,1))
  )

report(fit_arima)

# Hent faktisk antal estimerede parametre fra Auto-modellen
# Bruges som dof i Ljung-Box — korrekt justerer frihedsgrader for den valgte orden

dof_int <- fit_arima %>%
  filter(key == "International trafik i alt") %>%
  select(Auto) %>%
  coefficients() %>%
  nrow()

dof_osb <- fit_arima %>%
  filter(key == "Over Storebælt") %>%
  select(Auto) %>%
  coefficients() %>%
  nrow()


# Kan gøres meget mere avanceret -> der er ikke søgt særlig grundigt efter "bedste" model:

# Med corona
fit_arima_corona <- jernbane_corona_train %>%
  model(
    Auto = ARIMA(log(x1000_passagerer)),
    M1   = ARIMA(log(x1000_passagerer) ~ 0 + pdq(0,1,1) + PDQ(0,1,1)),
    M2   = ARIMA(log(x1000_passagerer) ~ 0 + pdq(1,1,0) + PDQ(1,1,0)),
    M3   = ARIMA(log(x1000_passagerer) ~ 0 + pdq(1,1,1) + PDQ(0,1,1)),
    M4   = ARIMA(log(x1000_passagerer) ~ 0 + pdq(2,1,0) + PDQ(0,1,1))
  )

report(fit_arima_corona)

# Hent faktisk antal estimerede parametre fra Auto-modellen
# Bruges som dof i Ljung-Box — korrekt justerer frihedsgrader for den valgte orden

dof_corona_int <- fit_arima_corona %>%
  filter(key == "International trafik i alt") %>%
  select(Auto) %>%
  coefficients() %>%
  nrow()

dof_corona_osb <- fit_arima_corona %>%
  filter(key == "Over Storebælt") %>%
  select(Auto) %>%
  coefficients() %>%
  nrow()

# Verificer hvilken model Auto valgte og hvilken dof der anvendes
cat("International inkl. corona: dof =", dof_int, "\n")
cat("Over Storebælt inkl. corona: dof =", dof_osb, "\n")
cat("International uden corona: dof =", dof_corona_int, "\n")
cat("Over Storebælt uden corona: dof =", dof_corona_osb, "\n")


#Arima Modelsammenligning med og uden corona

#Uden Corona
glance(fit_arima) %>%
  select(key, .model, sigma2, log_lik, AIC, AICc, BIC) %>%
  arrange(key, AICc) %>%
  kbl(caption = "Tabel: ARIMA-modelsammenligning inkl. corona (AICc)", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Med Corona
glance(fit_arima_corona) %>%
  select(key, .model, sigma2, log_lik, AIC, AICc, BIC) %>%
  arrange(key, AICc) %>%
  kbl(caption = "Tabel: ARIMA-modelsammenligning inkl. corona (AICc)", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Residualanalyse – inkl. corona (Auto-model, én serie ad gangen)
fit_arima %>%
  filter(key == "International trafik i alt") %>%
  select(Auto) %>%
  gg_tsresiduals(lag_max = 16) +
  labs(title = "Residualer – Auto ARIMA – International (inkl. corona)")

fit_arima %>%
  filter(key == "Over Storebælt") %>%
  select(Auto) %>%
  gg_tsresiduals(lag_max = 16) +
  labs(title = "Residualer – Auto ARIMA – Over Storebælt (inkl. corona)")

# Residualanalyse – uden corona
fit_arima_corona %>%
  filter(key == "International trafik i alt") %>%
  select(Auto) %>%
  gg_tsresiduals(lag_max = 16) +
  labs(title = "Residualer – Auto ARIMA – International (uden corona)")

fit_arima_corona %>%
  filter(key == "Over Storebælt") %>%
  select(Auto) %>%
  gg_tsresiduals(lag_max = 16) +
  labs(title = "Residualer – Auto ARIMA – Over Storebælt (uden corona)")

# Tester residualerne (ingen autokorrelation) H0: residualerne er ukorrelerede.
# Lag=8 = 2xM ved kvartalsdata. 
# Dof = antal estimerede arma parametre i auto mode

# Ljung-Box – inkl. corona
augment(fit_arima) %>%
  filter(.model == "Auto", key == "International trafik i alt") %>%
  features(.innov, ljung_box, lag = 8, dof = dof_int)

augment(fit_arima) %>%
  filter(.model == "Auto", key == "Over Storebælt") %>%
  features(.innov, ljung_box, lag = 8, dof = dof_osb)

# Ljung-Box – uden corona
augment(fit_arima_corona) %>%
  filter(.model == "Auto", key == "International trafik i alt") %>%
  features(.innov, ljung_box, lag = 8, dof = dof_corona_int)

augment(fit_arima_corona) %>%
  filter(.model == "Auto", key == "Over Storebælt") %>%
  features(.innov, ljung_box, lag = 8, dof = dof_corona_osb)

# Optimal ARIMA-søgning - bygger videre på fit_arima og fit_arima_corona
# stepwise = FALSE søger ALLE kombinationer (ikke kun stepwise-sti)
# approximation = FALSE: bruger eksakt likelihood (mere præcise AIC/BIC)
# order_constraint: begrænser søgerum så det ikke tager for lang tid.

# Optimal ARIMA-søgning
fit_arima_optimal <- jernbane_train %>%
  model(
    Auto = ARIMA(log(x1000_passagerer)),
    Optimal = ARIMA(
      log(x1000_passagerer),
      stepwise         = FALSE,
      approximation    = FALSE,
      order_constraint = p + q + P + Q <= 9 & (constant + d + D <= 2)
    )
  )

fit_arima_optimal_corona <- jernbane_corona_train %>%
  model(
    Auto = ARIMA(log(x1000_passagerer)),
    Optimal = ARIMA(
      log(x1000_passagerer),
      stepwise         = FALSE,
      approximation    = FALSE,
      order_constraint = p + q + P + Q <= 9 & (constant + d + D <= 2)
    )
  )

# AICc sammenligner modeller på fit, justeret for modelkompleksitet. 
# Lav AICc er bedre

# AICc-sammenligning: Auto vs. Optimal – inkl. corona
glance(fit_arima_optimal) %>%
  arrange(key, AICc) %>%
  kbl(caption = "Tabel: Optimal ARIMA vs. Auto – inkl. corona (AICc)", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# AICc-sammenligning: Auto vs. Optimal – uden corona
glance(fit_arima_optimal_corona) %>%
  arrange(key, AICc) %>%
  kbl(caption = "Tabel: Optimal ARIMA vs. Auto – uden corona (AICc)", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))



# Modelsammenligning med kilde label - Træning & Test
# Accuracy beregner fejlmål(metrikker, RMSE,MAE,MAPE,MASE) på både træning og test
# Træning fejl viser in-sample fit, Test-fejl viser out-of-sample prædiktionsevne.

resultat <- bind_rows(
  fit_arima        %>% accuracy() %>% mutate(data = "Inkl. corona", familie = "ARIMA"),
  fit_ets          %>% accuracy() %>% mutate(data = "Inkl. corona", familie = "ETS"),
  fit_arima_corona %>% accuracy() %>% mutate(data = "Uden corona",  familie = "ARIMA"),
  fit_ets_corona   %>% accuracy() %>% mutate(data = "Uden corona",  familie = "ETS"),
  fit_arima        %>% forecast(h = 4) %>% accuracy(jernbane_test)       %>% mutate(data = "Inkl. corona", familie = "ARIMA"),
  fit_ets          %>% forecast(h = 4) %>% accuracy(jernbane_test)       %>% mutate(data = "Inkl. corona", familie = "ETS"),
  fit_arima_corona %>% forecast(h = 4) %>% accuracy(jernbane_corona_test) %>% mutate(data = "Uden corona",  familie = "ARIMA"),
  fit_ets_corona   %>% forecast(h = 4) %>% accuracy(jernbane_corona_test) %>% mutate(data = "Uden corona",  familie = "ETS")
)

cols <- c("familie", ".model", "key", ".type", "RMSE", "MAE", "MAPE", "MASE")

# Tabellerne A-H viser fejlmål opdelt på modeltype, datasæt & split
# MASE < 1 betyder at modellen slår SNAIVE benchmark modellen
# Som er minimumskravet for en brugbar model

# Tabel A: ARIMA inkl. corona – træning
resultat %>%
  filter(data == "Inkl. corona", familie == "ARIMA", .type == "Training") %>%
  select(all_of(cols)) %>%
  arrange(key, RMSE) %>%
  kbl(caption = "Tabel A: ARIMA inkl. corona – træning", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel B: ARIMA inkl. corona – test
resultat %>%
  filter(data == "Inkl. corona", familie == "ARIMA", .type == "Test") %>%
  select(all_of(cols)) %>%
  arrange(key, RMSE) %>%
  kbl(caption = "Tabel B: ARIMA inkl. corona – test", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel C: ETS inkl. corona – træning
resultat %>%
  filter(data == "Inkl. corona", familie == "ETS", .type == "Training") %>%
  select(all_of(cols)) %>%
  arrange(key, RMSE) %>%
  kbl(caption = "Tabel C: ETS inkl. corona – træning", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel D: ETS inkl. corona – test
resultat %>%
  filter(data == "Inkl. corona", familie == "ETS", .type == "Test") %>%
  select(all_of(cols)) %>%
  arrange(key, RMSE) %>%
  kbl(caption = "Tabel D: ETS inkl. corona – test", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel E: ARIMA uden corona – træning
resultat %>%
  filter(data == "Uden corona", familie == "ARIMA", .type == "Training") %>%
  select(all_of(cols)) %>%
  arrange(key, RMSE) %>%
  kbl(caption = "Tabel E: ARIMA uden corona – træning", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel F: ARIMA uden corona – test
resultat %>%
  filter(data == "Uden corona", familie == "ARIMA", .type == "Test") %>%
  select(all_of(cols)) %>%
  arrange(key, RMSE) %>%
  kbl(caption = "Tabel F: ARIMA uden corona – test", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel G: ETS uden corona – træning
resultat %>%
  filter(data == "Uden corona", familie == "ETS", .type == "Training") %>%
  select(all_of(cols)) %>%
  arrange(key, RMSE) %>%
  kbl(caption = "Tabel G: ETS uden corona – træning", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel H: ETS uden corona – test
resultat %>%
  filter(data == "Uden corona", familie == "ETS", .type == "Test") %>%
  select(all_of(cols)) %>%
  arrange(key, RMSE) %>%
  kbl(caption = "Tabel H: ETS uden corona – test", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# opsummering af bedste modeller:
# Slice_Min/max finder den model med lavest og højest RMSE pr. gruppe.
# Giver et hurtigt overblik over hvilke modeller der konsekvent præsterer bedst

opsummering <- resultat %>%
  group_by(data, familie, .type, key) %>%
  summarise(
    Bedst_model    = .model[which.min(RMSE)],
    Bedst_RMSE     = min(RMSE),
    Bedst_MAPE     = MAPE[which.min(RMSE)],
    Dårligst_model = .model[which.max(RMSE)],
    Dårligst_RMSE  = max(RMSE),
    Dårligst_MAPE  = MAPE[which.max(RMSE)],
    .groups = "drop"
  ) %>%
  arrange(data, familie, .type, key)

opsummering %>%
  kbl(caption = "Opsummering: Bedste og dårligste model per gruppe", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# TIME SERIES CROSS VALIDATION
# TSCV Udvider træningsvinduer skridt for skridt, og forecaster H=4 kvartaler frem
# Giver et mere robust estimat af forecast fejl end et enkelt train/test split

# Time series cross validation med corona
resultat_tscv <- jernbanestretch %>%
  model(
    ARIMA  = ARIMA(log(x1000_passagerer)),
    ETS    = ETS(log(x1000_passagerer)),
    SNAIVE = SNAIVE(log(x1000_passagerer))
  ) %>%
  forecast(h = 4) %>%
  accuracy(jernbanedata) %>%
  mutate(data = "Inkl. corona")

# Time series cross validation uden corona
resultat_tscv_corona <- jernbanestretch_corona %>%
  model(
    ARIMA  = ARIMA(log(x1000_passagerer)),
    ETS    = ETS(log(x1000_passagerer)),
    SNAIVE = SNAIVE(log(x1000_passagerer))
  ) %>%
  forecast(h = 4) %>%
  accuracy(jernbanedata_corona) %>%
  mutate(data = "Uden corona")

# Samlet TSCV-tabel - SNAIVE bruges som benchmark
# en god model bør have lavere RMSE end SNAIVE

bind_rows(resultat_tscv, resultat_tscv_corona) %>%
  select(data, .model, key, RMSE, MAE, MAPE, MASE) %>%
  arrange(data, key, RMSE) %>%
  kbl(caption = "Tabel: TSCV-modelsammenligning inkl. og uden corona", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# RMSE pr forecast horisont 
# Viser om forecast fejlen stiger markant med horisonten
# SNAIVE inkluderes som reference for at vurdere om modellerne tilføjer værdi over tid.

# Med corona
actuals <- jernbanedata %>%
  as_tibble() %>%
  select(kvartal, key, actual = x1000_passagerer)

# Uden corona
actuals_corona <- jernbanedata_corona %>%
  as_tibble() %>%
  select(kvartal, key, actual = x1000_passagerer)


# Figur: RMSE per horisont med corona
jernbanestretch %>%
  model(
    ARIMA  = ARIMA(log(x1000_passagerer)),
    ETS    = ETS(log(x1000_passagerer)),
    SNAIVE = SNAIVE(log(x1000_passagerer))
  ) %>%
  forecast(h = 4) %>%
  as_tibble() %>%
  group_by(.id, key, .model) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  left_join(actuals, by = c("kvartal", "key")) %>%
  group_by(.model, key, h) %>%
  summarise(RMSE = sqrt(mean((.mean - actual)^2, na.rm = TRUE)),
            .groups = "drop") %>%
  ggplot(aes(x = h, y = RMSE, colour = .model)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ key, scales = "free_y") +
  scale_x_continuous(breaks = 1:4, labels = c("Q+1", "Q+2", "Q+3", "Q+4")) +
  labs(title = "Figur 23: RMSE per forecast-horisont (h=1–4)",
       x = "Horisont", y = "RMSE", colour = "Model") +
  theme_minimal()

# Figur: RMSE per horisont uden corona
jernbanestretch_corona %>%
  model(
    ARIMA  = ARIMA(log(x1000_passagerer)),
    ETS    = ETS(log(x1000_passagerer)),
    SNAIVE = SNAIVE(log(x1000_passagerer))
  ) %>%
  forecast(h = 4) %>%
  as_tibble() %>%
  group_by(.id, key, .model) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  left_join(actuals_corona, by = c("kvartal", "key")) %>%
  group_by(.model, key, h) %>%
  summarise(RMSE = sqrt(mean((.mean - actual)^2, na.rm = TRUE)), .groups = "drop") %>%
  ggplot(aes(x = h, y = RMSE, colour = .model)) +
  geom_line() + geom_point() +
  facet_wrap(~ key, scales = "free_y") +
  scale_x_continuous(breaks = 1:4, labels = c("Q+1", "Q+2", "Q+3", "Q+4")) +
  labs(title = "Figur 23b: RMSE per forecast-horisont – uden corona",
       x = "Horisont", y = "RMSE", colour = "Model") +
  theme_minimal()


# Forecasting / Prædiktion --------------------------------------------------
# Hilo() beregner prædiktionsintervaller på 80% og 95% 
# bredere interval betyder større usikkerhed.
# unpack_hilo() adskiller intervallerne i 
# separate lower/upper kolonner for tabelvisning.

# prædiktionsintervaller for jernbanedata med corona
jernbanedata %>%
  model(
    ARIMA = ARIMA(log(x1000_passagerer)),
    ETS   = ETS(log(x1000_passagerer))
  ) %>%
  forecast(h = 4) %>%
  hilo(level = c(80, 95)) %>%
  unpack_hilo(c("80%", "95%")) %>%
  select(key, .model, kvartal, .mean,
         `80%_lower`, `80%_upper`,
         `95%_lower`, `95%_upper`) %>%
  kbl(caption = "Tabel 12a: Forecast og prædiktionsintervaller inkl. corona (80% og 95%)",
      digits = 1) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# prædiktionsintervaller for jernbanedata uden corona

jernbanedata_corona %>%
  model(
    ARIMA = ARIMA(log(x1000_passagerer)),
    ETS   = ETS(log(x1000_passagerer))
  ) %>%
  forecast(h = 4) %>%
  hilo(level = c(80, 95)) %>%
  unpack_hilo(c("80%", "95%")) %>%
  select(key, .model, kvartal, .mean,
         `80%_lower`, `80%_upper`,
         `95%_lower`, `95%_upper`) %>%
  kbl(caption = "Tabel 12b: Forecast og prædiktionsintervaller uden corona (80% og 95%)",
      digits = 1) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# filter_index afskærer historikken så prædiktionsintervallerne 
# er visuelt tydelige.
# Begge modeller vises i samme plot for direkte sammenligning 
# af forecast og usikkerhed.

jernbanedata %>%
  model(
    ARIMA = ARIMA(log(x1000_passagerer)),
    ETS   = ETS(log(x1000_passagerer))
  ) |>
  forecast(h = 4) %>%
  autoplot(jernbanedata %>% filter_index("2020 Q1" ~ .),
           level = c(80, 95)) +
  facet_wrap(~ key, scales = "free_y") +
  labs(title = "Forecast med prædiktionsintervaller (80% og 95%)",
       y = "1.000 passagerer", x = NULL) +
  theme_minimal()

# Sammenfatning og konklusion

# Kombinerer STL-styrker, COVID-behandling og bedste model i ét overblik per serie.
# Gør det nemt at sammenligne på tværs af de to datasæt og to tidsserier.

stl_features <- bind_rows(
  jernbanedata        %>% features(x1000_passagerer, feat_stl) %>% mutate(data = "Inkl. corona"),
  jernbanedata_corona %>% features(x1000_passagerer, feat_stl) %>% mutate(data = "Uden corona")
) %>%
  select(key, data, trend_strength, seasonal_strength_year)

# bedste model og MASE fra test resultater:
# slice_min finder den model med lavest test-RMSE per serie og datasæt

bedste <- resultat %>%
  filter(.type == "Test") %>%
  group_by(data, key) %>%
  slice_min(RMSE, n = 1) %>%
  select(key, data, Bedste_model = .model, MASE, RMSE)

# sammensæt

sammenfatning <- stl_features %>%
  left_join(bedste, by = c("key", "data")) %>%
  mutate(
    COVID_impact = case_when(
      data == "Inkl. corona" ~ "Ja – ubehandlet",
      data == "Uden corona"  ~ "Interpoleret"
    )
  ) %>%
  select(
    Serie        = key,
    Datasæt      = data,
    Trend        = trend_strength,
    Sæson        = seasonal_strength_year,
    COVID        = COVID_impact,
    Bedste_model,
    RMSE
  ) %>%
  arrange(Serie, Datasæt)

# Endeligt overblik over bedste og dårligste model pr. gruppe

sammenfatning %>%
  kbl(caption = "Sammenfatningsoversigt: Nøgletal per serie og datasæt",
      digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))



