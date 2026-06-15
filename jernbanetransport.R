pacman::p_load(tidyverse, gghighlight, hrbrthemes, tsibble, lubridate, 
               ggplot2, feasts, fable, tsibbledata, forecast, fpp3,
               fabletools, knitr, ggpubr, gridExtra, GGally, slider,
               forecast, car, shiny, glue, broom, kableExtra, gt, seasonal, 
               latex2exp, ggfortify, janitor, readxl)



Flyvninger <- read_excel("data/Flyvninger.xlsx", 
                         sheet = "Ark1", 
                         col_types = c("date", "numeric")
)

flyvninger <- Flyvninger %>%
  clean_names() %>%
<<<<<<< Updated upstream
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date) %>%# Husk index, key og flere tidsserier
  filter_index(. ~ "2019 dec")
=======
  filter(key %in% c("International trafik i alt", "Over Storebælt")) %>%
  mutate(kvartal = str_replace(kvartal, "K", "Q")) %>%
  mutate(kvartal = yearquarter(kvartal)) %>%
  as_tsibble(index = kvartal, key = key) # Husk index, key og flere tidsserier
  #filter_index(. ~ "2006Q1")
>>>>>>> Stashed changes

# Husk at tilføje labels, titler og figurnummer

jernbanedata %>%
  autoplot(x1000_passagerer)
# Trend
# Sæson
# Hetroeksadicitet(denne del kan vi godt lide er væk) - box cox eller logaritme
jernbanedata %>%
  autoplot(log(x1000_passagerer))

# Kapitel 2
jernbanedata %>% 
  gg_season()

jernbanedata %>% 
  gg_subseries()

# Hvis det ikke bidrager med noget nyt - lad vær

# Her fjerner vi corona perioden
jernbanedata_corona <- jernbanedata %>%
  filter_index(. ~ "2019 Q4", "2022 Q2" ~ .)

jernbanedata_corona <- jernbanedata_corona %>%
  tsibble::fill_gaps() %>%
  group_by_key() %>%
  mutate(x1000_passagerer = forecast::na.interp(x1000_passagerer)) %>%
  ungroup()

jernbanedata_corona %>% 
  gg_season()

jernbanedata_corona %>% 
  gg_subseries()


jernbanedata_corona %>%
  autoplot(x1000_passagerer)

# ACF med Corona
# Forklar hvorfor vi vælger 12
jernbanedata_corona %>%
  ACF(lag_max = 12) %>%
  autoplot()

# ACF uden corona
# Vær opmærksom på fill_gaps() - pro og cons
jernbanedata_corona %>%
  ACF(lag_max = 12) %>%
  autoplot()
# Vil gå år før vi er under konfidenspunkt

# PACF & ACF
# Hvad betyder de forskellige informationer på visualiseringerne
# International trafik i alt
jernbanedata_corona %>%
  filter(key == "International trafik i alt") %>%
  mutate(x1000_passagerer = log(x1000_passagerer)) %>%
  gg_tsdisplay(x1000_passagerer, plot_type = "partial", lag_max = 12)

# Over Storebælt
jernbanedata_corona %>%
  filter(key == "Over Storebælt") %>%
  mutate(x1000_passagerer = log(x1000_passagerer)) %>%
  gg_tsdisplay(x1000_passagerer, plot_type = "partial", lag_max = 12)


# Deskriptive statistikker
# Deskriptive statistikker per serie

# Med Corona
jernbanedata %>%
  as_tibble() %>%
  group_by(key) %>%
  summarise(
    mean   = mean(x1000_passagerer, na.rm = TRUE),
    median = median(x1000_passagerer, na.rm = TRUE),
    sd     = sd(x1000_passagerer, na.rm = TRUE),
    min    = min(x1000_passagerer, na.rm = TRUE),
    max    = max(x1000_passagerer, na.rm = TRUE)
)

jernbanedata %>%
  features(x1000_passagerer, feat_stl)

# Uden Corona
jernbanedata_corona %>%
  as_tibble() %>%
  group_by(key) %>%
  summarise(
    mean   = mean(x1000_passagerer, na.rm = TRUE),
    median = median(x1000_passagerer, na.rm = TRUE),
    sd     = sd(x1000_passagerer, na.rm = TRUE),
    min    = min(x1000_passagerer, na.rm = TRUE),
    max    = max(x1000_passagerer, na.rm = TRUE)
  )

jernbanedata_corona %>%
  features(x1000_passagerer, feat_stl)

#STL med corona
model_jernbane <- jernbanedata %>%
  model(
    STL(log(x1000_passagerer), robust = TRUE) # Log eller cox box skal altid være her!
  ) %>%
  components()

# Ændre navne på plot
model_jernbane %>% 
  autoplot()

# STL med corona
model_jernbane_corona <- jernbanedata_corona %>%
  tsibble::fill_gaps() %>%
  group_by_key() %>%
  mutate(x1000_passagerer = forecast::na.interp(x1000_passagerer)) %>%
  ungroup() %>%
  model(
    STL(log(x1000_passagerer) ~ trend(window = 7) + season(window = "periodic"), 
        robust = TRUE)
  ) %>%
  #augment() %>%
  components()

# Ændre navne på plot
model_jernbane_corona %>% 
  autoplot()

# Dette kan man gå ind og kommentere - tjek hurtig på google og kommenter med
# 2 linjer (ovenover)

#Augment med corona
model_jernbane_aug <- jernbanedata %>%
  model(
    STL(log(x1000_passagerer), robust = TRUE) # Log eller cox box skal altid være her!
  ) %>%
  augment()

# Ændre navne på plot
model_jernbane %>% 
  autoplot()

# Augment uden
model_jernbane_corona_aug <- jernbanedata_corona %>%
  tsibble::fill_gaps() %>%
  group_by_key() %>%
  mutate(x1000_passagerer = forecast::na.interp(x1000_passagerer)) %>%
  ungroup() %>%
  model(
    STL(log(x1000_passagerer) ~ trend(window = 7) + season(window = "periodic"), 
        robust = TRUE)
  ) %>%
  augment()
# Dette kan man gå ind og kommentere - tjek hurtig på google og kommenter med
# 2 linjer

# EDA opsummering / del konklussion


# Transformation

model_jernbane %>% 
  as_tsibble() %>% 
  autoplot(`log(x1000_passagerer)`, color = 'gray') +
  geom_line(aes(y = season_adjust), colour = '#0072B2')

model_jernbane_corona %>% 
  as_tsibble() %>% 
  autoplot(`log(x1000_passagerer)`, color = 'gray') +
  geom_line(aes(y = season_adjust), colour = '#0072B2')

Feauteres

jernbanedata %>%
  features(x1000_passagerer, feat_stl)
# Denne kan man bruge til fortæller om trend og sæson oh hvis den x er hø
# kan man anvende til at sige at den er god at forecaste
# Hvad betyder de forskellige værdier??

# Andre feauteres???

# Tjek evt .innov med graf(er)

passagererstretch <- jernbanedata %>%
  stretch_tsibble(.init = 24, .step = 1)
# Nu er det muligt at beregtne RMSE - Vær forsigtig at kører det hvis det
# tager lang tid

# Boxcox & Guerrero

# Guerrero optimal lambda per. serie
jernbanedata %>%
  features(x1000_passagerer, features = guerrero) %>%
  rename(Lambda_Guerrero = lambda_guerrero) %>%
  kbl(caption = "Tabel 1: Guerrero-optimeret Box-Cox lambda",
      digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

jernbanedata_corona %>%
  features(x1000_passagerer, features = guerrero) %>%
  rename(Lambda_Guerrero = lambda_guerrero) %>%
  kbl(caption = "Tabel 1: Guerrero-optimeret Box-Cox lambda",
      digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Rå vs. log-transformeret (begge serier i facet)
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
  labs(title = "Figur 1: Rå vs. log-transformeret passagertal",
       y = NULL,
       x = NULL)

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
  labs(title = "Figur 1: Rå vs. log-transformeret passagertal",
       y = NULL,
       x = NULL)

#Box-COX
jernbanedata %>%
  group_by(key) %>%
  mutate(
    lambda = (jernbanedata %>%
                filter(key == first(key)) %>%
                features(x1000_passagerer, guerrero))$lambda_guerrero,
    x1000_passagerer_bc = box_cox(x1000_passagerer, lambda)
  ) |>
  autoplot(x1000_passagerer_bc) +
  labs(title = "Figur 2: Box-Cox transformeret passagertal (Guerrero lambda)",
       y = "box_cox(passagerer, lambda)",
       x = NULL)

jernbanedata_corona %>%
  group_by(key) %>%
  mutate(
    lambda = (jernbanedata_corona %>%
                filter(key == first(key)) %>%
                features(x1000_passagerer, guerrero))$lambda_guerrero,
    x1000_passagerer_bc = box_cox(x1000_passagerer, lambda)
  ) |>
  autoplot(x1000_passagerer_bc) +
  labs(title = "Figur 2: Box-Cox transformeret passagertal (Guerrero lambda)",
       y = "box_cox(passagerer, lambda)",
       x = NULL)

# Opsummering / Delkonklussion
# Guerrero estimerer lambda ved at minimere variationskoefficenten på tværs af sæsoner
# et lambda tæt på 0 bekræfter at log er det rettevalg mens lambda tæt på 1
# Betyder ingen transformation er nødvendig.


# Stationaritet og differentiering

# Transformation er kun nødvendigt for datasættet der indeholder Corona perioden
# da guerrero's estimeret lamda er tæt på / over 1.

# Logtransformeret

# Log-transformeret serie - er den stationær? 
jernbanedata |>
  mutate(log_passagerer = log(x1000_passagerer)) |>
  autoplot(log_passagerer) +
  labs(title = "Log-transformerede jernbanepassagerer",
       subtitle = "Visuel inspektion for trend og ikke-stationaritet",
       y = "log(1.000 passagerer)",
       x = NULL)

# Differentieret log-serie (d=1) - fjerner lineær trend
jernbanedata |>
  mutate(diff_log = difference(log(x1000_passagerer))) |>
  autoplot(diff_log) +
  labs(title = "Figur 2: Første differentiering af log(passagerer)",
       subtitle = "difference(log(x), lag = 1) - fjerner trend",
       y = "Δlog(1.000 passagerer)",
       x = NULL)

# Sæsondifferentieret log-serie (D=1, lag=4 for kvartalsdata)
# difference(..., lag = 4) svarer til D=1 i ARIMA-notation
jernbanedata |>
  mutate(sdiff_log = difference(log(x1000_passagerer), lag = 4)) |>
  autoplot(sdiff_log) +
  labs(title = "Sæsondifferentiering af log(passagerer)",
       subtitle = "difference(log(x), lag = 4) - fjerner sæsonmønster (D=1)",
       y = "Δ₄log(1.000 passagerer)",
       x = NULL)

# Dobbelt differentiering (sæson + trend) - D=1, d=1
jernbanedata |>
  mutate(ddiff_log = difference(difference(log(x1000_passagerer), lag = 4), 1)) |>
  autoplot(ddiff_log) +
  labs(title = "Figur 4: Dobbelt differentiering af log(passagerer)",
       subtitle = "Sæson (lag=4) + trend (lag=1) - D=1, d=1",
       y = "ΔΔ₄log(1.000 passagerer)",
       x = NULL)

#Alle fire trin samlet i ét facet-plot (inspireret af fpp3 kap. 9)
jernbanedata |>
  filter(key == "International trafik i alt") |>
  as_tibble() |>
  transmute(
    kvartal,
    `log(passagerer)`                  = log(x1000_passagerer),
    `Δlog (d=1)`                        = difference(log(x1000_passagerer), 1),
    `Δ₄log (D=1, lag=4)`               = difference(log(x1000_passagerer), 4),
    `ΔΔ₄log (d=1, D=1)`                = difference(difference(log(x1000_passagerer), 4), 1)
  ) |>
  pivot_longer(-kvartal, names_to = "Type", values_to = "Værdi") |>
  mutate(Type = factor(Type, levels = c(
    "log(passagerer)", "Δlog (d=1)", "Δ₄log (D=1, lag=4)", "ΔΔ₄log (d=1, D=1)"
  ))) |>
  ggplot(aes(x = kvartal, y = Værdi)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Figur 5: Differentieringstrin - International trafik i alt",
       y = NULL, x = NULL)

# KPSS - test på log data
# H0: Serien er stationær
# p < 0.05 → afvis H0 → serien er IKKE stationær → differentiering nødvendig
# p > 0.05 → behold H0 → serien er stationær → ingen differentiering nødvendig

# KPSS-test på log(passagerer)
jernbanedata |>
  features(log(x1000_passagerer), unitroot_kpss) |>
  kbl(caption = "Tabel 4: KPSS-test på log(x1000_passagerer) — H0: stationær",
      digits = 4) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

# KPSS på første differentiering - er d=1 nok?
jernbanedata |>
  mutate(diff_log = difference(log(x1000_passagerer))) |>
  features(diff_log, unitroot_kpss) |>
  kbl(caption = "Tabel 4b: KPSS-test på Δlog(passagerer) — er d=1 tilstrækkeligt?",
      digits = 4) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tabel 5: Antal sæsondifferentieringer D
jernbanedata |>
  features(log(x1000_passagerer), unitroot_nsdiffs) |>
  kbl(caption = "Tabel 5: Anbefalet antal sæsondifferentieringer (D) — kvartalsdata: lag=4",
      digits = 0) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

# NSDIFF
# Tabel 5: Antal sæsondifferentieringer D
jernbanedata |>
  features(log(x1000_passagerer), unitroot_nsdiffs) |>
  kbl(caption = "Tabel 5: Anbefalet antal sæsondifferentieringer (D) — kvartalsdata: lag=4",
      digits = 0) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

# ndiffs efter sæsondifferentiering - hvad er d, givet D=1?
jernbanedata |>
  mutate(sdiff_log = difference(log(x1000_passagerer), lag = 4)) |>
  features(sdiff_log, unitroot_ndiffs) |>
  kbl(caption = "Tabel 5b: Anbefalet d efter sæsondifferentiering (D=1)",
      digits = 0) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

#ndiffs
# unitroot_ndiffs: anbefalet antal ordinære differentieringer (d)
# Baseret på KPSS-test sekventielt
jernbanedata |>
  features(log(x1000_passagerer), unitroot_ndiffs) |>
  kbl(caption = "Tabel 4c: Anbefalet antal ordinære differentieringer (d)",
      digits = 0) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

# ACF på log-data (ikke-differentieret) vs. differentieret
# Inspireret af google_2015-eksemplet i fpp3 kapitel 9

p1 <- jernbanedata |>
  ACF(log(x1000_passagerer), lag_max = 12) |>
  autoplot() +
  labs(title = "ACF: log(passagerer)",
       subtitle = "Ikke-differentieret")

p2 <- jernbanedata |>
  mutate(sdiff_log = difference(log(x1000_passagerer), lag = 4)) |>
  ACF(sdiff_log, lag_max = 12) |>
  autoplot() +
  labs(title = "ACF: Δ₄log(passagerer)",
       subtitle = "Sæsondifferentieret (D=1, lag=4)")

grid.arrange(p1, p2, ncol = 2)


jernbanedata %>%
  filter(key == "International trafik i alt") %>%
  gg_tsdisplay(difference(log(x1000_passagerer), lag = 4),
               plot_type = "partial", lag_max = 24) +
  labs(title = "Figur 14: ACF/PACF – sæsondiff (D=1, lag=4) – International trafik i alt",
       subtitle = "Toppe ved lag 4, 8, 12 indikerer tilbageværende sæsonstruktur")

jernbanedata %>%
  filter(key == "Over Storebælt") %>%
  gg_tsdisplay(difference(log(x1000_passagerer), lag = 4),
               plot_type = "partial", lag_max = 24) +
  labs(title = "Figur 14: ACF/PACF – sæsondiff (D=1, lag=4) – International trafik i alt",
       subtitle = "Toppe ved lag 4, 8, 12 indikerer tilbageværende sæsonstruktur")

# Dobbelt differentiering
# Dobbelt differentiering bør kun vælges hvis unitroot_ndiff returnerer 1 efter 
# Sæsondifferentiering - Ellers risiko for overdifferentiering, som forværrer modellen
# Der retuneres 0 og derfor laves der ikke dobbelt differentiering.
Nedestående skal slettes!

jernbanedata |>
  filter(key == "International trafik i alt") %>%
  gg_tsdisplay(difference(log(x1000_passagerer), lag = 4) |> difference(),
               plot_type = "partial", lag_max = 24) +
  labs(title = "Figur 16: ACF/PACF – dobbelt diff (D=1, d=1) – International trafik i alt",
       subtitle = "Sæson (lag=4) + trend (lag=1) – kun hvis ndiffs=1 efter sæsondiff")

jernbanedata |>
  filter(key == "Over Storebælt") %>%
  gg_tsdisplay(difference(log(x1000_passagerer), lag = 4) |> difference(),
               plot_type = "partial", lag_max = 24) +
  labs(title = "Figur 16: ACF/PACF – dobbelt diff (D=1, d=1) – International trafik i alt",
       subtitle = "Sæson (lag=4) + trend (lag=1) – kun hvis ndiffs=1 efter sæsondiff")

# Træningsobjekter
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

# Benchmark model
jernbanestretch <- jernbanedata %>%
  stretch_tsibble(.init = 24, .step = 1)
# Nu er det muligt at beregtne RMSE - Vær forsigtig at kører det hvis det
# tager lang tid

# Benchmark model
jernbanestretch %>%
  model(SNAIVE(log(x1000_passagerer))) %>%
  forecast(h = 4) %>%
  accuracy(jernbanedata)

jernbanestretch_corona <- jernbanedata_corona %>%
  stretch_tsibble(.init = 24, .step = 1)
# Nu er det muligt at beregtne RMSE - Vær forsigtig at kører det hvis det
# tager lang tid

# Benchmark model
jernbanestretch_corona %>%
  model(SNAIVE(log(x1000_passagerer))) %>%
  forecast(h = 4) %>%
  accuracy(jernbanedata_corona)

# ETS 
fit_ets <- jernbane_train %>%
  model(ETS(log(x1000_passagerer)))

report(fit_ets)

fit_ets_corona <- jernbane_corona_train %>%
  model(ETS(log(x1000_passagerer)))

report(fit_ets_corona)
# Her må vi gerne kommenter kort på nogle af tingene - Hvis det mer forklaring
# gerne alpha, beta, gamma - evt. phi

fit_ets %>%
  gg_tsresiduals(lag_max = 36)

fit_ets_corona %>%
  gg_tsresiduals(lag_max = 36)

augment(fit_ets) %>%
  features(.innov, ljung_box, lag = 24, dof = 5) # Altid .innov for at tjekke

augment(fit_ets_corona) %>%
  features(.innov, ljung_box, lag = 24, dof = 5) # Altid .innov for at tjekke
# forslag til hvordan vi kan forbedre forcast ift - Corona (Hvad kan vi gøre
# anderledes)

fit_arima <- jernbane_train %>%
  model(ARIMA(log(x1000_passagerer)))

report(fit_arima)

# Kan gøres meget mere avanceret -> der er ikke søgt særlig grundigt efter "bedste" model:

fit_arima_corona <- jernbane_corona_train %>%
  model(ARIMA(log(x1000_passagerer)))

report(fit_arima_corona)

# Eksempel på hvad man kan lave
# Stepwise = FALSE
# Approximation = False
# Order_constraint = p + q + P + Q <= 9 & (constant + d + D <= 2)


# Vigtigt at der sikres for autokorrelation inden
# Altid kommenter at der er et problem og hvad det kan skyldes

# Residualanalyse

fit_arima %>%
  gg_tsresiduals(lag_max = 36)

augment(fit_arima) %>%
  features(.innov, ljung_box, lag = 24, dof = 5) # Altid .innov for at tjekke

# Altid kommenter at der er et problem og hvad det kan skyldes
fit_arima_corona %>%
  gg_tsresiduals(lag_max = 36)

augment(fit_arima_corona) %>%
  features(.innov, ljung_box, lag = 24, dof = 5) # Altid .innov for at tjekke

resultat <- bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima_corona %>% accuracy(),
  fit_ets_corona %>% accuracy(),
  fit_arima %>% forecast(h = 4) %>% accuracy(jernbane_test),
  fit_ets %>% forecast(h = 4) %>% accuracy(jernbane_test),
  fit_arima_corona %>% forecast(h = 4) %>% accuracy(jernbane_corona_test),
  fit_ets_corona %>% forecast(h = 4) %>% accuracy(jernbane_corona_test)
)
View(resultat)
# Time series cross validation
 jernbanedata %>%
  model(ARIMA(log(x1000_passagerer))) %>%
  forecast(h = "1 years") %>%
  autoplot(jernbanedata)
# Husk at der også er en manuel vej til at ginde en "optimal" (S)ARIMA - model
# Det er vigtigt at have nogle velvalgte kandidatmodeller, som vi selv laver
# Husk i den forbindelse at bruge uniroot test:

 jernbanedata_corona %>%
   model(ARIMA(log(x1000_passagerer))) %>%
   forecast(h = "1 years") %>%
   autoplot(jernbanedata_corona)
 
#Hvad betyder det her egentlig 
 jernbanedata %>%
  features(log(x1000_passagerer), unitroot_nsdiffs)

jernbanedata_corona %>%
  features(log(x1000_passagerer), unitroot_nsdiffs)

jernbanedata %>%
  features(difference(log(x1000_passagerer), 12), unitroot_nsdiffs)

jernbanedata_corona %>%
  features(difference(log(x1000_passagerer), 12), unitroot_nsdiffs)

# Brug derefter ACF og PACH til at finde kandidatmodeller. - Grov skitse
# Indenfor ikke sæson kan vi ikke anvende følgende
# Dette kan findes i kapitel 9
# Tjek om nedenstående er rigtigt
jernbane_train %>%
  gg_tsdisplay(log(x1000_passagerer), plot_type = "partial")

# TIPS
# Vær meget konsis
# Kapitel 5 eller 7 ift. - hvad man kan gøre med perioder, som corona
# evt. vælge at lave dummy variabler for følgende periode - hvis til den
# mundtlige eksamen

