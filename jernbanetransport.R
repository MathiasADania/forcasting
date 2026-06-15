pacman::p_load(tidyverse, gghighlight, hrbrthemes, tsibble, lubridate, 
               ggplot2, feasts, fable, tsibbledata, forecast, fpp3,
               fabletools, knitr, ggpubr, gridExtra, GGally, slider,
               forecast, car, shiny, glue, broom, kableExtra, gt, seasonal, 
               latex2exp, ggfortify, janitor, readxl)



jernbanedata <- read_excel("data/jernbanetransport af passagerer.xlsx", 
                         sheet = "BANE25"
)

# Upload virker
jernbanedata <- jernbanedata %>%
  clean_names() %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date) %>% # Husk index, key og flere tidsserier
  filter_index(. ~ "2006K1")

# Husk at tilføje labels, titler og figurnummer

flyvninger %>%
  autoplot(antal_flyvninger_i_1000)
# Trend
# Sæson
# Hetroeksadicitet(denne del kan vi godt lide er væk) - box cox eller logaritme
flyvninger %>%
  autoplot(log(antal_flyvninger_i_1000))

# Kapitel 2
flyvninger %>% 
  gg_season()

flyvninger %>% 
  gg_subseries()
# Hvis det ikke bidrager med noget nyt - lad vær

flyvninger %>%
  ACF(lag_max = 48) %>%
  autoplot
# Sæson = 
# Vil gå år før vi er under konfidenspunkt

model_flyv <- flyvninger %>%
  model(
    STL(log(antal_flyvninger_i_1000), robust = TRUE) # Log eller cox box skal altid være her!
  ) %>%
components()
# Dette kan man gå ind og kommentere - tjek hurtig på google og kommenter med
# 2 linjer

model_flyv <- flyvninger %>%
  model(
    STL(log(antal_flyvninger_i_1000), robust = TRUE) # Log eller cox box skal altid være her!
  ) %>%
  augment()
# Dette kan man gå ind og kommentere - tjek hurtig på google og kommenter med
# 2 linjer


model_flyv %>% 
  as_tsibble() %>% 
  autoplot(`log(antal_flyvninger_i_1000)`, color = 'gray') +
  geom_line(aes(y = season_adjust), colour = '#0072B2')

# Overvej gerrero/boxcox

flyvninger %>%
  features(antal_flyvninger_i_1000, feat_stl)
# Denne kan man bruge til fortæller om trend og sæson oh hvis den x er hø
# kan man anvende til at sige at den er god at forecaste
# Hvad betyder de forskellige værdier??

# Andre feauteres???

# Tjek evt .innov med graf(er)

flyvningerstretch <- flyvninger %>%
  stretch_tsibble(.init = 24, .step = 1)
# Nu er det muligt at beregtne RMSE - Vær forsigtig at kører det hvis det
# tager lang tid

# Benchmark model
flyvningerstretch %>%
  model(RW(log(antal_flyvninger_i_1000 ~ drift( )))) %>%
  forecast(h = 12) %>%
  accuracy(flyvninger)

# Eller - Når vi vælger model er det vigtigt at vi argumenterer for begrundelsen
flyvningerstretch %>%
  model(SNAIVE(log(antal_flyvninger_i_1000))) %>%
  forecast(h = 12) %>%
  accuracy(flyvninger)
  
# Opdel på træning (og test) eller brug stretch funktionen (rolling origion)
flyvninger_train <- flyvninger %>%
  filter_index(. ~ "2017 dec")

# forslag til hvordan vi kan forbedre forcast ift - Corona

fit_arima <- flyvninger_train %>%
  model(ARIMA(log(antal_flyvninger_i_1000)))
# Kan gøres meget mere avanceret -> der er ikke søgt særlig grundigt efter "bedste" model:

# Eksempel på hvad man kan lave
# Stepwise = FALSE
# Approximation = False
# Order_constraint = p + q + P + Q <= 9 & (constant + d + D <= 2)

report(fit_arima)

# Vigtigt at der sikres for autokorrelation inden
# Altid kommenter at der er et problem og hvad det kan skyldes
fit_arima %>%
  gg_tsresiduals(lag_max = 36)

augment(fit_arima) %>%
  features(.innov, ljung_box, lag = 24, dof = 5) # Altid .innov for at tjekke

fit_ets <- flyvninger_train %>%
  model(ETS(log(antal_flyvninger_i_1000)))

report(fit_ets)
# Her må vi gerne kommenter kort på nogle af tingene - Hvis det mer forklaring
# gerne alpha, beta, gamma - evt. phi

fit_ets %>%
  gg_tsresiduals(lag_max = 36)

augment(fit_ets) %>%
  features(.innov, ljung_box, lag = 24, dof = 5) # Altid .innov for at tjekke

bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = 24) %>% accuracy(flyvninger),
  fit_ets %>% forecast(h = 24) %>% accuracy(flyvninger),
)

flyvninger %>%
  model(ARIMA(log(antal_flyvninger_i_1000))) %>%
  forecast(h = "3 years") %>%
  autoplot(flyvninger)
# Husk at der også er en manuel vej til at ginde en "optimal" (S)ARIMA - model
# Det er vigtigt at have nogle velvalgte kandidatmodeller, som vi selv laver
# Husk i den forbindelse at bruge uniroot test:

#Hvad betyder det her egentlig 
flyvninger %>%
  features(log(antal_flyvninger_i_1000), unitroot_nsdiffs)

flyvninger %>%
  features(difference(log(antal_flyvninger_i_1000), 12), unitroot_nsdiffs)

# Brug derefter ACF og PACH til at finde kandidatmodeller. - Grov skitse
# Indenfor ikke sæson kan vi ikke anvende følgende
# Dette kan findes i kapitel 9
# Tjek om nedenstående er rigtigt
flyvninger_train %>%
  gg_tsdisplay(log(antal_flyvninger_i_1000), plot_type = "partial")

# TIPS
# Vær meget konsis
# Kapitel 5 eller 7 ift. - hvad man kan gøre med perioder, som corona
# evt. vælge at lave dummy variabler for følgende periode - hvis til den
# mundtlige eksamen

