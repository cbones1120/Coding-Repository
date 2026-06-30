library(tidyverse)
library(radiant)
library(lfe)

dat <- read_csv("randomized_data_for_hw.csv")

# Log-transformed Brand Attitude Metrics
dat$qua_ln <- log1p(dat$qua)
dat$val_ln <- log1p(dat$val)
dat$sat_ln <- log1p(dat$sat)

# Log-transformed Ad Metrics
dat$nat_ln <- log1p(dat$nat)
dat$loc_ln <- log1p(dat$loc)
dat$dig_ln <- log1p(dat$dig)

# Log-transformed Competitor Ad Metrics
dat$compnat_ln <- log1p(dat$compnat)
dat$comploc_ln <- log1p(dat$comploc)
dat$compdig_ln <- log1p(dat$compdig)

# 13 lag variables for each brand attitude metric
dat <- dat %>% 
  group_by(Brand) %>%
  mutate(lag1qua = lag(qua_ln, n = 1),
         lag2qua = lag(qua_ln, n = 2),
         lag3qua = lag(qua_ln, n =3),
         lag4qua = lag(qua_ln, n =4),
         lag5qua = lag(qua_ln, n =5),
         lag6qua = lag(qua_ln, n =6),
         lag7qua = lag(qua_ln, n =7),
         lag8qua = lag(qua_ln, n =8),
         lag9qua = lag(qua_ln, n =9),
         lag10qua = lag(qua_ln, n =10),
         lag11qua = lag(qua_ln, n =11),
         lag12qua = lag(qua_ln, n =12),
         lag13qua = lag(qua_ln, n =13)) %>%
  mutate(lag1val = lag(val_ln, n = 1),
         lag2val = lag(val_ln, n = 2),
         lag3val = lag(val_ln, n =3),
         lag4val = lag(val_ln, n =4),
         lag5val = lag(val_ln, n =5),
         lag6val = lag(val_ln, n =6),
         lag7val = lag(val_ln, n =7),
         lag8val = lag(val_ln, n =8),
         lag9val = lag(val_ln, n =9),
         lag10val = lag(val_ln, n =10),
         lag11val = lag(val_ln, n = 11),
         lag12val = lag(val_ln, n = 12),
         lag13val = lag(val_ln, n = 13)) %>%
  mutate(lag1sat = lag(sat_ln, n = 1),
         lag2sat = lag(sat_ln, n = 2),
         lag3sat = lag(sat_ln, n =3),
         lag4sat = lag(sat_ln, n =4),
         lag5sat = lag(sat_ln, n =5),
         lag6sat = lag(sat_ln, n =6),
         lag7sat = lag(sat_ln, n =7),
         lag8sat = lag(sat_ln, n =8),
         lag9sat = lag(sat_ln, n =9),
         lag10sat = lag(sat_ln, n =10),
         lag11sat = lag(sat_ln, n =11),
         lag12sat = lag(sat_ln, n =12),
         lag13sat = lag(sat_ln, n =13))

# 5 lags for each advertising spend metric
dat <- dat %>%
  group_by(Brand) %>%
  mutate(lag1nat = lag(nat_ln, n = 1),
         lag2nat = lag(nat_ln, n = 2),
         lag3nat = lag(nat_ln, n = 3),
         lag4nat = lag(nat_ln, n = 4),
         lag5nat = lag(nat_ln, n = 5)) %>%
  mutate(lag1loc = lag(loc_ln, n = 1),
         lag2loc = lag(loc_ln, n = 2),
         lag3loc = lag(loc_ln, n = 3),
         lag4loc = lag(loc_ln, n = 4),
         lag5loc = lag(loc_ln, n = 5)) %>%
  mutate(lag1dig = lag(dig_ln, n = 1),
         lag2dig = lag(dig_ln, n = 2),
         lag3dig = lag(dig_ln, n = 3),
         lag4dig = lag(dig_ln, n = 4),
         lag5dig = lag(dig_ln, n = 5)) 

# 5 Lags for each competitor advertising spend metric  
dat <- dat %>%
  group_by(Brand) %>%
  mutate(lag1compnat = lag(compnat_ln, n = 1),
         lag2compnat = lag(compnat_ln, n = 2),
         lag3compnat = lag(compnat_ln, n = 3),
         lag4compnat = lag(compnat_ln, n = 4),
         lag5compnat = lag(compnat_ln, n = 5)) %>%
  mutate(lag1comploc = lag(comploc_ln, n = 1),
         lag2comploc = lag(comploc_ln, n = 2),
         lag3comploc = lag(comploc_ln, n = 3),
         lag4comploc = lag(comploc_ln, n = 4),
         lag5comploc = lag(comploc_ln, n = 5)) %>%
  mutate(lag1compdig = lag(compdig_ln, n = 1),
         lag2compdig = lag(compdig_ln, n = 2),
         lag3compdig = lag(compdig_ln, n = 3),
         lag4compdig = lag(compdig_ln, n = 4),
         lag5compdig = lag(compdig_ln, n = 5))

# Dropping the first13 obs for each brand
dat <- dat %>%
  group_by(Brand) %>%
  slice(14:n())

write.csv(dat,"data_cleaned.csv", row.names = FALSE)

# Section 2 ------------------------------

# 2.1 Regression
lm_nat <- lm(qua_ln~lag1nat+lag2nat+lag3nat+lag4nat+lag5nat+lag1compnat+lag2compnat+lag3compnat+lag4compnat+lag5compnat+
               lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
               lag10qua+lag11qua+lag12qua+lag13qua+nat_ln+compnat_ln+
               as.factor(Brand)+as.factor(time_period), data=dat)

summary(lm_nat)

lm_loc <- lm(qua_ln~lag1loc+lag2loc+lag3loc+lag4loc+lag5loc+lag1comploc+lag2comploc+lag3comploc+lag4comploc+lag5comploc+
               lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
               lag10qua+lag11qua+lag12qua+lag13qua+loc_ln+comploc_ln+
               as.factor(Brand)+as.factor(time_period), data=dat)

summary(lm_loc)

lm_dig <- lm(qua_ln~lag1dig+lag2dig+lag3dig+lag4dig+lag5dig+lag1compdig+lag2compdig+lag3compdig+lag4compdig+lag5compdig+
               lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
               lag10qua+lag11qua+lag12qua+lag13qua+dig_ln+compdig_ln+
               as.factor(Brand)+as.factor(time_period), data=dat)

summary(lm_dig)

# 2.2 Regression using Felm 
felm_nat <- felm(qua_ln~lag1nat+lag2nat+lag3nat+lag4nat+lag5nat+lag1compnat+lag2compnat+lag3compnat+lag4compnat+lag5compnat+
                   lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                   lag10qua+lag11qua+lag12qua+lag13qua+nat_ln+compnat_ln|
                   as.factor(Brand)+as.factor(time_period), data=dat)

summary(felm_nat)

felm_loc <- felm(qua_ln~lag1loc+lag2loc+lag3loc+lag4loc+lag5loc+lag1comploc+lag2comploc+lag3comploc+lag4comploc+lag5comploc+
                   lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                   lag10qua+lag11qua+lag12qua+lag13qua+loc_ln+comploc_ln|
                   as.factor(Brand)+as.factor(time_period), data=dat)

summary(felm_loc) 

felm_dig <- felm(qua_ln~lag1dig+lag2dig+lag3dig+lag4dig+lag5dig+lag1compdig+lag2compdig+lag3compdig+lag4compdig+lag5compdig+
                   lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                   lag10qua+lag11qua+lag12qua+lag13qua+dig_ln+compdig_ln|
                   as.factor(Brand)+as.factor(time_period), data=dat)

summary(felm_dig) 


# 2.3 Regression using Felm, weight = data$qua_vol
felm_weight_nat <- felm(qua_ln~lag1nat+lag2nat+lag3nat+lag4nat+lag5nat+lag1compnat+lag2compnat+lag3compnat+lag4compnat+lag5compnat+
                          lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                          lag10qua+lag11qua+lag12qua+lag13qua+nat_ln+compnat_ln|
                          as.factor(Brand)+as.factor(time_period), data=dat, weight = dat$qua_vol)

summary(felm_weight_nat)

felm_weight_loc <- felm(qua_ln~lag1loc+lag2loc+lag3loc+lag4loc+lag5loc+lag1comploc+lag2comploc+lag3comploc+lag4comploc+lag5comploc+
                          lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                          lag10qua+lag11qua+lag12qua+lag13qua+loc_ln+comploc_ln|
                          as.factor(Brand)+as.factor(time_period), data=dat, weight = dat$qua_vol)

summary(felm_weight_loc)

felm_weight_dig <- felm(qua_ln~lag1dig+lag2dig+lag3dig+lag4dig+lag5dig+lag1compdig+lag2compdig+lag3compdig+lag4compdig+lag5compdig+
                          lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                          lag10qua+lag11qua+lag12qua+lag13qua+dig_ln+compdig_ln|
                          as.factor(Brand)+as.factor(time_period), data=dat, weight = dat$qua_vol)

summary(felm_weight_dig)

# 2.4
felm_weight_nat_bq <- felm(qua_ln~lag1nat+lag2nat+lag3nat+lag4nat+lag5nat+lag1compnat+lag2compnat+lag3compnat+lag4compnat+lag5compnat+
                             lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                             lag10qua+lag11qua+lag12qua+lag13qua+nat_ln+compnat_ln|
                             as.factor(Brand)+as.factor(yrqtr), data=dat, weight = dat$qua_vol)

summary(felm_weight_nat_bq)


felm_weight_loc_bq <- felm(qua_ln~lag1loc+lag2loc+lag3loc+lag4loc+lag5loc+lag1comploc+lag2comploc+lag3comploc+lag4comploc+lag5comploc+
                             lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                             lag10qua+lag11qua+lag12qua+lag13qua+loc_ln+comploc_ln|
                             as.factor(Brand)+as.factor(yrqtr), data=dat, weight = dat$qua_vol)

summary(felm_weight_loc_bq)


felm_weight_dig_bq <- felm(qua_ln~lag1dig+lag2dig+lag3dig+lag4dig+lag5dig+lag1compdig+lag2compdig+lag3compdig+lag4compdig+lag5compdig+
                             lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                             lag10qua+lag11qua+lag12qua+lag13qua+dig_ln+compdig_ln|
                             as.factor(Brand)+as.factor(yrqtr), data=dat, weight = dat$qua_vol)

summary(felm_weight_dig_bq)

# 2.5
felm_weight_nat_iw <- felm(qua_ln~lag1nat+lag2nat+lag3nat+lag4nat+lag5nat+lag1compnat+lag2compnat+lag3compnat+lag4compnat+lag5compnat+
                             lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                             lag10qua+lag11qua+lag12qua+lag13qua+nat_ln+compnat_ln|
                             as.factor(Industry)+as.factor(time_period), data=dat, weight = dat$qua_vol)

summary(felm_weight_nat_iw)


felm_weight_loc_iw <- felm(qua_ln~lag1loc+lag2loc+lag3loc+lag4loc+lag5loc+lag1comploc+lag2comploc+lag3comploc+lag4comploc+lag5comploc+
                             lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                             lag10qua+lag11qua+lag12qua+lag13qua+loc_ln+comploc_ln|
                             as.factor(Industry)+as.factor(time_period), data=dat, weight = dat$qua_vol)

summary(felm_weight_loc_iw)


felm_weight_dig_iw <- felm(qua_ln~lag1dig+lag2dig+lag3dig+lag4dig+lag5dig+lag1compdig+lag2compdig+lag3compdig+lag4compdig+lag5compdig+
                             lag1qua+lag2qua+lag3qua+lag4qua+lag5qua+lag6qua+lag7qua+lag8qua+lag9qua+
                             lag10qua+lag11qua+lag12qua+lag13qua+dig_ln+compdig_ln|
                             as.factor(Industry)+as.factor(time_period), data=dat, weight = dat$qua_vol)

summary(felm_weight_dig_iw)

#2.6
library(stargazer)
stargazer(felm_nat, felm_weight_nat, felm_weight_nat_bq, felm_weight_nat_iw, 
          felm_loc, felm_weight_loc, felm_weight_loc_bq, felm_weight_loc_iw,
          felm_dig, felm_weight_dig, felm_weight_dig_bq, felm_weight_dig_iw, 
          type = "html", column.labels   = c("Nation", "Local", "Digital"),
          column.separate = c(4, 4, 4), dep.var.caption = "",
          title = "Advertising Coefficient", out = "table.html")


