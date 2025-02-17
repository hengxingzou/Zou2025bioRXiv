library(rstan)
library(bayesplot)

# Read raw data

counts_final = read_csv("Counts.csv")

# Caution: This script fit multiple Bayesian models with rstan and could take hours
# We provide coefficients extracted from the models for faster reproduction of results


########## Prepare for Model Fitting ##########


# Simultaneous arrival

onegen_0 = counts_final %>%
  filter(Num_Gen == 1, Arriv_Time == "0")
twogens_0 = counts_final %>%
  filter(Num_Gen == 2, Arriv_Time == "0")
threegens_0 = counts_final %>%
  filter(Num_Gen == 3, Arriv_Time == "0")

# Single species for fitting priors of lambdas

onegen_Tonly = onegen_0 %>% 
  filter(Start_F == 0)
onegen_Fonly = onegen_0 %>% 
  filter(Start_T == 0)
twogens_Tonly = twogens_0 %>% 
  filter(Start_F == 0)
twogens_Fonly = twogens_0 %>% 
  filter(Start_T == 0)
threegens_Tonly = threegens_0 %>% 
  filter(Start_F == 0)
threegens_Fonly = threegens_0 %>% 
  filter(Start_T == 0)

# T First, 6 Days

onegen_T6 = counts_final %>%
  filter(Num_Gen == 1, Arriv_Time == "T6")
twogens_T6 = counts_final %>%
  filter(Num_Gen == 2, Arriv_Time == "T6")
threegens_T6 = counts_final %>%
  filter(Num_Gen == 3, Arriv_Time == "T6")

# T First, 12 Days

onegen_T12 = counts_final %>%
  filter(Num_Gen == 1, Arriv_Time == "T12")
twogens_T12 = counts_final %>%
  filter(Num_Gen == 2, Arriv_Time == "T12")
threegens_T12 = counts_final %>%
  filter(Num_Gen == 3, Arriv_Time == "T12")

# T First, 18 Days

onegen_T18 = counts_final %>%
  filter(Num_Gen == 1, Arriv_Time == "T18")
twogens_T18 = counts_final %>%
  filter(Num_Gen == 2, Arriv_Time == "T18")
threegens_T18 = counts_final %>%
  filter(Num_Gen == 3, Arriv_Time == "T18")

# F first, 6 days

onegen_F6 = counts_final %>%
  filter(Num_Gen == 1, Arriv_Time == "F6")
twogens_F6 = counts_final %>%
  filter(Num_Gen == 2, Arriv_Time == "F6")
threegens_F6 = counts_final %>%
  filter(Num_Gen == 3, Arriv_Time == "F6")

# F First, 12 Days

onegen_F12 = counts_final %>%
  filter(Num_Gen == 1, Arriv_Time == "F12")
twogens_F12 = counts_final %>%
  filter(Num_Gen == 2, Arriv_Time == "F12")
threegens_F12 = counts_final %>%
  filter(Num_Gen == 3, Arriv_Time == "F12")

# F First, 18 Days

onegen_F18 = counts_final %>%
  filter(Num_Gen == 1, Arriv_Time == "F18")
twogens_F18 = counts_final %>%
  filter(Num_Gen == 2, Arriv_Time == "F18")
threegens_F18 = counts_final %>%
  filter(Num_Gen == 3, Arriv_Time == "F18")

# Parameters for model fitting

warmups = 50000
total_iterations = 100000
max_treedepth = 50
n_chains =  4
n_cores = 4
adapt_delta = 0.95


########## Model Fitting, One Generation ##########


# One generation, T only

data = list(N = nrow(onegen_Tonly), 
            Start = onegen_Tonly$Start_T, 
            Final = onegen_Tonly$Adults_T)

m_onegen_Tonly = stan(file = "Models/lv_Bayesian_single.stan", 
                      data = data,
                      chains = n_chains,
                      warmup = warmups, 
                      iter = total_iterations, 
                      cores = n_cores,
                      refresh = 250)

summary(m_onegen_Tonly)$summary["lambda", c("mean")]

# One generation, F only

data = list(N = nrow(onegen_Fonly), 
            Start = onegen_Fonly$Start_F, 
            Final = onegen_Fonly$Adults_F)

m_onegen_Tonly = stan(file = "Models/lv_Bayesian_single.stan", 
                      data = data,
                      chains = n_chains,
                      warmup = warmups, 
                      iter = total_iterations, 
                      cores = n_cores,
                      refresh = 250)

summary(m_onegen_Fonly)$summary["lambda", c("mean")]

# Setting priors for lambdas accordingly--for one generation vials,

# lambdas[1] ~ normal (0.715, 0.0715)
# lambdas[2] ~ normal (0.774, 0.0774)

# One generation, simultaneous arrival

data = list(N = nrow(onegen_0), 
            Start = t(as.matrix(onegen_0[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(onegen_0[, c("Adults_T", "Adults_F")])))

m_onegen_0 = stan(file = "Models/lv_Bayesian.stan", 
                  data = data,
                  chains = n_chains,
                  warmup = warmups, 
                  iter = total_iterations, 
                  cores = n_cores,
                  refresh = 250)

# One generation, T first, 6 days

data = list(N = nrow(onegen_T6), 
            Start = t(as.matrix(onegen_T6[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(onegen_T6[, c("Adults_T", "Adults_F")])))

m_onegen_T6 = stan(file = "Models/lv_Bayesian.stan", 
                   data = data,
                   chains = n_chains,
                   warmup = warmups, 
                   iter = total_iterations, 
                   cores = n_cores,
                   refresh = 250)

# One generation, T first, 12 days

data = list(N = nrow(onegen_T12), 
            Start = t(as.matrix(onegen_T12[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(onegen_T12[, c("Adults_T", "Adults_F")])))

m_onegen_T12 = stan(file = "Models/lv_Bayesian.stan", 
                    data = data,
                    chains = n_chains,
                    warmup = warmups, 
                    iter = total_iterations, 
                    cores = n_cores,
                    refresh = 250)

# One generation, T first, 18 days

data = list(N = nrow(onegen_T18), 
            Start = t(as.matrix(onegen_T18[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(onegen_T18[, c("Adults_T", "Adults_F")])))

m_onegen_T18 = stan(file = "Models/lv_Bayesian.stan", 
                    data = data,
                    chains = n_chains,
                    warmup = warmups, 
                    iter = total_iterations, 
                    cores = n_cores,
                    refresh = 250)

# One generation, F first, 6 days

data = list(N = nrow(onegen_F6), 
            Start = t(as.matrix(onegen_F6[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(onegen_F6[, c("Adults_T", "Adults_F")])))

m_onegen_F6 = stan(file = "Models/lv_Bayesian.stan",
                   data = data,
                   chains = n_chains,
                   warmup = warmups, 
                   iter = total_iterations, 
                   cores = n_cores,
                   refresh = 250)

# One generation, F first, 12 days

data = list(N = nrow(onegen_F12), 
            Start = t(as.matrix(onegen_F12[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(onegen_F12[, c("Adults_T", "Adults_F")])))

m_onegen_F12 = stan(file = "Models/lv_Bayesian.stan",
                    data = data,
                    chains = n_chains,
                    warmup = warmups, 
                    iter = total_iterations, 
                    cores = n_cores,
                    refresh = 250)

# One generation, F first, 18 days

data = list(N = nrow(onegen_F18), 
            Start = t(as.matrix(onegen_F18[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(onegen_F18[, c("Adults_T", "Adults_F")])))

m_onegen_F18 = stan(file = "Models/lv_Bayesian.stan",
                    data = data,
                    chains = n_chains,
                    warmup = warmups, 
                    iter = total_iterations, 
                    cores = n_cores,
                    refresh = 250)


########## Model Fitting, Two Generations ##########


# Two generations, T only

data = list(N = nrow(twogens_Tonly), 
            Start = twogens_Tonly$Start_T, 
            Final = twogens_Tonly$Adults_T)

m_twogens_Tonly = stan(file = "Models/lv_Bayesian_single.stan", 
                       data = data,
                       chains = n_chains,
                       warmup = warmups, 
                       iter = total_iterations, 
                       cores = n_cores,
                       refresh = 250)

summary(m_twogens_Tonly)$summary["lambda", c("mean", "sd")]

# Two generations, F only

data = list(N = nrow(twogens_Fonly), 
            Start = twogens_Fonly$Start_F, 
            Final = twogens_Fonly$Adults_F)

m_twogens_Fonly = stan(file = "Models/lv_Bayesian_single.stan", 
                       data = data,
                       chains = n_chains,
                       warmup = warmups, 
                       iter = total_iterations, 
                       cores = n_cores,
                       refresh = 250)

summary(m_twogens_Fonly)$summary["lambda", c("mean", "sd")]

# Setting priors for lambdas accordingly--for two-generation vials,

# lambdas[1] ~ normal (1.582, 0.1582)
# lambdas[2] ~ normal (3.685, 0.3685)

# Two generations, simultaneous arrival

data = list(N = nrow(twogens_0), 
            Start = t(as.matrix(twogens_0[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(twogens_0[, c("Adults_T", "Adults_F")])))

m_twogens_0 = stan(file = "Models/lv_Bayesian.stan",
                   data = data,
                   chains = n_chains,
                   warmup = warmups, 
                   iter = total_iterations, 
                   cores = n_cores,
                   refresh = 250)

# Two generations, T first, 6 days

data = list(N = nrow(twogens_T6), 
            Start = t(as.matrix(twogens_T6[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(twogens_T6[, c("Adults_T", "Adults_F")])))

m_twogens_T6 = stan(file = "Models/lv_Bayesian.stan",
                    data = data,
                    chains = n_chains,
                    warmup = warmups, 
                    iter = total_iterations, 
                    cores = n_cores,
                    refresh = 250)

# Two generations, T first, 12 days

data = list(N = nrow(twogens_T12), 
            Start = t(as.matrix(twogens_T12[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(twogens_T12[, c("Adults_T", "Adults_F")])))

m_twogens_T12 = stan(file = "Models/lv_Bayesian.stan", 
                     data = data,
                     chains = n_chains,
                     warmup = warmups, 
                     iter = total_iterations, 
                     cores = n_cores,
                     refresh = 250)

# Two generations, T first, 18 days

data = list(N = nrow(twogens_T18), 
            Start = t(as.matrix(twogens_T18[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(twogens_T18[, c("Adults_T", "Adults_F")])))

m_twogens_T18 = stan(file = "Models/lv_Bayesian.stan", 
                     data = data,
                     chains = n_chains,
                     warmup = warmups, 
                     iter = total_iterations, 
                     cores = n_cores,
                     refresh = 250)

# Two generations, F first, 6 days

data = list(N = nrow(twogens_F6), 
            Start = t(as.matrix(twogens_F6[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(twogens_F6[, c("Adults_T", "Adults_F")])))

m_twogens_F6 = stan(file = "Models/lv_Bayesian.stan", 
                    data = data,
                    chains = n_chains,
                    warmup = warmups, 
                    iter = total_iterations, 
                    cores = n_cores,
                    refresh = 250)

# Two generations, F first, 12 days

data = list(N = nrow(twogens_F12), 
            Start = t(as.matrix(twogens_F12[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(twogens_F12[, c("Adults_T", "Adults_F")])))

m_twogens_F12 = stan(file = "Models/lv_Bayesian.stan", 
                     data = data,
                     chains = n_chains,
                     warmup = warmups, 
                     iter = total_iterations, 
                     cores = n_cores,
                     refresh = 250)

# Two generations, F first, 18 days

data = list(N = nrow(twogens_F18), 
            Start = t(as.matrix(twogens_F18[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(twogens_F18[, c("Adults_T", "Adults_F")])))

m_twogens_F18 = stan(file = "Models/lv_Bayesian.stan", 
                     data = data,
                     chains = n_chains,
                     warmup = warmups, 
                     iter = total_iterations, 
                     cores = n_cores,
                     refresh = 250)


########## Model Fitting, Three Generations ##########


# Three generations, T only

data = list(N = nrow(threegens_Tonly), 
            Start = threegens_Tonly$Start_T, 
            Final = threegens_Tonly$Adults_T)

m_threegens_Tonly = stan(file = "Models/lv_Bayesian_single.stan", 
                         data = data,
                         chains = n_chains,
                         warmup = warmups, 
                         iter = total_iterations, 
                         cores = n_cores,
                         refresh = 250)

summary(m_threegens_Tonly)$summary["lambda", c("mean")]

# Three generations, F only

data = list(N = nrow(threegens_Fonly), 
            Start = threegens_Fonly$Start_F, 
            Final = threegens_Fonly$Adults_F)

m_threegens_Fonly = stan(file = "Models/lv_Bayesian_single.stan", 
                         data = data,
                         chains = n_chains,
                         warmup = warmups, 
                         iter = total_iterations, 
                         cores = n_cores,
                         refresh = 250)

summary(m_threegens_Fonly)$summary["lambda", c("mean")]

# Setting priors for lambdas accordingly--for three-generation vials,

# lambdas[1] ~ normal (2.651, 0.2651)
# lambdas[2] ~ normal (3.365, 0.3365)

# Three generations, simultaneous arrival

data = list(N = nrow(threegens_0), 
            Start = t(as.matrix(threegens_0[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(threegens_0[, c("Adults_T", "Adults_F")])))

m_threegens_0 = stan(file = "Models/lv_Bayesian.stan",
                     data = data,
                     chains = n_chains,
                     warmup = warmups, 
                     iter = total_iterations, 
                     cores = n_cores,
                     refresh = 250)

# Three generations, T first, 6 days

data = list(N = nrow(threegens_T6), 
            Start = t(as.matrix(threegens_T6[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(threegens_T6[, c("Adults_T", "Adults_F")])))

m_threegens_T6 = stan(file = "Models/lv_Bayesian.stan", 
                      data = data,
                      chains = n_chains,
                      warmup = warmups, 
                      iter = total_iterations, 
                      cores = n_cores,
                      refresh = 250)

# Three generations, T first, 12 days

data = list(N = nrow(threegens_T12), 
            Start = t(as.matrix(threegens_T12[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(threegens_T12[, c("Adults_T", "Adults_F")])))

m_threegens_T12 = stan(file = "Models/lv_Bayesian.stan", 
                       data = data,
                       chains = n_chains,
                       warmup = warmups, 
                       iter = total_iterations, 
                       cores = n_cores,
                       refresh = 250)

# Three generations, T first, 18 days

data = list(N = nrow(threegens_T18), 
            Start = t(as.matrix(threegens_T18[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(threegens_T18[, c("Adults_T", "Adults_F")])))

m_threegens_T18 = stan(file = "Models/lv_Bayesian.stan", 
                       data = data,
                       chains = n_chains,
                       warmup = warmups, 
                       iter = total_iterations, 
                       cores = n_cores,
                       refresh = 250)

# Three generations, F first, 6 days

data = list(N = nrow(threegens_F6), 
            Start = t(as.matrix(threegens_F6[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(threegens_F6[, c("Adults_T", "Adults_F")])))

m_threegens_F6 = stan(file = "Models/lv_Bayesian.stan", 
                      data = data,
                      chains = n_chains,
                      warmup = warmups, 
                      iter = total_iterations, 
                      cores = n_cores,
                      refresh = 250)

# Three generations, F first, 12 days

data = list(N = nrow(threegens_F12), 
            Start = t(as.matrix(threegens_F12[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(threegens_F12[, c("Adults_T", "Adults_F")])))

m_threegens_F12 = stan(file = "Models/lv_Bayesian.stan", 
                       data = data,
                       chains = n_chains,
                       warmup = warmups, 
                       iter = total_iterations, 
                       cores = n_cores,
                       refresh = 250)

# Three generations, F first, 18 days

data = list(N = nrow(threegens_F18), 
            Start = t(as.matrix(threegens_F18[, c("Start_T", "Start_F")])), 
            Final = t(as.matrix(threegens_F18[, c("Adults_T", "Adults_F")])))

m_threegens_F18 = stan(file = "Models/lv_Bayesian.stan", 
                       data = data,
                       chains = n_chains,
                       warmup = warmups, 
                       iter = total_iterations, 
                       cores = n_cores,
                       refresh = 250)


########## Extracting Fitted Coefficients ##########


# Single-species vials

coeff_onegen_Tonly = as.data.frame(m_onegen_Tonly) %>% mutate(Num_Gen = 1, Species = "T")
coeff_onegen_Fonly = as.data.frame(m_onegen_Fonly) %>% mutate(Num_Gen = 1, Species = "F")

coeff_twogen_Tonly = as.data.frame(m_twogens_Tonly) %>% mutate(Num_Gen = 2, Species = "T")
coeff_twogen_Fonly = as.data.frame(m_twogens_Fonly) %>% mutate(Num_Gen = 2, Species = "F")

coeff_threegen_Tonly = as.data.frame(m_threegens_Tonly) %>% mutate(Num_Gen = 3, Species = "T")
coeff_threegen_Fonly = as.data.frame(m_threegens_Fonly) %>% mutate(Num_Gen = 3, Species = "F")

all_coeffs_single = rbind(coeff_onegen_Tonly, coeff_onegen_Fonly, 
                          coeff_twogen_Tonly, coeff_twogen_Fonly, 
                          coeff_threegen_Tonly, coeff_threegen_Fonly)

write_csv(all_coeffs_single, "Coefficients/SingleSpecies.csv")

# Two-species vials

coeff_onegen_0 = as.data.frame(m_onegen_0) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 1, Relative_Arriv_Time = 0)
coeff_twogens_0 = as.data.frame(m_twogens_0) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 2, Relative_Arriv_Time = 0)
coeff_threegens_0 = as.data.frame(m_threegens_0) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 3, Relative_Arriv_Time = 0)

coeff_onegen_T6 = as.data.frame(m_onegen_T6) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 1, Relative_Arriv_Time = -6)
coeff_onegen_T12 = as.data.frame(m_onegen_T12) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 1, Relative_Arriv_Time = -12)
coeff_onegen_T18 = as.data.frame(m_onegen_T18) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 1, Relative_Arriv_Time = -18)

coeff_twogens_T6 = as.data.frame(m_twogens_T6) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 2, Relative_Arriv_Time = -6)
coeff_twogens_T12 = as.data.frame(m_twogens_T12) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 2, Relative_Arriv_Time = -12)
coeff_twogens_T18 = as.data.frame(m_twogens_T18) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 2, Relative_Arriv_Time = -18)

coeff_threegens_T6 = as.data.frame(m_threegens_T6) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 3, Relative_Arriv_Time = -6)
coeff_threegens_T12 = as.data.frame(m_threegens_T12) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 3, Relative_Arriv_Time = -12)
coeff_threegens_T18 = as.data.frame(m_threegens_T18) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 3, Relative_Arriv_Time = -18)

coeff_onegen_F6 = as.data.frame(m_onegen_F6) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 1, Relative_Arriv_Time = 6)
coeff_onegen_F12 = as.data.frame(m_onegen_F12) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 1, Relative_Arriv_Time = 12)
coeff_onegen_F18 = as.data.frame(m_onegen_F18) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 1, Relative_Arriv_Time = 18)

coeff_twogens_F6 = as.data.frame(m_twogens_F6) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 2, Relative_Arriv_Time = 6)
coeff_twogens_F12 = as.data.frame(m_twogens_F12) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 2, Relative_Arriv_Time = 12)
coeff_twogens_F18 = as.data.frame(m_twogens_F18) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 2, Relative_Arriv_Time = 18)

coeff_threegens_F6 = as.data.frame(m_threegens_F6) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 3, Relative_Arriv_Time = 6)
coeff_threegens_F12 = as.data.frame(m_threegens_F12) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 3, Relative_Arriv_Time = 12)
coeff_threegens_F18 = as.data.frame(m_threegens_F18) %>% 
  select(starts_with(c("alpha", "lambda"))) %>% 
  mutate(Num_Gen = 3, Relative_Arriv_Time = 18)

all_coeffs = rbind(coeff_onegen_0, coeff_onegen_T6, coeff_onegen_T12, coeff_onegen_T18,
                   coeff_onegen_F6, coeff_onegen_F12, coeff_onegen_F18, 
                   coeff_twogens_0, coeff_twogens_T6, coeff_twogens_T12, coeff_twogens_T18, 
                   coeff_twogens_F6, coeff_twogens_F12, coeff_twogens_F18, 
                   coeff_threegens_0, coeff_threegens_T6, coeff_threegens_T12, coeff_threegens_T18, 
                   coeff_threegens_F6, coeff_threegens_F12, coeff_threegens_F18) %>% 
  rename(alpha11 = `alphas[1,1]`, alpha12 = `alphas[1,2]`, 
         alpha21 = `alphas[2,1]`, alpha22 = `alphas[2,2]`, 
         lambda1 = `lambdas[1]`, lambda2 = `lambdas[2]`)

write_csv(all_coeffs, "Coefficients/TwoSpecies.csv")
