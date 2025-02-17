library(tidyverse)
library(patchwork)

# Plotting setup

theme_set(theme_bw())
color_scheme = c("#a61b29", "#0eb0c9")

# Read raw data

counts_final = read_csv("Counts.csv")

# Read fitted coefficients

all_coeffs_single = read_csv("Coefficients/SingleSpecies.csv")
all_coeffs = read_csv("Coefficients/TwoSpecies.csv")


########## Intrinsic Growth Rates (Figure S6) ##########


lambda_plot = all_coeffs_single %>% 
  
  # calculate quantiles and median
  group_by(Species, Num_Gen) %>% 
  mutate(Q10 = quantile(lambda, 0.1), Q90 = quantile(lambda, 0.9), Median = median(lambda)) %>% 
  
  # only include 10% and 90% quantiles
  filter(lambda >= Q10 & lambda <= Q90) %>% 
  
  # only include quantiles and median
  select(Num_Gen, Species, lambda, Q10, Q90, Median) %>% 
  distinct(Q10, Q90, Median)

p_lambda = lambda_plot %>% 
  # plotting
  ggplot(aes(x = as.factor(Num_Gen), y = Median, color = as.factor(Species))) +
  geom_errorbar(aes(ymin = Q10, ymax = Q90), width = 0, linewidth = 1, position = position_dodge(0.5)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  # geom_boxplot() +
  # geom_violin(draw_quantiles = c(0.5)) +
  scale_color_manual(name = "Coefficients", values = c("#a61b29", "#0eb0c9"),
                     labels = c(expression(paste(lambda[`F`])), expression(paste(lambda[`T`])))) +
  xlab("Number of Generations in A Season") +
  ylab("Intrinsic Growth Rate") + 
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 15), axis.title = element_text(size = 20), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), 
        strip.text = element_text(size = 15))

p_lambda


########## Competition Coefficients ##########


# Competition coefficients and relative arrival times (Figure 3)

coeffs_plot = 
  all_coeffs %>% 
  pivot_longer(starts_with("alpha"), names_to = "Coeff", values_to = "Value") %>% 
  
  # calculate quantiles and median
  group_by(Coeff, Num_Gen, Relative_Arriv_Time) %>% 
  mutate(Q10 = quantile(Value, 0.1), Q90 = quantile(Value, 0.9), Median = median(Value)) %>% 
  
  # only include 10% and 90% quantiles
  filter(Value >= Q10 & Value <= Q90) %>% 
  
  # filter coefficients
  filter(grepl("12|21", Coeff)) %>%
  # filter(grepl("11|22", Coeff)) %>% 
  
  # filter by numbers of generations
  # filter(Num_Gen == 2) %>%
  
  # only include quantiles and median
  select(Num_Gen, Relative_Arriv_Time, Coeff, Q10, Q90, Median) %>% 
  distinct(Q10, Q90, Median)

p_coeff = coeffs_plot %>% 
  # filter(!(Num_Gen == 1 & Relative_Arriv_Time == -18)) %>% 
  ggplot(aes(x = as.factor(Relative_Arriv_Time), y = Median, color = Coeff)) +
  geom_errorbar(aes(ymin = Q10, ymax = Q90), width = 0, linewidth = 1, position = position_dodge(0.5)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  # geom_boxplot() +
  # geom_violin(draw_quantiles = c(0.5)) +
  scale_color_manual(name = "Coefficients", values = c("#a61b29","#0eb0c9"), 
                     labels = c(expression(paste(alpha[TF])), expression(paste(alpha[FT])))) + 
  xlab("Relative Arrival Times") +
  ylab("Interaction Strength") + 
  # scale_y_log10() +
  facet_wrap(~ Num_Gen, 
             labeller = labeller(Num_Gen = c(`1` = "1 Generation", `2` = "2 Generations", `3` = "3 Generations"))) + 
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 15), axis.title = element_text(size = 20), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), 
        strip.text = element_text(size = 15))

p_coeff

# Helper function for the coexistence space

generate_baseplot = function(ND_min, ND_max, RFD_min, RFD_max, alpha_val = 0.15) {
  
  ND_range = seq(ND_min, ND_max, 0.001)
  nd_rfd_line = rbind(data.frame(ND = ND_range, RFD = 1/(1-ND_range), type = "upper"),
                      data.frame(ND = ND_range, RFD = 1-ND_range, type = "lower"))
  baseplot =
    ggplot() +
    
    geom_line(data = nd_rfd_line %>% filter(type == "lower"), aes(x = ND, y = RFD), color = "black", linetype = 1) + 
    geom_line(data = nd_rfd_line %>% filter(type == "upper"), aes(x = ND, y = RFD), color = "black", linetype = 1) + 
    
    # Species 2 wins (confusum)
    geom_ribbon(data = nd_rfd_line %>% filter(type == "upper", ND >= 0),
                aes(x = ND, ymin = (RFD), ymax = RFD_max), fill = "#a61b29", alpha = alpha_val) +
    geom_ribbon(data = nd_rfd_line %>% filter(type == "lower", ND <= 0),
                aes(x = ND, ymin = (RFD), ymax = RFD_max), fill = "#a61b29", alpha = alpha_val) +
    
    # Species 1 wins (castaneum)
    geom_ribbon(data = nd_rfd_line %>% filter(type == "lower", ND >= 0),
                aes(x = ND, ymin = RFD_min, ymax = (RFD)), fill = "#0eb0c9", alpha = alpha_val) +
    geom_ribbon(data = nd_rfd_line %>% filter(type == "upper", ND <= 0),
                aes(x = ND, ymin = RFD_min, ymax = (RFD)), fill = "#0eb0c9", alpha = alpha_val) +
    
    theme_bw() + theme(panel.grid = element_blank())
  
  return(baseplot)
}

# Calculate stabilization potential and relative fitness differences

nd_rfd = 
  all_coeffs %>% 
  group_by(Num_Gen, Relative_Arriv_Time) %>% 
  filter(across(starts_with("alpha"), ~ .x > quantile(.x, 0.1) & .x < quantile(.x, 0.9))) %>% 
  slice_sample(n = 1000) %>% 
  mutate(ND = 1-sqrt((alpha12 * alpha21) / (alpha11 * alpha22)), 
         RFD = sqrt((alpha11 * alpha12) / (alpha22 * alpha21))) %>% 
  summarize(mean_ND = mean(ND), mean_RFD = mean(RFD), 
            sd_ND = sd(ND), sd_RFD = sd(RFD))

# Plotting

ND_min = -8.75
ND_max = 1
RFD_min = 0.05
RFD_max = 8

p_nd_rfd = 
  generate_baseplot(ND_min, ND_max, RFD_min, RFD_max) + 
  geom_path(data = nd_rfd,
            aes(x = mean_ND, y = mean_RFD)) +
  geom_pointrange(data = nd_rfd, 
                  aes(x = mean_ND, y = mean_RFD,
                      xmin = mean_ND-sd_ND, xmax = mean_ND+sd_ND, 
                      color = as.factor(Relative_Arriv_Time))) + 
  geom_pointrange(data = nd_rfd,
                  aes(x = mean_ND, y = mean_RFD,
                      ymin = mean_RFD-sd_RFD, ymax = mean_RFD+sd_RFD, 
                      color = as.factor(Relative_Arriv_Time))) +
  geom_hline(yintercept = 1, linetype = 2, color = "#577C8A") + 
  geom_vline(xintercept = 0, linetype = 2, color = "#577C8A") + 
  scale_x_continuous(lim = c(ND_min, ND_max), name = "Stabilization Potential") +
  scale_y_log10(lim = c(RFD_min, RFD_max), name = "Relative Fitness Difference") +
  scale_color_manual(name = "Relative Arrival Times",
                     values = c("#B8B8B8", "#9C9C9C", "#7F7F7F", "#636363", "#474747", "#2B2B2B", "#0F0F0F")) +
  facet_wrap(~ Num_Gen, 
             labeller = labeller(Num_Gen = c(`1` = "1 Generation", `2` = "2 Generations", `3` = "3 Generations"))) + 
  theme(axis.text = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.ticks = element_blank(), plot.title = element_text(size = 17), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), legend.position = "bottom", 
        strip.text = element_text(size = 15)) + 
  guides(color = guide_legend(nrow = 1))

p_nd_rfd


########## Compare Fitted and Actual Population Data (Figure S1-S2) ##########


# Actual data

actual_lambdas_T = 
  counts_final %>% 
  filter(!(Start_T == 0 | Start_F == 0)) %>% 
  mutate(Actual_lambda_T = Adults_T/Start_T) %>% 
  unite("Start_Pop", Start_T:Start_F, sep = ":", remove = F)

actual_lambdas_F = 
  counts_final %>% 
  filter(!(Start_T == 0 | Start_F == 0)) %>% 
  mutate(Actual_lambda_F = Adults_F/Start_F) %>% 
  unite("Start_Pop", Start_T:Start_F, sep = ":", remove = F)

# Fitted data

Start_T = c(10, 15, 20, 20, 30, 40)
Start_F = c(20, 15, 10, 40, 30, 20)

coeffs_samples_T = 
  all_coeffs %>% 
  group_by(Num_Gen, Relative_Arriv_Time) %>% 
  select(-starts_with("sigma"), -`lambda2`) %>% 
  slice_sample(n = 1000) %>%
  slice(rep(1:n(), first(length(Start_T)))) %>% 
  mutate(Start_T = rep(Start_T, 1000), 
         Start_F = rep(Start_F, 1000)) %>% 
  mutate(Estimated_lambda_T = lambda1*(1-alpha11*Start_T-alpha12*Start_F)) %>% 
  unite("Start_Pop", Start_T:Start_F, sep = ":", remove = F)

coeffs_samples_F = 
  all_coeffs %>% 
  group_by(Num_Gen, Relative_Arriv_Time) %>% 
  select(-starts_with("sigma"), -`lambda1`) %>% 
  slice_sample(n = 1000) %>% 
  slice(rep(1:n(), first(length(Start_T)))) %>% 
  mutate(Start_T = rep(Start_T, 1000), 
         Start_F = rep(Start_F, 1000)) %>% 
  mutate(Estimated_lambda_F = lambda2*(1-alpha22*Start_F-alpha21*Start_T)) %>% 
  unite("Start_Pop", Start_T:Start_F, sep = ":", remove = F)

# Plotting

p_compare_T = ggplot() + 
  tidybayes::stat_interval(data = coeffs_samples_T,
                           aes(x = Start_Pop, y = Estimated_lambda_T)) +
  geom_point(data = actual_lambdas_T, 
             aes(x = Start_Pop, y = Actual_lambda_T), 
             color = "#0eb0c9", size = 1) +
  scale_color_grey(start = 0.7, end = 0.1) +
  xlab("Initial Density (T:F)") + ylab("Actual and Estimated Growth") +
  ggh4x::facet_grid2(Num_Gen ~ Relative_Arriv_Time, 
                     scale = "free_y", 
                     labeller = labeller(Num_Gen = c(`1` = "1 Generation", `2` = "2 Generations", `3` = "3 Generations"))) + 
  theme(panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size = 12, angle = 60, vjust = 0.5), 
        axis.text.y = element_text(size = 15), axis.title = element_text(size = 20), 
        legend.position = "none", 
        strip.text = element_text(size = 15))

p_compare_F = 
  ggplot() + 
  tidybayes::stat_interval(data = coeffs_samples_F,
                           aes(x = Start_Pop, y = Estimated_lambda_F)) +
  geom_point(data = actual_lambdas_F, 
             aes(x = Start_Pop, y = Actual_lambda_F), 
             color = "#a61b29", size = 1) +
  scale_color_grey(start = 0.7, end = 0.1) +
  xlab("Initial Density (T:F)") + ylab("Actual and Estimated Growth") +
  ggh4x::facet_grid2(Num_Gen ~ Relative_Arriv_Time, 
                     scale = "free_y", 
                     labeller = labeller(Num_Gen = c(`1` = "1 Generation", `2` = "2 Generations", `3` = "3 Generations"))) + 
  theme(panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size = 12, angle = 60, vjust = 0.5), 
        axis.text.y = element_text(size = 15), axis.title = element_text(size = 20), 
        legend.position = "none",
        strip.text = element_text(size = 15))

p_compare_T
p_compare_F
