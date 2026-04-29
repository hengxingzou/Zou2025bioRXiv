library(tidyverse)
library(readxl)
library(lmerTest)
library(patchwork)

# Plotting setup

theme_set(theme_bw())
color_scheme = c("#a61b29", "#0eb0c9")

# Read raw data

counts_final = read_csv("Counts.csv")

# Get two-species vial data only

counts_final_twospp = counts_final %>% 
  filter(Start_T != 0 & Start_F != 0) %>% 
  mutate(Ratio = Start_T/Start_F, Total = Start_T + Start_F)


########### Final Population ##########


# Heatmap of final population (Figure 2)

p_final_T = counts_final_twospp %>%
  group_by(Relative_Arriv_Time, Num_Gen, Ratio, Total) %>% 
  summarize(Mean_T = mean(Adults_T)) %>% 
  ggplot(aes(x = as.factor(Relative_Arriv_Time), y = as.factor(Ratio), fill = Mean_T)) + 
  geom_tile() + 
  ggtitle("A") + 
  scale_x_discrete(name = "Relative Arrival Time") + 
  scale_y_discrete(name = "Initial T: Initial F", labels = c("1:2", "1:1", "2:1")) +
  scale_fill_gradient2(low = "#91dbe6", high = "#0eb0c9",
                       name = "Final Density of T. castaneum", 
                       n.breaks = 4) + 
  facet_grid(Num_Gen ~ Total, 
             labeller = labeller(Total = c(`30` = "Total 30 Eggs", `60` = "Total 60 Eggs"),
                                 Num_Gen = c(`1` = "1 Generation", `2` = "2 Generations", `3` = "3 Generations"))) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 15), 
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.ticks = element_blank(), plot.title = element_text(size = 17), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), legend.position = "bottom",
        strip.text.x = element_text(size = 15), strip.text.y = element_blank())

p_final_F = counts_final_twospp %>% 
  group_by(Relative_Arriv_Time, Num_Gen, Ratio, Total) %>% 
  summarize(Mean_F = mean(Adults_F)) %>% 
  ggplot(aes(x = as.factor(Relative_Arriv_Time), y = as.factor(Ratio), fill = Mean_F)) + 
  geom_tile() + 
  ggtitle("B") +
  scale_x_discrete(name = "Relative Arrival Time") + 
  scale_y_discrete(name = "Initial T: Initial F", labels = c("1:2", "1:1", "2:1")) +
  scale_fill_gradient2(low = "#d6979d", high = "#a61b29",
                       name = "Final Density of T. confusum") + 
  facet_grid(Num_Gen ~ Total, 
             labeller = labeller(Total = c(`30` = "Total 30 Eggs", `60` = "Total 60 Eggs"),
                                 Num_Gen = c(`1` = "1 Generation", `2` = "2 Generations", `3` = "3 Generations"))) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 15),
        axis.title.y = element_blank(), axis.title.x = element_text(size = 20),
        axis.ticks = element_blank(), plot.title = element_text(size = 17), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), legend.position = "bottom",
        strip.text = element_text(size = 15))

p_final_T
p_final_F

p_final_pop = p_final_T | p_final_F
p_final_pop

# Scatterplot (Figure S3-S5)

p_pop_onegen = 
  counts_final_twospp %>% 
  pivot_longer(starts_with("Adults"), names_to = "Species", values_to = "Count") %>% 
  unite(Init_Count, c(Start_T, Start_F), sep = "-", remove = F) %>% 
  filter(Num_Gen == 1) %>% 
  ggplot(aes(x = Relative_Arriv_Time, y = Count, color = Species, fill = Species)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  facet_wrap(~ Init_Count, ncol = 3) + 
  scale_x_continuous(breaks = unique(counts_final$Relative_Arriv_Time)) + 
  scale_color_manual(values = color_scheme, name = "Species",
                     label = c("T. confusum", "T. castaneum"),
                     guide = guide_legend(reverse = T)) +
  scale_fill_manual(values = color_scheme, guide = "none") + 
  xlab("Relative Arrival Time") + 
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 15), axis.title = element_text(size = 20), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), 
        strip.text = element_text(size = 15))

p_pop_twogen = 
  counts_final_twospp %>% 
  pivot_longer(starts_with("Adults"), names_to = "Species", values_to = "Count") %>% 
  unite(Init_Count, c(Start_T, Start_F), sep = "-", remove = F) %>% 
  filter(Num_Gen == 2) %>% 
  ggplot(aes(x = Relative_Arriv_Time, y = Count, color = Species, fill = Species)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  facet_wrap(~ Init_Count, ncol = 3) + 
  scale_x_continuous(breaks = unique(counts_final$Relative_Arriv_Time)) + 
  scale_color_manual(values = color_scheme, name = "Species",
                     label = c("T. confusum", "T. castaneum"),
                     guide = guide_legend(reverse = T)) +
  scale_fill_manual(values = color_scheme, guide = "none") + 
  xlab("Relative Arrival Time") + 
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 15), axis.title = element_text(size = 20), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), 
        strip.text = element_text(size = 15))

p_pop_threegen = 
  counts_final_twospp %>% 
  pivot_longer(starts_with("Adults"), names_to = "Species", values_to = "Count") %>% 
  unite(Init_Count, c(Start_T, Start_F), sep = "-", remove = F) %>% 
  filter(Num_Gen == 3) %>% 
  ggplot(aes(x = Relative_Arriv_Time, y = Count, color = Species, fill = Species)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  facet_wrap(~ Init_Count, ncol = 3) + 
  scale_x_continuous(breaks = unique(counts_final$Relative_Arriv_Time)) + 
  scale_color_manual(values = color_scheme, name = "Species",
                     label = c("T. confusum", "T. castaneum"),
                     guide = guide_legend(reverse = T)) +
  scale_fill_manual(values = color_scheme, guide = "none") + 
  xlab("Relative Arrival Time") + 
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 15), axis.title = element_text(size = 20), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), 
        strip.text = element_text(size = 15))

p_pop_onegen
p_pop_twogen
p_pop_threegen

# Summary stats for final population (Table S2)

# T only

m_all_T = lmerTest::lmer(Adults_T ~ Num_Gen + Relative_Arriv_Time + as.factor(Ratio) + 
                           # Num_Gen:Relative_Arriv_Time + Num_Gen:as.factor(Ratio) + Relative_Arriv_Time:as.factor(Ratio) + 
                           (1|Replication),
                         data = counts_final_twospp)

lmerTest::step(m_all_T, reduce_random = F)

summary(m_all_T)
confint(m_all_T)

# F only

m_all_F = lmerTest::lmer(Adults_F ~ Num_Gen + Relative_Arriv_Time + as.factor(Ratio) + 
                           # Num_Gen:Relative_Arriv_Time + Num_Gen:as.factor(Ratio) + Relative_Arriv_Time:as.factor(Ratio) +
                           (1|Replication),
                         data = counts_final_twospp)

lmerTest::step(m_all_F, reduce_random = F)

summary(m_all_F)
confint(m_all_F)

# Summary stats for final population, linear models

m_all_T_lm = lm(Adults_T ~ Num_Gen + Relative_Arriv_Time + as.factor(Ratio) +
              Num_Gen:Relative_Arriv_Time + Num_Gen:as.factor(Ratio) + Relative_Arriv_Time:as.factor(Ratio),
            data = counts_final_twospp)

summary(m_all_T_lm)
confint(m_all_T_lm)

coeffs_T_lm = broom::tidy(m_all_T_lm) %>% 
  mutate(Species = "T") %>% 
  bind_cols(confint(m_all_T_lm)[, 1:2])

m_all_F_lm = lm(Adults_F ~ Num_Gen + Relative_Arriv_Time + as.factor(Ratio) +
              Num_Gen:Relative_Arriv_Time + Num_Gen:as.factor(Ratio) + Relative_Arriv_Time:as.factor(Ratio),
            data = counts_final_twospp)

summary(m_all_F_lm)
confint(m_all_F_lm)

coeffs_F_lm = broom::tidy(m_all_F_lm) %>% 
  mutate(Species = "F") %>% 
  bind_cols(confint(m_all_F_lm)[, 1:2])

# Visualization of the linear models

rbind(coeffs_T_lm, coeffs_F_lm) %>% 
  mutate(signif = if_else(p.value < 0.05, T, F)) %>% 
  ggplot(aes(y = term, x = estimate, color = Species, alpha = signif)) + 
  geom_pointrange(aes(xmin = `2.5 %`, xmax = `97.5 %`)) + 
  geom_vline(xintercept = 0) + 
  scale_color_manual(values = color_scheme) +
  scale_alpha_manual(values = c(0.2, 1), name = "Significance") + 
  ggh4x::facet_wrap2(.~ Species) + 
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 15), axis.title = element_text(size = 20), 
        legend.text = element_text(size = 17), legend.title = element_text(size = 20), 
        strip.text = element_text(size = 15))

# Summary stats for final population, linear model, separately for generation lengths

m_onegen_T_lm = lm(Adults_T ~ Relative_Arriv_Time + as.factor(Ratio), 
                data = counts_final_twospp %>% filter(Num_Gen == 1))

summary(m_onegen_T_lm)
confint(m_onegen_T_lm)

m_twogen_T_lm = lm(Adults_T ~ Relative_Arriv_Time + as.factor(Ratio), 
                   data = counts_final_twospp %>% filter(Num_Gen == 2))

summary(m_twogen_T_lm)
confint(m_twogen_T_lm)

m_threegen_T_lm = lm(Adults_T ~ Relative_Arriv_Time + as.factor(Ratio), 
                   data = counts_final_twospp %>% filter(Num_Gen == 3))

summary(m_threegen_T_lm)
confint(m_threegen_T_lm)

m_onegen_F_lm = lm(Adults_F ~ Relative_Arriv_Time + as.factor(Ratio), 
                   data = counts_final_twospp %>% filter(Num_Gen == 1))

summary(m_onegen_F_lm)
confint(m_onegen_F_lm)

m_twogen_F_lm = lm(Adults_F ~ Relative_Arriv_Time + as.factor(Ratio), 
                   data = counts_final_twospp %>% filter(Num_Gen == 2))

summary(m_twogen_F_lm)
confint(m_twogen_F_lm)

m_threegen_F_lm = lm(Adults_F ~ Relative_Arriv_Time + as.factor(Ratio), 
                     data = counts_final_twospp %>% filter(Num_Gen == 3))

summary(m_threegen_F_lm)
confint(m_threegen_F_lm)

# Summary stats for final population, ANOVA

m_all_T_aov = aov(Adults_T ~ Num_Gen + Relative_Arriv_Time + as.factor(Ratio) +
                Num_Gen:Relative_Arriv_Time + Num_Gen:as.factor(Ratio) + Relative_Arriv_Time:as.factor(Ratio),
                data = counts_final_twospp)

summary(m_all_T_aov)

m_all_F_aov = aov(Adults_F ~ Num_Gen + Relative_Arriv_Time + as.factor(Ratio) +
                Num_Gen:Relative_Arriv_Time + Num_Gen:as.factor(Ratio) + Relative_Arriv_Time:as.factor(Ratio),
                data = counts_final_twospp)

summary(m_all_F_aov)


########## Additional Assays ##########


# Egg predation (Figure S7)

eggpre = read_excel("AdditionalAssays.xlsx", sheet = "Egg Predation New") %>% 
  separate(Vial_Name, c("Spp_Time", "Dens", "Rep"), sep = "-") %>% 
  mutate(Spp = substring(Spp_Time, 1, 1), Age = as.numeric(substring(Spp_Time, 2, nchar(Spp_Time))))

p_eggpre = eggpre %>% 
  mutate(Species = substring(Spp_Time, 1, 1), 
         Larvae_Age = as.numeric(substring(Spp_Time, 2))) %>% 
  ggplot(aes(x = as.factor(Larvae_Age), y = Day_1/30*100, color = Species)) + 
  geom_boxplot() + 
  xlab("Age of Larvae (Days)") + ylab("% Eggs Survived") + 
  scale_color_manual(name = "Species of Larva", values = c("#a61b29","#0eb0c9"), 
                     labels = c("T. confusum", "T. castaneum")) + 
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 17), legend.title = element_text(size = 20))

p_eggpre

# Fecundity (Table S4)

fecundity = read_excel("AdditionalAssays.xlsx", sheet = "Fecundity") %>% 
  filter(!is.na(Day_1))

# Mean and SE of T

mean(as.numeric(fecundity[1, 3:7]))
sd(as.numeric(fecundity[1, 3:7]))/sqrt(5)

# Mean and SE of F

mean(as.numeric(fecundity[2, 3:7]))
sd(as.numeric(fecundity[2, 3:7]))/sqrt(5)

# t-test

t.test(as.numeric(fecundity[1, 3:7]), as.numeric(fecundity[2, 3:7]), 
       alternative = "less")

