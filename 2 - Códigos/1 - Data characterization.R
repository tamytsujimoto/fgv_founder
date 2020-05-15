require(tidyverse)
require(ggplot2)
library(viridis)

ibrx <- 
  read.csv("../1 - Bases de dados/ibrx_db_12052020.csv", stringsAsFactors = FALSE) %>% 
  mutate(ceo_chair = ifelse(ceo + chairman > 0, 1, 0)) 

########################################################################################################
# TABLES
########################################################################################################

# Frequency
summarytools::freq(ibrx)

# Number of companies
ibrx %>% 
  group_by(empresa) %>% 
  summarise(n=n())

##############################
# FREQUENCY TYPE OF PORTFOLIO
##############################

ceo_type <-
  ibrx %>% 
  group_by(empresa, ceo_chair) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  group_by(empresa) %>% 
  summarise(n = n(), ceo = sum(ceo_chair)) %>% 
  mutate(ceo_type = ifelse(n > 1, 2, ceo)) %>% 
  select(empresa, ceo_type)

summarytools::freq(ceo_type$ceo_type)

#####################
# FREQUENCY INDUSTRY
#####################

ibrx %>% 
  group_by(empresa, setor) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  select(setor) %>% 
  summarytools::freq()

############################
# SUMMARY TYPE PF PORTFOLIO
############################

tab_ceo = 
  ibrx %>% 
  group_by(ceo_chair, ano) %>% 
  summarise(n=n(), mean = mean(retorno_anual, na.rm = TRUE), sd = sd(retorno_anual, na.rm = TRUE)) %>% 
  pivot_longer(-c(ceo_chair, ano), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = ano, values_from = value)

tab_tot = 
  ibrx %>% 
  group_by(ano) %>% 
  summarise(n=n(), 
            mean = round(mean(retorno_anual, na.rm = TRUE),2), 
            sd = round(sd(retorno_anual, na.rm = TRUE),2)) %>% 
  mutate(ceo_chair = 2) %>% 
  pivot_longer(-c(ceo_chair, ano), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = ano, values_from = value)

write.csv(bind_rows(tab_ceo, tab_tot), file = "Output/charac_tab.csv", row.names = FALSE)

#######################################
# SUMMARY TYPE PF PORTFOLIO x INDUSTRY
#######################################

tab_setor = 
  ibrx %>% 
  group_by(setor, ano) %>% 
  summarise(n=n(), 
            mean = round(mean(retorno_anual, na.rm = TRUE),2), 
            sd = round(sd(retorno_anual, na.rm = TRUE),2)) %>% 
  group_by(ano) %>% 
  mutate(freq = round(n/sum(n)*100,1)) %>% 
  pivot_longer(c(n, freq, mean, sd), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = ano, values_from = value)

write.csv(bind_rows(tab_setor, tab_tot), file = "Output/tab_setor.csv", row.names = FALSE)

tab_ceo_setor = 
  ibrx %>% 
  group_by(setor, ceo_chair, ano) %>% 
  summarise(n=n(), 
            mean = round(mean(retorno_anual, na.rm = TRUE),2), 
            sd = round(sd(retorno_anual, na.rm = TRUE),2)) %>% 
  group_by(ceo_chair, ano) %>% 
  mutate(freq = round(n/sum(n)*100,1)) %>% 
  pivot_longer(c(n, freq, mean, sd), names_to = "stat", values_to = "value") %>% 
  unite(ano_ceo, ano, ceo_chair) %>% 
  pivot_wider(names_from = ano_ceo, values_from = value)

write.csv(tab_ceo_setor, file = "Output/tab_ceo_setor.csv", row.names = FALSE)

########################################################################################################
# PLOTS
########################################################################################################

yr = c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")
pd <- position_dodge(.1)
ceo_type.lab <- c("Successor only", "Founder only", "Both")
names(ceo_type.lab) <- c("0", "1", "2")
ceo_chair.lab <- c("Successor", "Founder")
names(ceo_chair.lab) <- c("0", "1")

# Industry distribution

ibrx %>% 
  group_by(ano, setor) %>% 
  summarise(n = n()) %>% 
  group_by(ano) %>% 
  mutate(freq = n/sum(n)*100,
         y_pos = 102-cumsum(freq)+.7*freq) %>% 
  ggplot(aes(x=ano, y=freq, fill = setor)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(y=y_pos, label=round(freq,1)), vjust=1.6, color="white", size=3.5) +
  labs(title="", x="Year", y="%", fill="") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2009, 2019, by = 1), labels = yr)  +
  theme(legend.position='top', panel.grid.minor = element_blank())

ggsave('Output/plot_bar_setor.pdf', width = 9.5, height = 6)

ibrx %>% 
  group_by(ano, ceo_chair, setor) %>% 
  summarise(n = n()) %>% 
  group_by(ano, ceo_chair) %>% 
  mutate(freq = n/sum(n)*100,
         y_pos = 102-cumsum(freq)+.7*freq) %>% 
  ggplot(aes(x=ano, y=freq, fill = setor)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(y=y_pos, label=round(freq,1)), vjust=1.6, color="white", size=3.5) +
  #scale_fill_viridis(discrete = TRUE) +
  facet_grid(~ ceo_chair,  labeller = labeller(ceo_chair = ceo_chair.lab)) +
  labs(title="", x="Year", y="%", fill="") +
  theme_bw() +
  scale_x_continuous(breaks=seq(2009, 2019, by = 1), labels = yr) +
  theme(legend.position='top', panel.grid.minor = element_blank())

ggsave('Output/plot_bar_setor_ceo.pdf', width = 9.5, height = 6)

# PLOT: Number of observations

ibrx %>% 
  group_by(empresa) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  group_by(n) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x=n, y=freq)) +
  geom_bar(colour="black", stat="identity", position=position_dodge()) +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_bw() +
  scale_x_continuous(name ="Number of timepoints", breaks=c(1:11)) + 
  scale_y_continuous(name ="Frequency of companies")

ggsave('Output/plot_bar.pdf', width = 8, height = 4.5)

########################  
# PLOT: Spaguetii plot #
########################

# MEAN PLOTS
plot_tot <- 
  ibrx %>% 
  ggplot(aes(x = ano, y = retorno_anual, group = empresa, color = factor(ceo_chair))) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name="", labels = c("Successor", "Founder")) +
  scale_x_continuous(name ="Year", breaks=seq(2009, 2019, by = 1), labels = yr) + 
  scale_y_continuous(name ="Annual stock market return", limits = c(-1,6)) + 
  theme_bw() +
  theme(plot.margin = unit(c(.1,.05,.1,.1), "cm"))

plot_ceo <-
  ibrx %>% 
  group_by(ano,ceo_chair) %>% 
  summarise(mean = mean(retorno_anual, na.rm = T),
            sd = sd(retorno_anual, na.rm = T),
            n = sum(!is.na(retorno_anual), na.rm = T),
            ci = sd/sqrt(n)) %>% 
  ggplot(aes(x = ano, y = mean, color = factor(ceo_chair))) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), width = 1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  scale_y_continuous(name ="", limits = c(-1,6)) + 
  scale_x_continuous(name ="Year", breaks=seq(2009, 2019, by = 1), labels = yr) + 
  theme_bw() +
  scale_color_discrete(name="", labels = c("Successor", "Founder")) +
  theme(legend.position='top', panel.grid.minor = element_blank())

ggpubr::ggarrange(plot_tot, plot_ceo, ncol=2, nrow=1, 
                  common.legend = TRUE, 
                  legend="top")
ggsave('Output/plot_perfis.pdf', width = 8, height = 4.5)


# INDUSTRY
plot_tot_setor <- 
  ibrx %>% 
  ggplot(aes(x = ano, y = retorno_anual, group = empresa, color = factor(setor))) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name="") +
  scale_x_continuous(name ="Year", breaks=seq(2009, 2019, by = 1), labels = yr) + 
  scale_y_continuous(name ="Annual stock market return", limits = c(-1,6)) + 
  theme_bw() +
  theme(plot.margin = unit(c(.1,.05,.1,.1), "cm"))

plot_setor <-
  ibrx %>% 
  group_by(ano,setor) %>% 
  summarise(mean = mean(retorno_anual, na.rm = T),
            sd = sd(retorno_anual, na.rm = T),
            n = sum(!is.na(retorno_anual), na.rm = T),
            ci = sd/sqrt(n)) %>% 
  ggplot(aes(x = ano, y = mean, color = factor(setor))) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), width = 1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd) +
  scale_y_continuous(name ="", limits = c(-1,6)) + 
  scale_x_continuous(name ="Year", breaks=seq(2009, 2019, by = 1), labels = yr) + 
  theme_bw() +
  scale_color_discrete(name="") +
  theme(legend.position='top', panel.grid.minor = element_blank())

ggpubr::ggarrange(plot_tot_setor, plot_setor, ncol=2, nrow=1, 
                  common.legend = TRUE, 
                  legend="top")
ggsave('Output/plot_perfis_setor.pdf', width = 8, height = 4.5)

# ibrx %>% 
#   group_by(ano,setor,ceo_chair) %>% 
#   summarise(mean = mean(retorno_anual, na.rm = T),
#             sd = sd(retorno_anual, na.rm = T),
#             n = sum(!is.na(retorno_anual), na.rm = T),
#             ci = sd/sqrt(n)) %>% 
#   ggplot(aes(x = ano, y = mean, color = factor(setor))) +
#   geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), width = 1, position = pd) +
#   geom_line(position = pd) +
#   geom_point(position = pd) +
#   facet_grid(~ ceo_chair,  labeller = labeller(ceo_chair = ceo_chair.lab)) +
#   scale_y_continuous(name ="", limits = c(-1,6)) + 
#   scale_x_continuous(name ="Year", breaks=seq(2009, 2019, by = 1), labels = yr) + 
#   theme_bw() +
#   scale_color_discrete(name="") +
#   theme(legend.position='top', panel.grid.minor = element_blank())
