require(tidyverse)
require(ggplot2)

ibrx <- 
  read.csv("../1 - Bases de dados/ibrx_db.csv", stringsAsFactors = FALSE) %>% 
  filter(ibrx == 1,
        !(empresa %in% c('BBDC3',
                       'CMIG3',
                       'ELET3',
                       'ELPL3',
                       'GGBR3',
                       'ITUB3',
                       'LAME3',
                       'OIBR3',
                       'PETR3',
                       'SUZB3',
                       'TNPL3',
                       'USIM3',
                       'VALE5'))) %>% 
  mutate(empresa = recode(empresa, 
                          '0927042D' = 'CCXC3',
                          'GOAU4'    = 'GGBR3',
                          'RUMO3'    = 'RAIL3',
                          'GETI4'    = 'TIET11'),
         ceo_chair = ifelse(ceo + chairman > 0, 1, 0)) 

write.csv(ibrx, 'Output/base_dados_08052020.csv', row.names = FALSE) #sem o filtro ibrx == 1

summarytools::freq(ibrx)

# Number of companies
ibrx %>% 
  group_by(empresa) %>% 
  summarise(n=n())

# Number of companies by type of ceo,chairman

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

# Summary table

tab_ceo = 
  ibrx %>% 
  group_by(ceo_chair, ano) %>% 
  summarise(n=n(), mean = mean(retorno_anual, na.rm = TRUE), sd = sd(retorno_anual, na.rm = TRUE)) %>% 
  pivot_longer(-c(ceo_chair, ano), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = ano, values_from = value)

tab_tot = 
  ibrx %>% 
  group_by(ano) %>% 
  summarise(n=n(), mean = mean(retorno_anual, na.rm = TRUE), sd = sd(retorno_anual, na.rm = TRUE)) %>% 
  mutate(ceo_chair = 2) %>% 
  pivot_longer(-c(ceo_chair, ano), names_to = "stat", values_to = "value") %>% 
  pivot_wider(names_from = ano, values_from = value)

write.csv(bind_rows(tab_ceo, tab_tot), file = "Output/charac_tab.csv", row.names = FALSE)

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

ibrx %>% 
  group_by(empresa) %>% 
  summarise(n = n()) %>% 
  filter(n > 11)

ggsave('Output/plot_bar.pdf', width = 8, height = 4.5)

########################  
# PLOT: Spaguetii plot #
########################

yr = c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")
pd <- position_dodge(.1)

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

# BY TYPE OF CEO

ceo_type.lab <- c("Successor only", "Founder only", "Both")
names(ceo_type.lab) <- c("0", "1", "2")

ibrx %>% 
  left_join(ceo_type, by = 'empresa') %>% 
  ggplot(aes(x = ano, y = retorno_anual, group = empresa, color = factor(ceo_chair))) +
  geom_line() +
  geom_point() +
  facet_grid(. ~ ceo_type, labeller = labeller(ceo_type = ceo_type.lab)) +
  scale_color_discrete(name="", labels = c("Successor", "Founder")) +
  scale_x_continuous(name ="Year", breaks=seq(2009, 2019, by = 1), labels = yr) + 
  scale_y_continuous(name ="Annual stock market return", limits = c(-1,6)) + 
  theme_bw() +
  theme(legend.position = 'top', plot.margin = unit(c(.1,.05,.1,.1), "cm"))

ggsave('Output/plot_perfis_ceotype.pdf', width = 8, height = 4.5)

