require(tidyverse)
require(ggplot2)
require(nlme)

ibrx <- 
  read.csv("../1 - Bases de dados/ibrx_db_12052020.csv", stringsAsFactors = FALSE) %>% 
  mutate(ceo_chair = ifelse(ceo + chairman > 0, 1, 0),
         ano_adj = ano - 2009) %>% 
  filter(complete.cases(.))

# MIXED EFFECTS MODEL

fit1 <- 
  ibrx %>% 
  lme(retorno_anual ~ factor(ceo_chair) + factor(setor) + ano_adj, random = ~ 1 | empresa, data=., method = "ML")
summary(fit1)

fit2 <- 
  ibrx %>% 
  lme(retorno_anual ~ factor(ceo_chair) + factor(setor) + ano_adj + I(ano_adj^2), random = ~ 1 | empresa, data=., method = "ML")
summary(fit2)

anova(fit1, fit2) #fit the models using "ML"

fit.final <- 
  ibrx %>% 
  lme(retorno_anual ~ factor(ceo_chair) + factor(setor) + ano_adj + I(ano_adj^2), random = ~ 1 | empresa, data=.)
summary(fit.final)

write.csv(summary(fit.final)$tTable, file = 'Output/fit_final.csv')

# Plotting final model
# yr = c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19")
# 
# f1 <- function(x) fit.final$coefficients$fixed[1] + fit.final$coefficients$fixed[3]*x + fit.final$coefficients$fixed[4]*x^2
# f2 <- function(x) sum(fit.final$coefficients$fixed[1:2]) + fit.final$coefficients$fixed[3]*x + fit.final$coefficients$fixed[4]*x^2
# 
# ibrx %>% 
#   mutate(ano_adj = ano - 2009) %>% 
#   ggplot(aes(x = ano_adj, y = retorno_anual, group = empresa, color = factor(ceo_chair))) +
#   geom_line() +
#   geom_point() +
#   scale_color_discrete(name="", labels = c("Successor", "Founder")) +
#   scale_x_continuous(name ="Year", breaks=seq(0, 10, by = 1), labels = yr) + 
#   scale_y_continuous(name ="Annual stock market return", limits = c(-1,6)) + 
#   theme_bw() +
#   theme(legend.position = 'top', plot.margin = unit(c(.1,.05,.1,.1), "cm")) +
#   stat_function(fun = f1, colour = "deeppink4", lwd = 1.5) +
#   stat_function(fun = f2, colour = "deepskyblue4", lwd = 1.5)
# 
# ggsave('Output/plot_model.pdf', width = 8, height = 4.5)

##########################
# PREDICTED TABLE

data.frame(
    ano_adj = rep(0:10, 20),
    setor = rep(names(table(ibrx$setor)), 2, each = 11),
    ceo_chair = c(rep(0,110), rep(1,110))
    ) %>% 
  mutate(pred = predict(fit.final, newdata = ., level = 0),
         yr = paste0('yr_', ano_adj)) %>% 
  pivot_wider(id_cols = c(setor, ceo_chair),
              names_from = yr,
              values_from = pred) %>% 
  write.csv(file = 'Output/fit_final_pred.csv', row.names = FALSE)
  
