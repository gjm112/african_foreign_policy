
library(modelsummary)
library(tinytable)

mod0_summary_V2X_LIBDEM <- modelsummary(
  models = list(
    "Unlagged" = mod0_V2x_Libdem,
    "One-Year Lag" = mod0_V2x_Libdem_lag1,
    "Two-Year Lag" = mod0_V2x_Libdem_lag2,
    "Three-Year Lag" = mod0_V2x_Libdem_lag3
  ),
  stars = T,
  gof_omit = ".",
  coef_map = c(
    "(Intercept)", "V2x_Libdem", "V2x_Libdem_lag1", "V2x_Libdem_lag2", "V2x_Libdem_lag3", "SD (Intercept YEAR)", "SD (Intercept CCODE)", 
    "SD (Observations)"
  ),
  title = "Reduced Model Summary for V2X_LIBDEM"
) %>%
  style_tt(
    i = "caption",
    bold = TRUE,
    align = "c",
    fontsize = 1.5,
    bootstrap_class = "table caption-top"
  )

mod1_summary_V2X_LIBDEM <- modelsummary(
  models = list(
    "Unlagged" = mod1_V2x_Libdem,
    "One-Year Lag" = mod1_V2x_Libdem_lag1,
    "Two-Year Lag" = mod1_V2x_Libdem_lag2,
    "Three-Year Lag" = mod1_V2x_Libdem_lag3
  ),
  stars = T,
  gof_omit = ".",
  coef_map = c(
    "(Intercept)", "V2x_Libdem", "V2x_Libdem_lag1", "V2x_Libdem_lag2", "V2x_Libdem_lag3", "logPOP", "I(logPOP^2)", "I(logPOP^3)",
    "logGNI", "I(logGNI^2)", "logODAGtot", "I(logODAGtot^2)", "SD (Intercept YEAR)", "SD (Intercept CCODE)", "SD (Observations)"
  ),
  title = "Full Model Summary for V2X_LIBDEM"
) %>%
  style_tt(
    i = "caption",
    bold = TRUE,
    align = "c",
    fontsize = 1.5,
    bootstrap_class = "table caption-top"
  )

mod0_summary_POLITY <- modelsummary(
  models = list(
    "Unlagged" = mod0_POLITY,
    "One-Year Lag" = mod0_POLITY_lag1,
    "Two-Year Lag" = mod0_POLITY_lag2,
    "Three-Year Lag" = mod0_POLITY_lag3
  ),
  stars = T,
  gof_omit = ".",
  coef_map = c(
    "(Intercept)", "POLITY", "POLITY_lag1", "POLITY_lag2", "POLITY_lag3", "SD (Intercept YEAR)", "SD (Intercept CCODE)", "SD (Observations)"
  ),
  title = "Reduced Model Summary for POLITY"
) %>%
  style_tt(
    i = "caption",
    bold = TRUE,
    align = "c",
    fontsize = 1.5,
    bootstrap_class = "table caption-top"
  )

mod1_summary_POLITY <- modelsummary(
  models = list(
    "Unlagged" = mod1_POLITY,
    "One-Year Lag" = mod1_POLITY_lag1,
    "Two-Year Lag" = mod1_POLITY_lag2,
    "Three-Year Lag" = mod1_POLITY_lag3
  ),
  stars = T,
  gof_omit = ".",
  coef_map = c(
    "(Intercept)", "POLITY", "POLITY_lag1", "POLITY_lag2", "POLITY_lag3", "logPOP", "I(logPOP^2)", "I(logPOP^3)",
    "logGNI", "I(logGNI^2)", "logODAGtot", "I(logODAGtot^2)", "SD (Intercept YEAR)", "SD (Intercept CCODE)", "SD (Observations)"
  ),
  title = "Full Model Summary for POLITY"
) %>%
  style_tt(
    i = "caption",
    bold = TRUE,
    align = "c",
    fontsize = 1.5,
    bootstrap_class = "table caption-top"
  )

mod0_summary_POLITY2 <- modelsummary(
  models = list(
    "Unlagged" = mod0_POLITY2,
    "One-Year Lag" = mod0_POLITY2_lag1,
    "Two-Year Lag" = mod0_POLITY2_lag2,
    "Three-Year Lag" = mod0_POLITY2_lag3
  ),
  stars = T,
  gof_omit = ".",
  coef_map = c(
    "(Intercept)", "POLITY2", "POLITY2_lag1", "POLITY2_lag2", "POLITY2_lag3", "SD (Intercept YEAR)", "SD (Intercept CCODE)", "SD (Observations)"
  ),
  title = "Reduced Model Summary for POLITY2"
) %>%
  style_tt(
    i = "caption",
    bold = TRUE,
    align = "c",
    fontsize = 1.5,
    bootstrap_class = "table caption-top"
  )

mod1_summary_POLITY2 <- modelsummary(
  models = list(
    "Unlagged" = mod1_POLITY2,
    "One-Year Lag" = mod1_POLITY2_lag1,
    "Two-Year Lag" = mod1_POLITY2_lag2,
    "Three-Year Lag" = mod1_POLITY2_lag3
  ),
  stars = T,
  gof_omit = ".",
  coef_map = c(
    "(Intercept)", "POLITY2", "POLITY2_lag1", "POLITY2_lag2", "POLITY2_lag3", "logPOP", "I(logPOP^2)", "I(logPOP^3)",
    "logGNI", "I(logGNI^2)", "logODAGtot", "I(logODAGtot^2)", "SD (Intercept YEAR)", "SD (Intercept CCODE)", "SD (Observations)"
  ),
  title = "Full Model Summary for POLITY2"
) %>%
  style_tt(
    i = "caption",
    bold = TRUE,
    align = "c",
    fontsize = 1.5,
    bootstrap_class = "table caption-top"
  )

mod0_summary_TOTLIB1 <- modelsummary(
  models = list(
    "Unlagged" = mod0_TOTLIB1,
    "One-Year Lag" = mod0_TOTLIB1_lag1,
    "Two-Year Lag" = mod0_TOTLIB1_lag2,
    "Three-Year Lag" = mod0_TOTLIB1_lag3
  ),
  stars = T,
  gof_omit = ".",
  coef_map = c(
    "(Intercept)", "TOTLIB1", "TOTLIB1_lag1", "TOTLIB1_lag2", "TOTLIB1_lag3", "SD (Intercept YEAR)", "SD (Intercept CCODE)", "SD (Observations)"
  ),
  title = "Full Model Summary for TOTLIB1"
) %>%
  style_tt(
    i = "caption",
    bold = TRUE,
    align = "c",
    fontsize = 1.5,
    bootstrap_class = "table caption-top"
  )

mod1_summary_TOTLIB1 <- modelsummary(
  models = list(
    "Unlagged" = mod1_TOTLIB1,
    "One-Year Lag" = mod1_TOTLIB1_lag1,
    "Two-Year Lag" = mod1_TOTLIB1_lag2,
    "Three-Year Lag" = mod1_TOTLIB1_lag3
  ),
  stars = T,
  gof_omit = ".",
  coef_map = c(
    "(Intercept)", "TOTLIB1", "TOTLIB1_lag1", "TOTLIB1_lag2", "TOTLIB1_lag3", "logPOP", "I(logPOP^2)", "I(logPOP^3)",
    "logGNI", "I(logGNI^2)", "logODAGtot", "I(logODAGtot^2)", "SD (Intercept YEAR)", "SD (Intercept CCODE)", "SD (Observations)"
  ),
  title = "Full Model Summary for TOTLIB1"
) %>%
  style_tt(
    i = "caption",
    bold = TRUE,
    align = "c",
    fontsize = 1.5,
    bootstrap_class = "table caption-top"
  )

## ---- GT TABLE CODE ----

library(lme4)
library(data.table)
library(gt)

mod_TOTLIB1 <- glmer(N_EMBASSY ~ TOTLIB1 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_TOTLIB1_lag1 <- glmer(N_EMBASSY ~ TOTLIB1_lag1 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_TOTLIB1_lag2 <- glmer(N_EMBASSY ~ TOTLIB1_lag2 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_TOTLIB1_lag3 <- glmer(N_EMBASSY ~ TOTLIB1_lag3 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

totlib_nolag <- list(
  totlib1 = as.data.frame(coef(summary(mod_TOTLIB1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_TOTLIB1, parm = "TOTLIB1", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag0") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

totlib_lag1 <- list(
  totlib1 = as.data.frame(coef(summary(mod_TOTLIB1_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag1") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_TOTLIB1_lag1, parm = "TOTLIB1_lag1", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag1") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

totlib_lag2 <- list(
  totlib1 = as.data.frame(coef(summary(mod_TOTLIB1_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag2") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_TOTLIB1_lag2, parm = "TOTLIB1_lag2", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag2") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

totlib_lag3 <- list(
  totlib1 = as.data.frame(coef(summary(mod_TOTLIB1_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag3") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_TOTLIB1_lag3, parm = "TOTLIB1_lag3", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag3") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

mod_DEMOC <- glmer(N_EMBASSY ~ DEMOC + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_DEMOC_lag1 <- glmer(N_EMBASSY ~ DEMOC_lag1 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_DEMOC_lag2 <- glmer(N_EMBASSY ~ DEMOC_lag2 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_DEMOC_lag3 <- glmer(N_EMBASSY ~ DEMOC_lag3 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

DEMOC_nolag <- list(
  totlib1 = as.data.frame(coef(summary(mod_DEMOC))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "DEMOC") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_DEMOC, parm = "DEMOC", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag0") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

DEMOC_lag1 <- list(
  totlib1 = as.data.frame(coef(summary(mod_DEMOC_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "DEMOC_lag1") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_DEMOC_lag1, parm = "DEMOC_lag1", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag1") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

DEMOC_lag2 <- list(
  totlib1 = as.data.frame(coef(summary(mod_DEMOC_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "DEMOC_lag2") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_DEMOC_lag2, parm = "DEMOC_lag2", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag2") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

DEMOC_lag3 <- list(
  totlib1 = as.data.frame(coef(summary(mod_DEMOC_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "DEMOC_lag3") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_DEMOC_lag3, parm = "DEMOC_lag3", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag3") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

mod_v2x_LIBDEM <- glmer(N_EMBASSY ~ v2x_LIBDEM + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_v2x_LIBDEM_lag1 <- glmer(N_EMBASSY ~ v2x_LIBDEM_lag1 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_v2x_LIBDEM_lag2 <- glmer(N_EMBASSY ~ v2x_LIBDEM_lag2 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod_v2x_LIBDEM_lag3 <- glmer(N_EMBASSY ~ v2x_LIBDEM_lag3 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

v2x_LIBDEM_nolag <- list(
  totlib1 = as.data.frame(coef(summary(mod_v2x_LIBDEM))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "v2x_LIBDEM") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_v2x_LIBDEM, parm = "v2x_LIBDEM", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag0") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

v2x_LIBDEM_lag1 <- list(
  totlib1 = as.data.frame(coef(summary(mod_v2x_LIBDEM_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "v2x_LIBDEM_lag1") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_v2x_LIBDEM_lag1, parm = "v2x_LIBDEM_lag1", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag1") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

v2x_LIBDEM_lag2 <- list(
  totlib1 = as.data.frame(coef(summary(mod_v2x_LIBDEM_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "v2x_LIBDEM_lag2") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_v2x_LIBDEM_lag2, parm = "v2x_LIBDEM_lag2", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag2") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

v2x_LIBDEM_lag3 <- list(
  totlib1 = as.data.frame(coef(summary(mod_v2x_LIBDEM_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "v2x_LIBDEM_lag3") %>%
    mutate(Estimate = round(Estimate, 3), Estimate_exp = exp(Estimate), pval = round(`Pr(>|z|)`, 3)),
  conf = data.frame(conf = confint(mod_v2x_LIBDEM_lag3, parm = "v2x_LIBDEM_lag3", level = 0.95)) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(conf.2.5.., 3), conf_2 = round(conf.97.5.., 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>% reduce(merge, by = "var") %>%
  mutate(lag = "lag3") %>%
  select(var, lag, Estimate, Estimate_exp, pval, confidence, confidence_exp)

table_data = list(
  totlib_nolag, totlib_lag1, totlib_lag2, totlib_lag3, DEMOC_nolag, DEMOC_lag1, DEMOC_lag2, DEMOC_lag3,
  v2x_LIBDEM_nolag, v2x_LIBDEM_lag1, v2x_LIBDEM_lag2, v2x_LIBDEM_lag3
) %>%
  rbindlist() %>%
  mutate(var = case_when(str_detect(var, "TOTLIB1") ~ "TOTLIB1",
                         str_detect(var, "DEMOC") ~ "DEMOC",
                         str_detect(var, "LIBDEM") ~ "LIBDEM"))

table_data %>%
  gt(rowname_col = "var", groupname_col = "lag") %>%
  tab_options(row_group.as_column = T) %>%
  cols_label(
    Estimate = md("$\\beta$"),
    Estimate_exp = md("$e^{\\beta}$"),
    pval = md("$p$"),
    confidence = md("$\\beta: 95\\% CI$"),
    confidence_exp = md("$e^{\\beta}: 95\\% CI$")
  ) %>%
  tab_header(
    title = md("Reduced Model Parameters for **Democracy Dcores** and Their **Lags**")
  ) %>%
  gtsave(filename = "table.png")
