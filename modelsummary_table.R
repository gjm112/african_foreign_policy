
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

totlib_nolag <- (
  totlib1 = as.data.frame(coef(summary(mod0_TOTLIB1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_TOTLIB1, parm = "TOTLIB1", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag0", n = nobs(mod0_TOTLIB1)) %>%
    select(var, lag, n, Estimate, pval, confidence))

totlib_lag1 <- (
  totlib1 = as.data.frame(coef(summary(mod0_TOTLIB1_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag1") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_TOTLIB1_lag1, parm = "TOTLIB1_lag1", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag1", n = nobs(mod0_TOTLIB1_lag1)) %>%
    select(var, lag, n, Estimate, pval, confidence))

totlib_lag2 <- (
  totlib1 = as.data.frame(coef(summary(mod0_TOTLIB1_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag2") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_TOTLIB1_lag2, parm = "TOTLIB1_lag2", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag2", n = nobs(mod0_TOTLIB1_lag2)) %>%
    select(var, lag, n, Estimate, pval, confidence))

totlib_lag3 <- (
  totlib1 = as.data.frame(coef(summary(mod0_TOTLIB1_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag3") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_TOTLIB1_lag3, parm = "TOTLIB1_lag3", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag3", n = nobs(mod0_TOTLIB1_lag3)) %>%
    select(var, lag, n, Estimate, pval, confidence))

POLITY2_nolag <- (
  POLITY2 = as.data.frame(coef(summary(mod0_POLITY2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "POLITY2") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_POLITY2, parm = "POLITY2", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag0", n = nobs(mod0_POLITY2)) %>%
    select(var, lag, n, Estimate, pval, confidence))

POLITY2_lag1 <- (
  POLITY2 = as.data.frame(coef(summary(mod0_POLITY2_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "POLITY2_lag1") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_POLITY2_lag1, parm = "POLITY2_lag1", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag1", n = nobs(mod0_POLITY2_lag1)) %>%
    select(var, lag, n, Estimate, pval, confidence))

POLITY2_lag2 <- (
  POLITY2 = as.data.frame(coef(summary(mod0_POLITY2_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "POLITY2_lag2") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_POLITY2_lag2, parm = "POLITY2_lag2", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag2", n = nobs(mod0_POLITY2_lag2)) %>%
    select(var, lag, n, Estimate, pval, confidence))

POLITY2_lag3 <- (
  POLITY2 = as.data.frame(coef(summary(mod0_POLITY2_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "POLITY2_lag3") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_POLITY2_lag3, parm = "POLITY2_lag3", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag3", n = nobs(mod0_POLITY2_lag3)) %>%
    select(var, lag, n, Estimate, pval, confidence))



V2x_Libdem_nolag <- (
  V2x_Libdem = as.data.frame(coef(summary(mod0_V2x_Libdem))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "V2x_Libdem") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_V2x_Libdem, parm = "V2x_Libdem", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag0", n = nobs(mod0_V2x_Libdem)) %>%
    select(var, lag, n, Estimate, pval, confidence))

V2x_Libdem_lag1 <- (
  V2x_Libdem = as.data.frame(coef(summary(mod0_V2x_Libdem_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "V2x_Libdem_lag1") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_V2x_Libdem_lag1, parm = "V2x_Libdem_lag1", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag1", n = nobs(mod0_V2x_Libdem_lag1)) %>%
    select(var, lag, n, Estimate, pval, confidence))

V2x_Libdem_lag2 <- (
  V2x_Libdem = as.data.frame(coef(summary(mod0_V2x_Libdem_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "V2x_Libdem_lag2") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_V2x_Libdem_lag2, parm = "V2x_Libdem_lag2", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag2", n = nobs(mod0_V2x_Libdem_lag2)) %>%
    select(var, lag, n, Estimate, pval, confidence))

V2x_Libdem_lag3 <- (
  V2x_Libdem = as.data.frame(coef(summary(mod0_V2x_Libdem_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "V2x_Libdem_lag3") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod0_V2x_Libdem_lag3, parm = "V2x_Libdem_lag3", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag3", n = nobs(mod0_V2x_Libdem_lag3)) %>%
    select(var, lag, n, Estimate, pval, confidence))




table_data = list(
  totlib_nolag, totlib_lag1, totlib_lag2, totlib_lag3, POLITY2_nolag, POLITY2_lag1, POLITY2_lag2, POLITY2_lag3, V2x_Libdem_nolag, V2x_Libdem_lag1, V2x_Libdem_lag2, V2x_Libdem_lag3
) %>%
  rbindlist() %>%
  mutate(var = case_when(str_detect(var, "TOTLIB1") ~ "TOTLIB1",
                         str_detect(var, "POLITY2") ~ "POLITY2",
                         str_detect(var, "V2x_Libdem") ~ "V2x_Libdem"))

table_data %>%
  gt(rowname_col = "var", groupname_col = "lag") %>%
  tab_options(row_group.as_column = T) %>%
  cols_label(
    n = md("$n$"),
    Estimate = md("$\\hat{\\beta}$"),
    pval = md("$p$"),
    confidence = md("$\\hat{\\beta}: 95\\% CI$")
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = md("Table 1: Reduced Model Parameters for **Democracy Variables** and Their **Lags**")
  )

totlib_nolag <- (
  totlib1 = as.data.frame(coef(summary(mod1_TOTLIB1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_TOTLIB1, parm = "TOTLIB1", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag0", n = nobs(mod1_TOTLIB1)) %>%
    select(var, lag, n, Estimate, pval, confidence))

totlib_lag1 <- (
  totlib1 = as.data.frame(coef(summary(mod1_TOTLIB1_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag1") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_TOTLIB1_lag1, parm = "TOTLIB1_lag1", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag1", n = nobs(mod1_TOTLIB1_lag1)) %>%
    select(var, lag, n, Estimate, pval, confidence))

totlib_lag2 <- (
  totlib1 = as.data.frame(coef(summary(mod1_TOTLIB1_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag2") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_TOTLIB1_lag2, parm = "TOTLIB1_lag2", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag2", n = nobs(mod1_TOTLIB1_lag2)) %>%
    select(var, lag, n, Estimate, pval, confidence))

totlib_lag3 <- (
  totlib1 = as.data.frame(coef(summary(mod1_TOTLIB1_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "TOTLIB1_lag3") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_TOTLIB1_lag3, parm = "TOTLIB1_lag3", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag3", n = nobs(mod1_TOTLIB1_lag3)) %>%
    select(var, lag, n, Estimate, pval, confidence))




POLITY2_nolag <- (
  POLITY2 = as.data.frame(coef(summary(mod1_POLITY2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "POLITY2") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_POLITY2, parm = "POLITY2", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag0", n = nobs(mod1_POLITY2)) %>%
    select(var, lag, n, Estimate, pval, confidence))

POLITY2_lag1 <- (
  POLITY2 = as.data.frame(coef(summary(mod1_POLITY2_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "POLITY2_lag1") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_POLITY2_lag1, parm = "POLITY2_lag1", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag1", n = nobs(mod1_POLITY2_lag1)) %>%
    select(var, lag, n, Estimate, pval, confidence))

POLITY2_lag2 <- (
  POLITY2 = as.data.frame(coef(summary(mod1_POLITY2_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "POLITY2_lag2") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_POLITY2_lag2, parm = "POLITY2_lag2", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag2", n = nobs(mod1_POLITY2_lag2)) %>%
    select(var, lag, n, Estimate, pval, confidence))

POLITY2_lag3 <- (
  POLITY2 = as.data.frame(coef(summary(mod1_POLITY2_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "POLITY2_lag3") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_POLITY2_lag3, parm = "POLITY2_lag3", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag3", n = nobs(mod1_POLITY2_lag3)) %>%
    select(var, lag, n, Estimate, pval, confidence))



V2x_Libdem_nolag <- (
  V2x_Libdem = as.data.frame(coef(summary(mod1_V2x_Libdem))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "V2x_Libdem") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_V2x_Libdem, parm = "V2x_Libdem", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag0", n = nobs(mod1_V2x_Libdem)) %>%
    select(var, lag, n, Estimate, pval, confidence))

V2x_Libdem_lag1 <- (
  V2x_Libdem = as.data.frame(coef(summary(mod1_V2x_Libdem_lag1))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "V2x_Libdem_lag1") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_V2x_Libdem_lag1, parm = "V2x_Libdem_lag1", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag1", n = nobs(mod1_V2x_Libdem_lag1)) %>%
    select(var, lag, n, Estimate, pval, confidence))

V2x_Libdem_lag2 <- (
  V2x_Libdem = as.data.frame(coef(summary(mod1_V2x_Libdem_lag2))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "V2x_Libdem_lag2") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_V2x_Libdem_lag2, parm = "V2x_Libdem_lag2", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag2", n = nobs(mod1_V2x_Libdem_lag2)) %>%
    select(var, lag, n, Estimate, pval, confidence))

V2x_Libdem_lag3 <- (
  V2x_Libdem = as.data.frame(coef(summary(mod1_V2x_Libdem_lag3))) %>%
    rownames_to_column(var = "var") %>%
    filter(var == "V2x_Libdem_lag3") %>%
    mutate(Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|t|)`,digits = 3, eps = 1e-5), conf = confint(mod1_V2x_Libdem_lag3, parm = "V2x_Libdem_lag3", level = 0.95, method = "Wald")) %>%
    mutate(conf_1 = round(conf[1], 3), conf_2 = round(conf[2], 3), confidence = paste0(conf_1, ", ", conf_2)) %>% 
    mutate(lag = "lag3", n = nobs(mod1_V2x_Libdem_lag3)) %>%
    select(var, lag, n, Estimate, pval, confidence))




table_data = list(
  totlib_nolag, totlib_lag1, totlib_lag2, totlib_lag3, POLITY2_nolag, POLITY2_lag1, POLITY2_lag2, POLITY2_lag3, V2x_Libdem_nolag, V2x_Libdem_lag1, V2x_Libdem_lag2, V2x_Libdem_lag3
) %>%
  rbindlist() %>%
  mutate(var = case_when(str_detect(var, "TOTLIB1") ~ "TOTLIB1",
                         str_detect(var, "POLITY2") ~ "POLITY2",
                         str_detect(var, "V2x_Libdem") ~ "V2x_Libdem"))

table_data %>%
  gt(rowname_col = "var", groupname_col = "lag") %>%
  tab_options(row_group.as_column = T) %>%
  cols_label(
    n = md("$n$"),
    Estimate = md("$\\hat{\\beta}$"),
    pval = md("$p$"),
    confidence = md("$\\hat{\\beta}: 95\\% CI$")
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = md("Table 2: Full Model Parameters for **Democracy Variables** and Their **Lags**")
  )