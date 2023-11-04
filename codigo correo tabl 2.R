library(haven)
library(sandwich)
library(lmtest)
library(plm)
library(texreg)

wide <- read_dta("ms_blel_jpal_wide.dta ")

#Tabla 2 ------------
wide <- read_dta("ms_blel_jpal_wide.dta ")

#Modelo matemática sin FE (en el paper parece como con FE)
modelomat <- lm(m_theta_mle2 ~ treat + m_theta_mle1, data = wide)
robust_semat <- coeftest(modelomat, vcov = vcovHC(modelomat, type = "HC1"))

robust_semat
summary(modelomat)$r.squared

#Modelo hindi sin FE (en el paper parece como con FE)
modelohindi <- lm(h_theta_mle2 ~ treat + h_theta_mle1, data = wide)
robust_sehindi <- coeftest(modelohindi, vcov = vcovHC(modelohindi, type = "HC1"))

robust_sehindi
summary(modelohindi)$r.squared


# Regresiones con efectos fijos
#matemática
modelo_m_fe <- plm(m_theta_mle2 ~ treat + m_theta_mle1, data = wide, index = "strata", model = "within")
robust_se_m_fe <- coeftest(modelo_m_fe, vcov = vcovHC(modelo_m_fe, type = "sss"))
screenreg(modelo_m_fe)
robust_se_m_fe

#hindi
modelo_h_fe <- plm(h_theta_mle2 ~ treat + h_theta_mle1, data = wide, index = "strata", model = "within")
robust_se_h_fe <- coeftest(modelo_h_fe, vcov = vcovHC(modelo_h_fe, type = "sss"))
screenreg(modelo_h_fe)
robust_se_h_fe

#Tabla 4 ------------------------------
#Female 
modelo_m_female <- plm(m_theta_mle2 ~ treat + st_female1 + I(treat * st_female1)  + m_theta_mle1, data = wide, index = "strata", model = "within")
robust_m_female  <- coeftest(modelo_m_female, vcov = vcovHC(modelo_m_female, type = "sss"))
screenreg(modelo_m_female)
robust_m_female

modelo_h_female <- plm(h_theta_mle2 ~ treat + st_female1 + I(treat * st_female1)  + h_theta_mle1, data = wide, index = "strata", model = "within")
robust_h_female  <- coeftest(modelo_h_female, vcov = vcovHC(modelo_h_female, type = "sss"))
screenreg(modelo_h_female)
robust_h_female

#ses 
modelo_m_ses <- plm(m_theta_mle2 ~ treat + ses_index + I(treat * ses_index)  + m_theta_mle1, data = wide, index = "strata", model = "within")
robust_m_ses  <- coeftest(modelo_m_ses, vcov = vcovHC(modelo_m_ses, type = "sss"))
screenreg(modelo_m_ses)
robust_m_ses

modelo_h_ses <- plm(h_theta_mle2 ~ treat + ses_index + I(treat * ses_index)  + h_theta_mle1, data = wide, index = "strata", model = "within")
robust_h_ses  <- coeftest(modelo_h_ses, vcov = vcovHC(modelo_h_ses, type = "sss"))
screenreg(modelo_h_ses)
robust_h_ses


#baseline 
modelo_m_base <- plm(m_theta_mle2 ~ treat + m_theta_mle1+ I(treat * m_theta_mle1)  , data = wide, index = "strata", model = "within")
robust_m_base  <- coeftest(modelo_m_base, vcov = vcovHC(modelo_m_base, type = "sss"))
screenreg(modelo_m_base)
robust_m_base

modelo_h_base <- plm(h_theta_mle2 ~ treat + h_theta_mle1+  I(treat * h_theta_mle1) , data = wide, index = "strata", model = "within")
robust_h_base  <- coeftest(modelo_h_base, vcov = vcovHC(modelo_h_base, type = "sss"))
screenreg(modelo_h_base)
robust_h_base





