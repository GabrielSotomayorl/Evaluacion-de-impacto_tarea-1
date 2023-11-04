library(haven)
library(tidyverse)
library(sandwich)
library(lmtest)
library(plm)
library(texreg)
library(knitr)
library(kableExtra)
library(patchwork)

long <- read_dta("ms_blel_jpal_long.dta")

long <- long %>% 
  mutate(
    grade_4 = as.numeric(st_grade1 == 4),
    grade_5 = as.numeric(st_grade1 == 5),
    grade_6 = as.numeric(st_grade1 == 6),
    grade_7 = as.numeric(st_grade1 == 7),
    grade_8 = as.numeric(st_grade1 == 8),
    grade_9 = as.numeric(st_grade1 == 9)
  ) 

tab1_1a = long %>%  
  filter(round==1) %>% group_by(-treat)  %>%
  summarise(
    female = mean(st_female1, na.rm = TRUE),
    age = mean(st_age1, na.rm = TRUE),
    ses = mean(ses_index, na.rm = TRUE),
    grade_4 = mean(grade_4, na.rm = TRUE),
    grade_5 = mean(grade_5, na.rm = TRUE),
    grade_6 = mean(grade_6, na.rm = TRUE),
    grade_7 = mean(grade_7, na.rm = TRUE),
    grade_8 = mean(grade_8, na.rm = TRUE),
    grade_9 = mean(grade_9, na.rm = TRUE),
    math = mean(m_theta_mle, na.rm = TRUE),
    hindi = mean(h_theta_mle, na.rm = TRUE), 
    end =  mean(in_r2, na.rm = TRUE)
  )  %>% select(-1) %>% t() %>% data.frame() %>% 
  rename(mtreat = X1, mcontrol = X2)

tab1_2a = long %>%  
  filter(round==1) %>% group_by(-treat)  %>%
  summarise(
    female = sd(st_female1, na.rm = TRUE),
    age = sd(st_age1, na.rm = TRUE),
    ses = sd(ses_index, na.rm = TRUE),
    grade_4 = sd(grade_4, na.rm = TRUE),
    grade_5 = sd(grade_5, na.rm = TRUE),
    grade_6 = sd(grade_6, na.rm = TRUE),
    grade_7 = sd(grade_7, na.rm = TRUE),
    grade_8 = sd(grade_8, na.rm = TRUE),
    grade_9 = sd(grade_9, na.rm = TRUE),
    math = sd(m_theta_mle, na.rm = TRUE),
    hindi = sd(h_theta_mle, na.rm = TRUE), 
    sd =  sd(in_r2, na.rm = TRUE)
  )  %>% select(-1) %>% t() %>% data.frame() %>% 
  rename(streat = X1, scontrol = X2)

tab1_3a = long %>%  
  filter(round == 1) %>%
  group_by(-treat) %>%
  summarise(
    female = sum(!is.na(st_female1), na.rm = TRUE),
    age = sum(!is.na(st_age1)),
    ses = sum(!is.na(ses_index)),
    grade_4 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_5 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_6 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_7 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_8 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_9 = sum(!is.na(st_grade1), na.rm = TRUE),
    math = sum(!is.na(m_theta_mle)),
    hindi = sum(!is.na(h_theta_mle)),
    end = sum(!is.na(in_r2))
  ) %>%  select(-1) %>% 
  t() %>% 
  data.frame() %>% 
  rename(ntreat = X1, ncontrol = X2)

tab1a = cbind(tab1_1a, tab1_2a,tab1_3a) %>% 
  mutate(dif = round(mtreat - mcontrol,3),
         sd =round(sqrt((streat^2 / ntreat) + (scontrol^2 / ncontrol)),3) ) %>% 
  mutate(mtreat = round(mtreat, 2),
         mcontrol = round(mcontrol, 2)) %>% 
  select("Mean (treatment)" = mtreat,"Mean (control)" = mcontrol, Difference =
           dif, "Standard error" = sd, "Observations (treatment)" = ntreat,"Observations (control)" = ncontrol)


#segunda mitad de la tabla
tab1_1b = long %>%  
  filter(round==1 & in_r2 == 1) %>% group_by(-treat)  %>%
  summarise(
    female = mean(st_female1, na.rm = TRUE),
    age = mean(st_age1, na.rm = TRUE),
    ses = mean(ses_index, na.rm = TRUE),
    grade_4 = mean(grade_4, na.rm = TRUE),
    grade_5 = mean(grade_5, na.rm = TRUE),
    grade_6 = mean(grade_6, na.rm = TRUE),
    grade_7 = mean(grade_7, na.rm = TRUE),
    grade_8 = mean(grade_8, na.rm = TRUE),
    grade_9 = mean(grade_9, na.rm = TRUE),
    math = mean(m_theta_mle, na.rm = TRUE),
    hindi = mean(h_theta_mle, na.rm = TRUE))  %>% select(-1) %>% t() %>% data.frame() %>% 
  rename(mtreat = X1, mcontrol = X2)

tab1_2b = long %>%  
  filter(round==1 & in_r2 == 1) %>% group_by(-treat)  %>%
  summarise(
    female = sd(st_female1, na.rm = TRUE),
    age = sd(st_age1, na.rm = TRUE),
    ses = sd(ses_index, na.rm = TRUE),
    grade_4 = sd(grade_4, na.rm = TRUE),
    grade_5 = sd(grade_5, na.rm = TRUE),
    grade_6 = sd(grade_6, na.rm = TRUE),
    grade_7 = sd(grade_7, na.rm = TRUE),
    grade_8 = sd(grade_8, na.rm = TRUE),
    grade_9 = sd(grade_9, na.rm = TRUE),
    math = sd(m_theta_mle, na.rm = TRUE),
    hindi = sd(h_theta_mle, na.rm = TRUE))  %>% select(-1) %>% t() %>% data.frame() %>% 
  rename(streat = X1, scontrol = X2)

tab1_3b = long %>%  
  filter(round==1 & in_r2 == 1) %>%
  group_by(-treat) %>%
  summarise(
    female = sum(!is.na(st_female1), na.rm = TRUE),
    age = sum(!is.na(st_age1)),
    ses = sum(!is.na(ses_index)),
    grade_4 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_5 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_6 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_7 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_8 = sum(!is.na(st_grade1), na.rm = TRUE),
    grade_9 = sum(!is.na(st_grade1), na.rm = TRUE),
    math = sum(!is.na(m_theta_mle)),
    hindi = sum(!is.na(h_theta_mle))) %>%  select(-1) %>% 
  t() %>% 
  data.frame() %>% 
  rename(ntreat = X1, ncontrol = X2)

tab1b = cbind(tab1_1b, tab1_2b,tab1_3b) %>% 
  mutate(dif = round(mtreat - mcontrol,3),
         sd =round(sqrt((streat^2 / ntreat) + (scontrol^2 / ncontrol)),3) ) %>% 
  mutate(mtreat = round(mtreat, 2),
         mcontrol = round(mcontrol, 2)) %>% 
  select("Mean (treatment)" = mtreat,"Mean (control)" = mcontrol, Difference =
           dif, "Standard error" = sd, "Observations (treatment)" = ntreat,"Observations (control)" = ncontrol)

tab1 = rbind(tab1a,tab1b)
rownames(tab1) = c("Female",
                   "Age (years)", 
                   "SES index", 
                   "Grade 4", 
                   "Grade 5",
                   "Grade 6", 
                   "Grade 7",
                   "Grade 8", 
                   "Grade 9", 
                   "Math",
                   "Hindi", 
                   "Present at endline",
                   "Female ",
                   "Age (years) ", 
                   "SES index ", 
                   "Grade 4 ", 
                   "Grade 5 ",
                   "Grade 6 ", 
                   "Grade 7 ",
                   "Grade 8 ", 
                   "Grade 9 ", 
                   "Math ",
                   "Hindi ")
kable(tab1, format = "html", caption = "Table 1 — SAMPLE DESCRIPTIVES AND BALANCE ON OBSERVABLES")  %>%
  kable_styling(full_width = FALSE, font_size = 10)  %>%
  pack_rows("Panel A. All students in the baseline sample", 1, 12) %>%
  pack_rows("Demographic characteristics", 1, 3) %>%
  pack_rows("Grade in school", 4, 9) %>%
  pack_rows("Baseline test scores", 10, 11) %>%
  pack_rows("Panel B. Only students present in endline", 13, 23) %>%
  pack_rows("Demographic characteristics", 13, 15) %>%
  pack_rows("Grade in school", 16, 21) %>%
  pack_rows("Baseline test scores", 22, 23) %>%
  save_kable(file = "tabla1.html")

#figura 2 
datos<-long %>%  
  group_by(treat,round)  %>%
  summarise(
    math = mean(m_theta_mle, na.rm = TRUE),
    hindi = mean(h_theta_mle, na.rm = TRUE),
    smath = sd(m_theta_mle, na.rm = TRUE),
    shindi = sd(h_theta_mle, na.rm = TRUE),
    nmath = sum(!is.na(m_theta_mle)),
    nhindi = sum(!is.na(h_theta_mle))
  ) %>% 
  round(2) 

# Creación del gráfico
gmath<-ggplot(datos, aes(x = factor(round, labels = c("Baseline", "Endline")))) +
  geom_bar(aes(y = math, fill = factor(treat)), stat = "identity", position = "dodge", width = 0.4) +
  geom_errorbar(aes(ymin = math - (1.96*smath/sqrt(nmath)), ymax = math + (1.96*smath/sqrt(nmath)), group = treat), width = 0.2, position = position_dodge(0.4)) +
  
  geom_text(aes(y = math + 0.02, label = round(math, 2), group = factor(treat)), position = position_dodge(width=0.4), vjust = -0.5) +
  labs(title = "Panel A. Mathematics", x = NULL, y = "Normalized Score", fill = NULL) +
  scale_fill_manual(values = c("lightgrey", "darkblue"), labels = c("Control", "Treatment")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom")



  # Barras para Hindi (en el segundo gráfico)
ghindi<-ggplot(datos, aes(x = factor(round, labels = c("Baseline", "Endline")))) +
  geom_bar(aes(y = hindi, fill = factor(treat)), stat = "identity", position = "dodge", width = 0.4) +
  geom_errorbar(aes(ymin = hindi - (1.96*shindi/sqrt(nhindi)), ymax = hindi + (1.96*shindi/sqrt(nhindi)), group = treat), width = 0.2, position = position_dodge(0.4)) +
  
  geom_text(aes(y = hindi + 0.02, label = round(hindi, 2), group = factor(treat)), position = position_dodge(width=0.4), vjust = -0.5) +
  labs(title = "Panel B. Hindi", x = NULL, y = "Normalized Score", fill = NULL) +
  scale_fill_manual(values = c("lightgrey", "darkblue"), labels = c("Control", "Treatment")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom")
  
(gmath | ghindi) + plot_annotation(title = "Figure 2. Mean Difference in Test Scores between Lottery Winners and Losers", caption = "Figure 2. Mean Difference in Test Scores between Lottery Winners and Losers", theme = theme(plot.title = element_blank()))


#Tabla 2 ------------
wide <- read_dta("ms_blel_jpal_wide.dta ")

#Modelo matemática sin FE (en el paper parece como con FE)
modelomat <- lm(m_theta_mle2 ~ treat + m_theta_mle1, data = wide)
robust_semat <- coeftest(modelomat, vcov = vcovHC(modelomat, type = "HC1"))

#Modelo hindi sin FE (en el paper parece como con FE)
modelohindi <- lm(h_theta_mle2 ~ treat + h_theta_mle1, data = wide)
robust_sehindi <- coeftest(modelohindi, vcov = vcovHC(modelohindi, type = "HC1"))

# Regresiones con efectos fijos
#matemática
modelo_m_fe <- plm(m_theta_mle2 ~ treat + m_theta_mle1, data = wide, index = "strata", model = "within")
robust_se_m_fe <- coeftest(modelo_m_fe, vcov = vcovHC(modelo_m_fe, type = "sss"))
screenreg(modelo_m_fe)

#hindi
modelo_h_fe <- plm(h_theta_mle2 ~ treat + h_theta_mle1, data = wide, index = "strata", model = "within" )
robust_se_h_fe <- coeftest(modelo_h_fe, vcov = vcovHC(modelo_h_fe, type = "sss"))

tab2 <- data.frame(c(round(robust_se_m_fe[1,1],2),
                     round(robust_se_m_fe[1,2],3),
                     round(robust_se_m_fe[2,1],2),
                     round(robust_se_m_fe[2,2],3),
                     round(within_intercept(modelo_m_fe,vcov = function(x) vcovHC(x,  type="sss"))[1],2),
                     round(attr(within_intercept(modelo_m_fe,vcov = function(x) vcovHC(x,  type="sss")), "se"),3),
                     "Yes",
                     length(modelo_m_fe$residuals),
                     round(summary(modelo_m_fe)$r.squared[1],3)),
                   c(round(robust_se_h_fe[1,1],2),
                     round(robust_se_h_fe[1,2],3),
                     round(robust_se_h_fe[2,1],2),
                     round(robust_se_h_fe[2,2],3),
                     round(within_intercept(modelo_h_fe,vcov = function(x) vcovHC(x,  type="sss"))[1],2),
                     round(attr(within_intercept(modelo_h_fe,vcov = function(x) vcovHC(x,  type="sss")), "se"),3),
                     "Yes",
                     length(modelo_h_fe$residuals),
                     round(summary(modelo_h_fe)$r.squared[1],3)) , 
                   c(round(robust_semat[2,1],2),
                     round(robust_semat[2,2],3),
                     round(robust_semat[3,1],2),
                     round(robust_semat[3,2],3),
                     round(robust_semat[1,1],2),
                     round(robust_semat[1,2],3),
                     "No",
                     length(modelomat$residuals),
                     round(summary(modelomat)$r.squared[1],3)) , 
                   c(round(robust_sehindi[2,1],2),
                     round(robust_sehindi[2,2],3),
                     round(robust_sehindi[3,1],2),
                     round(robust_sehindi[3,2],3),
                     round(robust_sehindi[1,1],2),
                     round(robust_sehindi[1,2],3),
                     "No",
                     length(modelohindi$residuals),
                     round(summary(modelohindi)$r.squared[1],3))
)
colnames(tab2) <- c("Math (1)", "Hindi (2)","Math (3)", "Hindi (4)")


rownames(tab2) <- c( "Treatment","","Baseline Score"," ","Constant","  ",
                     "Strata fixed effects","Observations","R2")

kable(tab2, format = "html", caption = "Table 2—Intent-To-Treat (ITT) Effects in a Regression Framework")  %>%
  kable_styling(full_width = FALSE, font_size = 10)  %>%
  add_header_above(c(" ", "Standardized IRT scores (endline)" = 4)) %>% 
  save_kable(file = "tabla2.html") 

#Tabla 4 ------------------------------
#Female 
modelo_m_female <- plm(m_theta_mle2 ~ treat + st_female1 + I(treat * st_female1)  + m_theta_mle1, data = wide, index = "strata", model = "within")
robust_m_female  <- coeftest(modelo_m_female, vcov = vcovHC(modelo_m_female, type = "sss"))

modelo_h_female <- plm(h_theta_mle2 ~ treat + st_female1 + I(treat * st_female1)  + h_theta_mle1, data = wide, index = "strata", model = "within")
robust_h_female  <- coeftest(modelo_h_female, vcov = vcovHC(modelo_h_female, type = "sss"))


#ses 
modelo_m_ses <- plm(m_theta_mle2 ~ treat + ses_index + I(treat * ses_index)  + m_theta_mle1, data = wide, index = "strata", model = "within")
robust_m_ses  <- coeftest(modelo_m_ses, vcov = vcovHC(modelo_m_ses, type = "sss"))

modelo_h_ses <- plm(h_theta_mle2 ~ treat + ses_index + I(treat * ses_index)  + h_theta_mle1, data = wide, index = "strata", model = "within")
robust_h_ses  <- coeftest(modelo_h_ses, vcov = vcovHC(modelo_h_ses, type = "sss"))

#baseline 
modelo_m_base <- plm(m_theta_mle2 ~ treat + m_theta_mle1+ I(treat * m_theta_mle1)  , data = wide, index = "strata", model = "within")
robust_m_base  <- coeftest(modelo_m_base, vcov = vcovHC(modelo_m_base, type = "sss"))

modelo_h_base <- plm(h_theta_mle2 ~ treat + h_theta_mle1+  I(treat * h_theta_mle1) , data = wide, index = "strata", model = "within")
robust_h_base  <- coeftest(modelo_h_base, vcov = vcovHC(modelo_h_base, type = "sss"))

tab4 <- data.frame("Math (1)" = c(round(robust_m_female[1,1],2),
                                  round(robust_m_female[1,2],2),
                                  round(robust_m_female[2,1],2),
                                  round(robust_m_female[2,2],2),
                                  round(robust_m_female[3,1],2),
                                  round(robust_m_female[3,2],2),
                                  round(length(summary(modelo_m_female)$residuals),0),
                                  round(summary(modelo_m_female)$r.squared[1],3)),
                   "Hindi (2)" = c(round(robust_h_female[1,1],2),
                                   round(robust_h_female[1,2],2),
                                   round(robust_h_female[2,1],2),
                                   round(robust_h_female[2,2],2),
                                   round(robust_h_female[3,1],2),
                                   round(robust_h_female[3,2],2),
                                   round(length(summary(modelo_h_female)$residuals),0),
                                   round(summary(modelo_h_female)$r.squared[1],3)),
                   "Math (3)" = c(round(robust_m_ses[1,1],2),
                                  round(robust_m_ses[1,2],3),
                                  round(robust_m_ses[2,1],4),
                                  round(robust_m_ses[2,2],3),
                                  round(robust_m_ses[3,1],3),
                                  round(robust_m_ses[3,2],3),
                                  round(length(summary(modelo_m_ses)$residuals),0),
                                  round(summary(modelo_m_ses)$r.squared[1],3)),
                   "Hindi (4)" = c(round(robust_h_ses[1,1],2),
                                   round(robust_h_ses[1,2],3),
                                   round(robust_h_ses[2,1],3),
                                   round(robust_h_ses[2,2],3),
                                   round(robust_h_ses[3,1],4),
                                   round(robust_h_ses[3,2],3),
                                   round(length(summary(modelo_h_ses)$residuals),0),
                                   round(summary(modelo_h_ses)$r.squared[1],3)),
                   "Math (5)" = c(round(robust_m_base[1,1],2),
                                  round(robust_m_base[1,2],3),
                                  round(robust_m_base[2,1],3),
                                  round(robust_m_base[2,2],3),
                                  round(robust_m_base[3,1],3),
                                  round(robust_m_base[3,2],3),
                                  round(length(summary(modelo_m_base)$residuals),0),
                                  round(summary(modelo_h_base)$r.squared[1],3)),
                   "Hindi (6)" = c(round(robust_h_base[1,1],2),
                                   round(robust_h_base[1,2],3),
                                   round(robust_h_base[2,1],1),
                                   round(robust_h_base[2,2],3),
                                   round(robust_h_base[3,1],3),
                                   round(robust_h_base[3,2],3),
                                   round(length(summary(modelo_h_base)$residuals),0),
                                   round(summary(modelo_h_base)$r.squared[1],3))
)

colnames(tab4) <- c("Math (1)",  "Hindi (2)",
                    "Math (3)",  "Hindi (4)",
                    "Math (5)",  "Hindi (6)")

rownames(tab4) <- c( "Treatment","","Covariate"," ","Interaction","  ",
                    "Observations","R2")
kable(tab4, format = "html", caption = "Table 4—Heterogeneity in Treatment Effect by Gender, Socioeconomic Status, and Baseline Score")  %>%
  kable_styling(full_width = FALSE, font_size = 10)  %>%
  add_header_above(c(" ", "Female" = 2, "SES" = 2, "Baseline score" = 2)) %>% 
  add_header_above(c(" ", "Standardized IRT scores (endline)" = 6)) %>% 
  save_kable(file = "tabla4.html") 



