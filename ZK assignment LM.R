install.packages("tidyverse")
library(tidyverse)
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") 
View(data_sample_1)
library(psych)
library(gridExtra)
library(lm.beta)
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(broom)
library(lme4)
library(MuMIn)
install.packages(cAIC4)
library(cAIC4)
library(influence.ME)
library(lattice) # for qqmath	
library(lmerTest)

#Assignment 1 
#Outliers
data_sample_1_nooutliers <- data_sample_1 %>%
  filter(age < 60, STAI_trait > 20) %>%
  mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1))

view(data_sample_1)
------------------------------------------------
  
  # Plotting the variables: 
  data_sample_1_nooutliers %>% 	
  mutate(rownum = row.names(data_sample_1_nooutliers)) %>%  	
  ggplot() +	
  aes(x = age, y = pain, label = rownum) +	
  geom_point() +	
  geom_text()	

data_sample_1_nooutliers%>% 	
  mutate(rownum = row.names(data_sample_1_nooutliers)) %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain, label = rownum) +	
  geom_label()

data_sample_1_nooutliers %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm")	

LMM_3 %>% 	
  plot(which = 5)	

M2 %>%
  plot(which = 4)

# histogram	
data_sample_1_nooutliers %>%
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()+
  geom_smooth(method = lm)

data_sample_1_nooutliers %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram(bins = 30)	

data_sample_1_nooutliers %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram(bins = 30) 	

data_sample_1_nooutliers %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_bar() +	
  scale_x_continuous(breaks = 4:12)	

data_sample_1_nooutliers %>%	
  select(sex, pain) %>%	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram() +	
  facet_wrap(~ sex)	

data_sample_1_nooutliers %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()	

#cooks distance
plot(x = LMM_3, which = 4) 

backCoefPlot <- tidy(LMM_3)

# Multiple Linear Regression: 
model1 <- lm(pain ~ age + sex_recode, data = data_sample_1_nooutliers)

summary(model1) 

model2 <- lm(pain ~ age + sex_recode + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_1_nooutliers)

summary(model2) 

data_sample_1 <- data_sample_1 %>% 	
  mutate(sex = factor(sex)) 

# Comparing two models 

anova(model1, model2)
anova(lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data=data_sample_1))

---------------------------------------
  
  summary(model1)$adj.r.squared	             
summary(model2)$adj.r.squared	    


AIC(model1)
AIC(model2)

lm.beta(model1)
lm.beta(model2)


sm_table = coef_table(model2)	
sm_table
-----------------------
  
  Normality
model2 %>% 	
  plot(which = 2)

residuals_model1 = enframe(residuals(model1))	
residuals_model1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

describe(residuals(model1))	
describe(residuals(model2))	

Linearity
plot(x = model2, which = 1)
model2 %>% 	
  residualPlots()	

residualPlots(model = model2) 

Homodecasticity 
model2 %>% 	
  plot(which = 3)	

model2 %>% 	
  ncvTest()

model2 %>% 	
  bptest()

plot(x = model2, which = 3)

Multicollinearity 
model2 %>% vif()

vif(mod = model2) 

-----------------------------------
  #Assignment 2 
  home_sample_2_remove <- home_sample_2 %>%
  slice(-c(8, 125))

home_sample_2_nooutliers <- home_sample_2_remove %>% 
  mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1))

data_sample_1_nooutliers <- data_sample_1 %>%
  filter(age < 60, STAI_trait > 20, household_income > 0 ) %>% 
  mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1))  

home_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")
View(home_sample_2)
summary(home_sample_2)
library(scatterplot3d)
-------------------------------------------------------
  #Cook's distance
  cooks_distance <- lm(pain ~ pain_cat, data = home_sample_2)
plot(x = cooks_distance, which = 4) 
-----------------------------------------------------------
  #Backward regression
  model_her_variables <- lm(pain ~ age + sex_recode + STAI_trait + pain_cat + mindfulness+ cortisol_serum + weight + IQ + household_income, data = data_sample_1_nooutliers)
step(object = model_her_variables, direction = "backward")

summary(model_her_variables)
AIC(model_her_variables)

backward_model <- lm(pain ~ sex_recode + age + pain_cat + mindfulness + cortisol_serum + household_income, data = data_sample_1_nooutliers)

theorybased_model <- lm(pain ~ sex_recode + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_1_nooutliers)
-------------------------------------------------
  #Comparison
  AIC(backward_model)
AIC(theorybased_model) 

anova(backward_model, theorybased_model)

summary(backward_model)$adj.r.squared	             
summary(theorybased_model)$adj.r.squared

summary(backward_model)
summary(theorybased_model)

sm_table = coef_table(model_her_variables)	
sm_table

#prediction performance
pred_backward <- predict(backward_model, home_sample_2_nooutliers)
pred_theory <- predict(theorybased_model, home_sample_2_nooutliers)
RSS_backward = sum((home_sample_2_nooutliers[, "pain"] - pred_backward)^2)
RSS_theory = sum((home_sample_2_nooutliers[, "pain"] - pred_theory)^2)
pred_inital <- predict(model_her_variables, home_sample_2_nooutliers)
RSS_inital <- sum((home_sample_2_nooutliers[, "pain"] - pred_inital)^2)

RSS_backward
RSS_theory
RSS_inital

#Visualization
home_sample_2_nooutliers %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram( bins = 20)	

home_sample_2 %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram(bins = 20) 

home_sample_2 %>% 	
  ggplot() +	
  aes(x = weight) +	
  geom_histogram(bins = 20) 

home_sample_2_nooutliers %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram(bins = 20) 

home_sample_2_nooutliers %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_bar() +	
  scale_x_continuous(breaks = 4:12)	

data_sample_1 %>%	
  select(sex, pain) %>%	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram() +	
  facet_wrap(~ sex)	

home_sample_2_nooutliers%>% 	
  ggplot() +	
  aes(x = household_income, y = pain) +	
  geom_point() +
  geom_smooth(method = lm)

home_sample_2 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()
--------------------------
  #Model checking
  Normality
backward_model %>% 	
  plot(which = 2)

residuals_backward_model = enframe(residuals(backward_model))	
residuals_backward_model %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

describe(residuals(backward_model))	

Linearity
plot(x = backward_model, which = 1)

backward_model %>% 	
  residualPlots()	

residualPlots(model = backward_model) 

Homodecasticity 
backward_model %>% 	
  plot(which = 3)	

backward_model %>% 	
  ncvTest()

backward_model %>% 	
  bptest()

plot(x = backward_model, which = 3)

Multicollinearity 
backward_model %>% vif()

vif(mod = backward_model)
-----------------------------------------------
  #Assignment 3 
  data_file_3 = read.csv("https://tinyurl.com/ha-dataset3")
data_file_4 = read.csv("https://tinyurl.com/ha-dataset4")

View(data_file_3)
View(data_file_4)

121: pain = 0
182: sex = femlae 

data_file_3_remove <- data_file_3 %>%
  slice(-c(182, 121)) 

data_file_3_sex <- data_file_3_remove %>%
  mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1))

data_file_3_nooutliers <- data_file_3_sex %>%
  mutate(hospital = factor(hospital))

#Visualization 
data_file_4 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point()

data_file_3_nooutliers%>%	
  select (pain) %>%	
  ggplot() +	
  aes(pain) +	
  geom_histogram()

data_file_3_nooutliers %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point() +
  geom_smooth(method = lm)


#Random effect intercept model (this is the one I'll be using)
LMM_3 <- lmer(pain ~ pain_cat + STAI_trait + age + sex_recode + mindfulness + cortisol_serum + (1|hospital), data = data_file_3_nooutliers, REML = FALSE)
summary(LMM_3)

Extracting fixed effects estimates
fixef(LMM_3)

Extracting random effects estimates
ranef(LMM_3)

#To calculate marginal and conditional R2:
r.squaredGLMM(LMM_3)

r.squaredGLMM(LMM_3_slope)


#To calculate CI 
confint(LMM_3)

stdCoef.merMod(LMM_3)
----------------------------------------
  #Cook's distance
  cooks_distance <- lm(pain ~ pain_cat + STAI_trait + age + sex_recode + mindfulness + cortisol_serum + (1|hospital), data = data_file_3_nooutliers)
plot(x = cooks_distance, which = 4) 

int_plot <- data_file_3_nooutliers %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)	

int_plot+xlim(-1, 50)+geom_hline(yintercept=0)+geom_vline(xintercept=0)

data_file_3_nooutliers %>% 		
  ggplot() +		
  aes(y = pain, x = age, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

slope_plot <- data_file_3_nooutliers %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE) 

slope_plot +
  xlim(-1, 50) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0)


#slope and intercept model
anova(LMM_3)
summary(LMM_3)
summary(m1)

lrtest(LMM_3, LMM_3_slope)
anova(LMM_3, LMM_3_slope)

coef(LMM_3)

LMM_3_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = data_file_3_nooutliers, REML = FALSE)	
summary(LMM_3_slope)

#prediction performace
data_file_4_remove <- data_file_4 %>%
  slice(-c(9, 111))

data_file_4_sex <- data_file_4_remove %>%
  mutate(sex_recode = recode(sex, 
                             "female" = 0,
                             "male" = 1))

data_file_4_nooutliers <- data_file_4_sex %>% 
  mutate(hospital = factor(hospital))

RSS <- sum((data_file_4_nooutliers$pain - predict(LMM_3, data_file_4_nooutliers, allow.new.levels = TRUE))^2)
mod_mean<- lm(pain~1, data_file_3_nooutliers)
TSS <-sum((data_file_4_nooutliers$pain - predict(mod_mean, data_file_4_nooutliers))^2)
RSS
TSS
1-RSS/TSS

#Check model fit
cAIC(LMM_3)
cAIC(LMM_3_slope)
anova(LMM_3, LMM_3_slope)

sum(residuals(LMM_3)^2)		
sum(residuals(LMM_3_slope)^2)		

confint(LMM_3_slope)

----------------
  int_plot = data_file_3_nooutliers%>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, color = hospital)+
  geom_point(size = 4)+
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

int_plot

int_plot+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)


slope_plot = data_file_3_nooutliers %>%
  ggplot()+
  aes(y = pain, x = cortisol_serum, color = hospital)+
  geom_point(size = 4)+
  geom_smooth(method = "lm", se = F, fullrange=TRUE)+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

slope_plot


data_file_3_slope = data_file_3_nooutliers %>%
  mutate(pred_int = predict(LMM_3),
         pred_slope = predict(LMM_3_slope))

data_file_3_slope %>% 
  ggplot()+ 
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4)+
  geom_line(color='red',aes(y=pred_int, x=cortisol_serum))+
  facet_wrap(~hospital, ncol = 2)

data_file_3_slope%>%
  ggplot()+
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4)+
  geom_line(color='red',aes(y=pred_slope, x=cortisol_serum))+
  facet_wrap(~hospital, ncol = 2)

slope_plot = data_file_3_slope%>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE) +
  xlim(0, 10)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)  