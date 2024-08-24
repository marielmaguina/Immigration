# Packages ----------------------------------------------------------------------------------------------------------------

install.packages("margins")
install.packages("marginaleffects")
install.packages("clubSandwich")
install.packages("AER")
install.packages("oglmx")

library(dplyr) #Data manipulation package
library(stargazer)
library(haven)
library(MASS)
library(sandwich)
library(ggplot2)
library(forcats)
library(margins)
library(marginaleffects)
library(lmtest)
library(clubSandwich)
library(AER)
library(oglmx)


# Read data and cleaning --------------------------------------------------------------------------------------------------

#data <- read_dta("Project 3.0/2021 Canadian Election Study v2.0.dta")
data <- read_dta("2021 Canadian Election Study v2.0.dta")
#data <- read_dta("Documents/McGill/ECON 468/Project 3.0/2021 Canadian Election Study v2.0.dta")
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FXBZHKC&fbclid=IwAR0aLumla0JS8l5R4H5OFQdQ-aaEAm6r8SRJ1eeXLYBD0BbHBBRi4vAL2IA

data_cleaned <- data %>% dplyr::select(cps21_age, # 1. Age 
                                       cps21_spend_defence, # 3. How much should the federal government spend on defence?
                                       cps21_spend_imm_min, # 4. How much should the federal government spend on immigrants and minorities?
                                       cps21_own_fin_retro, # 6. Over the past year, has your financial situation:
                                       cps21_own_fin_future, # 7 Over the next year, do you think your financial situation will:
                                       cps21_bornin_canada, # 8. Where you born in Canada?
                                       cps21_imm_year, # 9. In what year did you come to Canada?
                                       cps21_citizenship, # 10. Are you Canadian citizen? 
                                       cps21_genderid, # 11. Gender
                                       cps21_yob, # 12. Year of birth
                                       cps21_lr_scale_bef_1, # 15. 0 is left and 10 is right, where would you place yourself?
                                       cps21_immig_status, # 16. Under which immigration category did you enter Canada or become a permanent resident in Canada?
                                       cps21_imm, # 17. Do you think Canada should admit more/less/about the same number of immigrants?
                                       pes21_immigjobs) %>% #Immigrants take jobs away from other Canadians. Disagree, agree...
  
  filter(cps21_spend_defence != 4) %>% # Drop rows where the person decide to not reply to cps21_spend_defence
  filter(cps21_spend_imm_min != 4) %>% # Drop rows where the person decide to not reply to cps21_spend_imm_min
  filter(cps21_imm != 4) %>% # Drop rows where the person decide to not reply to cps21_imm
  filter(cps21_own_fin_retro != 4) %>% # Drop rows where the person decide to not reply to cps21_own_fin_retro
  filter(cps21_own_fin_future != 4) %>% # Drop rows where the person decide to not reply to cps21_own_fin_future
  filter(cps21_bornin_canada != 3) %>% # Drop rows where the person decide to not reply to cps21_bornin_canada
  filter(cps21_genderid == 1 | cps21_genderid == 2) %>% # Keep only female and male genders, few observations are dropped
  filter(cps21_lr_scale_bef_1 != -99) %>% # Drop rows where the person decide to not reply to cps21_lr_scale_bef_1 
  mutate(cps21_immig_status = ifelse(is.na(cps21_immig_status), -99, cps21_immig_status)) %>% # Turn NaNs in cps21_immig_status to -99
  filter(cps21_immig_status != 6) # Drop people who came to Canada as refugees

#nrow(data_cleaned)
#colnames(data_cleaned)
  
# Data Manipulation ----------------------------------------------------------------------------------------------------------------------------

data_manipulated <- data_cleaned

# cps21_age -> No manipulations

# cps21_spend_defence
data_manipulated$cps21_spend_defence = ifelse(data_cleaned$cps21_spend_defence==1, "SpendLess", ifelse(data_cleaned$cps21_spend_defence==2, "SpendTheSame", "SpendMore"))

# cps21_spend_imm_min
data_manipulated$cps21_spend_imm_min <- factor(data_manipulated$cps21_spend_imm_min)

# cps21_imm (*)
data_manipulated$cps21_imm <- ifelse(data_manipulated$cps21_imm==1, 3,
                                     ifelse(data_manipulated$cps21_imm==2, 1, 2))
data_manipulated$cps21_imm <- factor(data_manipulated$cps21_imm)

# cps21_own_fin_retro
data_manipulated$cps21_own_fin_retro = ifelse(data_cleaned$cps21_own_fin_retro==1, "GotBetter", ifelse(data_cleaned$cps21_own_fin_retro==2, "StayedTheSame", "GotWorse"))

# cps21_own_fin_future
data_manipulated$cps21_own_fin_future = ifelse(data_cleaned$cps21_own_fin_future==1, "GetBetter", ifelse(data_cleaned$cps21_own_fin_future==2, "StayTheSame", "GetWorse"))

# cps21_bornin_canada: Born in Canada = 0, not Born in Canada = 1 (*)
names(data_manipulated)[names(data_manipulated) == "cps21_bornin_canada"] <- "cps21_not_bornin_canada"
data_manipulated$cps21_not_bornin_canada = ifelse(data_manipulated$cps21_not_bornin_canada==1, 0, 1)

# cps21_imm_year
data_manipulated$cps21_imm_year = data_manipulated$cps21_imm_year + 1919

# cps21_citizenship, Citizen = 1, Not Citizen = 0 (*)
data_manipulated$cps21_citizenship = ifelse(data_cleaned$cps21_citizenship==1, 1, 0) 

# cps21_genderid
data_manipulated$cps21_genderid = ifelse(data_cleaned$cps21_genderid==1, "Man", "Woman")

# cps21_yob
data_manipulated$cps21_yob <- 2004 - data_manipulated$cps21_yob

# years_outof_Canada
data_manipulated <- data_manipulated %>% mutate(years_outof_Canada = ifelse(cps21_not_bornin_canada==0, 0, cps21_imm_year-cps21_yob)) # Create how many years out of Canada
data_manipulated <- data_manipulated %>% filter(years_outof_Canada>=0)

# Create a column that indicates if the person is Canadian-born, a naturalized Citizen, or a foreign citizen
data_manipulated <- data_manipulated %>% mutate(exp_var = ifelse(cps21_citizenship==0, "Foreign Citizen", 
                                                                 ifelse(cps21_not_bornin_canada==1, "AANaturalized Canadian", 
                                                                        "Canadian-born")))
data_manipulated$exp_var <- factor(data_manipulated$exp_var, levels = c("Foreign Citizen", "Naturalized Citizen", "Canadian-born"))
#View(data_manipulated)

# Regressions: Dependent V: Spending on immigrants ----------------------------------------------------------------------------------------------------------------------------

model_1 <- polr(cps21_spend_imm_min ~ exp_var, # Foreign Citizen, Canadian Born, Naturalized Citizen
                data=data_manipulated, Hess=TRUE)
summary(model_1)

ME1  <- marginaleffects(model_1, type = "probs")
ME1_ <- ME1 %>% dplyr::select(contrast, estimate, group) %>% group_by(contrast, group) %>% summarise(mean(estimate))
ME1_

model_2 <- polr(cps21_spend_imm_min ~ exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid, # Control for Age, Gender
                data=data_manipulated) 
summary(model_2)

ME2  <- marginaleffects(model_2, type = "probs")
ME2_ <- ME2 %>% dplyr::select(contrast, estimate, group) %>% group_by(contrast, group) %>% summarise(mean(estimate))
ME2_

AME_2 <- margins(model_2)
summary(AME_2)


model_3 <- polr(cps21_spend_imm_min ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence, # Control for preferences in spending on defense (More, less, about the same)
                data = data_manipulated)
summary(model_3)

ME3  <- marginaleffects(model_3, type = "probs")
ME3_ <- ME3 %>% dplyr::select(contrast, estimate, group) %>% group_by(contrast, group) %>% summarise(mean(estimate))
ME3_

model_4 <- polr(cps21_spend_imm_min ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence + # Control for preferences in spending on defense (More, less, about the same)
                  cps21_lr_scale_bef_1, # Control for political beliefs (0 left, 10 right)
                data = data_manipulated)
summary(model_4)

ME4  <- marginaleffects(model_4, type = "probs")
ME4_ <- ME4 %>% dplyr::select(contrast, estimate, group) %>% group_by(contrast, group) %>% summarise(mean(estimate))
ME4_

model_5 <- polr(cps21_spend_imm_min ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence + # Control for preferences in spending on defense (More, less, about the same)
                  cps21_lr_scale_bef_1 + # Control for political beliefs (0 left, 10 right)
                  cps21_own_fin_retro + cps21_own_fin_future, # Control for financial situation
                 data = data_manipulated)
summary(model_5)

ME5  <- marginaleffects(model_5, type = "probs")
ME5_ <- ME5 %>% dplyr::select(contrast, estimate, group) %>% group_by(contrast, group) %>% summarise(mean(estimate))
ME5_

# Regressions: Dependent V: Want more immigrants?  ----------------------------------------------------------------------------------------------------------------------------

model_6 <- polr(cps21_imm ~ exp_var, # Foreign Citizen, Canadian Born, Naturalized Citizen
                data=data_manipulated)
summary(model_6)

model_7 <- polr(cps21_imm ~ exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid, # Control for Age, Gender
                data=data_manipulated) 
summary(model_7)

model_8 <- polr(cps21_imm ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence, # Control for preferences in spending on defense (More, less, about the same)
                data = data_manipulated)
summary(model_8)

model_9 <- polr(cps21_imm ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence + # Control for preferences in spending on defense (More, less, about the same)
                  cps21_lr_scale_bef_1, # Control for political beliefs (0 left, 10 right)
                data = data_manipulated)
summary(model_9)

model_10 <- polr(cps21_imm ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence + # Control for preferences in spending on defense (More, less, about the same)
                  cps21_lr_scale_bef_1 + # Control for political beliefs (0 left, 10 right)
                  cps21_own_fin_retro + cps21_own_fin_future, # Control for financial situation
                data = data_manipulated)
summary(model_10)

# Regressions: Dependent V: Are immigrants stealing jobs?  ----------------------------------------------------------------------------------------------------------------------------

data_res <- data_manipulated %>% filter(!is.na(pes21_immigjobs) & pes21_immigjobs != 6) #Observations are reduced to 10 949
#data_res$pes21_immigjobs <- factor(data_res$pes21_immigjobs) 

model_11 <- polr(pes21_immigjobs ~ exp_var, # Foreign Citizen, Canadian Born, Naturalized Citizen
                data=data_res)
summary(model_11)

model_12 <- polr(pes21_immigjobs ~ exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid, # Control for Age, Gender
                data=data_res) 
summary(model_12)

model_13 <- polr(pes21_immigjobs ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence, # Control for preferences in spending on defense (More, less, about the same)
                data = data_res)
summary(model_13)

model_14 <- polr(pes21_immigjobs ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence + # Control for preferences in spending on defense (More, less, about the same)
                  cps21_lr_scale_bef_1, # Control for political beliefs (0 left, 10 right)
                data = data_res)
summary(model_14)

model_17 <- polr(pes21_immigjobs ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                   cps21_age + cps21_genderid + # Control for Age, Gender
                   cps21_spend_defence + # Control for preferences in spending on defense (More, less, about the same)
                   cps21_lr_scale_bef_1 + # Control for political beliefs (0 left, 10 right)
                   cps21_own_fin_retro + cps21_own_fin_future, # Control for financial situation
                 data = data_res)
summary(model_17)

model_18 <-polr(pes21_immigjobs ~  exp_var + # Foreign Citizen, Canadian Born, Naturalized Citizen
                  cps21_age + cps21_genderid + # Control for Age, Gender
                  cps21_spend_defence + # Control for preferences in spending on defense (More, less, about the same)
                  cps21_lr_scale_bef_1 + # Control for political beliefs (0 left, 10 right)
                  cps21_own_fin_retro + cps21_own_fin_future+ # Control for financial situation
                  years_outof_Canada, # Control for years before moving to Canada (0 for Canadian-born)
                data = data_res)
summary(model_18)

# Preliminary Analysis  ------------------------------------------------------------------------------------------------------------------------------------------------------------

# Graph 1
# Calculate proportions by grouping 'exp_var' and 'cps21_spend_imm_min'
data_proportions_1 <- data_manipulated %>%
  group_by(exp_var, cps21_spend_imm_min) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Create a grouped bar plot showing relative proportions
ggplot(data_proportions_1, aes(x = factor(cps21_spend_imm_min), y = Proportion, fill = fct_relevel(exp_var, "Foreign Citizen", "Naturalized Canadian", "Canadian-born"))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(x = "", y=" ", title = "Relative Distribution of Preferences Over Spending On Immigrants") +
  scale_fill_discrete(name = "exp_var") +
  theme_minimal() + theme(plot.title = element_text(size = 12)) + 
  theme(legend.position="bottom") +
  guides(fill = guide_legend(title = NULL)) +
  scale_x_discrete(labels = c("Spend Less", "Spend About The Same", "Spend More"))

# Graph 2
# Calculate proportions by grouping 'exp_var' and 'cps21_imm'
data_proportions_2 <- data_manipulated %>%
  group_by(exp_var, cps21_imm) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Create a grouped bar plot showing relative proportions
ggplot(data_proportions_2, aes(x = factor(cps21_imm), y = Proportion, fill = fct_relevel(exp_var, "Foreign Citizen", "Naturalized Canadian", "Canadian-born"))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(x = "", y=" ", title = "Relative Distribution of Preferences Over Intake of Immigrants") +
  scale_fill_discrete(name = "exp_var") +
  theme_minimal() + theme(plot.title = element_text(size = 12)) + 
  theme(legend.position="bottom") +
  guides(fill = guide_legend(title = NULL)) +
  scale_x_discrete(labels = c("Admit Less", "Admit About The Same", "Admit More"))

# Graph 3
# Calculate proportions by grouping 'exp_var' and 'pes21_immigjobs'
data_proportions_3 <- data_res %>%
  group_by(exp_var, pes21_immigjobs) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Create a grouped bar plot showing relative proportions
ggplot(data_proportions_3, aes(x = factor(pes21_immigjobs), y = Proportion, fill = fct_relevel(exp_var, "Foreign Citizen", "Naturalized Canadian", "Canadian-born"))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(x = "", y=" ", title = "Opinion about immigrants taking jobs away from Canadians") +
  scale_fill_discrete(name = "exp_var") +
  theme_minimal() + theme(plot.title = element_text(size = 12)) + 
  theme(legend.position="bottom") +
  guides(fill = guide_legend(title = NULL)) +
  scale_x_discrete(labels = c("Strongly Disagree", "", " ", " ", "Strongly Agree"))

data_manipulated %>% group_by(exp_var) %>% summarise(count=n())
View(data_manipulated)

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# DONE Dependent variable: spending and want more/less immigrants, stealing jobs 
# DONE Graphs showing stuff, correlation table, 
# Ask about the interpretation of the coefficient
# Get the heteroskedastic standard errors

bread_matrix <- bread(model_1)
meat_matrix <- meat(model_1)
robust_se <- bread_matrix %*% solve(meat_matrix) %*% t(bread_matrix)
coeftest(model_1, vcov = robust_se)

# Stuff for the effect thing ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# https://stackoverflow.com/questions/67138433/how-to-use-margins-package-to-evaluate-marginal-affects-at-different-values-of-t


