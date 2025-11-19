#This is the code for all analyses done in the eLABE cortical expansion paper. See github for corresponding data files.

#set up environment and load relevant packages ------------------------------------------------
rm(list=ls())
list.of.packages <- c("dplyr", "readxl", "openxlsx", "ggplot2", "tidyr", "gapminder", "writexl", "car", "reshape2", "MatchIt", 
                      "lm.beta", "matrixStats", "sp", "imager", "ggseg", "ggseg3d", "mgcv", "visreg", "viridis",
                      "patchwork", "ggsegGlasser", "purrr", "brms", "tidybayes", "rstan", "StanHeaders", "tidyverse", "modelr",
                      "marginaleffects", "parameters", "modelsummary", "psych", "tinytable", "lme4", "lmerTest", "foreign", 
                      "lavaan", "semTools")
invisible(lapply(list.of.packages, library, character.only = TRUE))

#set your working directory to whatever file path is appropriate
setwd("C:/your file path/")

#Regenerating the thrive variable using a CFA  ----------------------------------------------------------

#load in the file that is called thrive_raw_data_deidentified.xlsx in github
#note that we generated these values for the entire eLABE cohort, not just the children included in this manuscript's analyses 
rawdata <- read_xlsx("C:/your file path/thrive_raw_data_deidentified.xlsx")

thrive_model <- '
  # First-order latent variables
  nutrition =~ nutr_BM7D4M + nutr_FORM7D4M + nutr_BM7D8M + nutr_FORM7D8M + nutr_BM7DY1 + nutr_FORM7DY1 + nutr_LEATY1
  safety =~ safety_ZPERCB + safety_ZPERC4M + safety_ZPERC8M + safety_ZPERCY1
  enstim =~ enstim_z_ed_nat_birth + enstim_z_ed_nat_4mo + enstim_z_ed_nat_8mo + enstim_z_ed_nat_1yr

  # Single-indicator latent variables
  parenting =~ 1*parenting_composite_Z
  sleep =~ 1*sleep_itsea_Z

  # Second-order latent variable
  THRIVE =~ nutrition + safety + enstim + parenting + sleep
  
  # Residual covariances to improve fit
  safety_ZPERCB ~~ enstim_z_ed_nat_birth
  safety_ZPERCY1 ~~ enstim_z_ed_nat_1yr
  safety_ZPERC4M ~~ enstim_z_ed_nat_4mo
  safety_ZPERC8M ~~ enstim_z_ed_nat_8mo
  enstim_z_ed_nat_8mo ~~ enstim_z_ed_nat_1yr 
  safety_ZPERC4M ~~ enstim_z_ed_nat_8mo
  enstim_z_ed_nat_birth ~~ enstim_z_ed_nat_4mo 
  nutr_FORM7D8M ~~ nutr_BM7DY1 
  nutr_BM7D4M ~~ nutr_BM7DY1 
  safety_ZPERC8M ~~ enstim_z_ed_nat_4mo
  nutr_BM7D4M ~~ nutr_FORM7D8M 
  safety_ZPERCB ~~ enstim_z_ed_nat_1yr 
  safety_ZPERC8M ~~ safety_ZPERCY1 
  safety_ZPERCY1 ~~ enstim_z_ed_nat_birth 
  nutr_BM7D4M ~~ nutr_BM7D8M 
  safety_ZPERC4M ~~ enstim_z_ed_nat_1yr 
  enstim_z_ed_nat_4mo ~~ enstim_z_ed_nat_1yr
  enstim_z_ed_nat_birth ~~ enstim_z_ed_nat_1yr 
  safety_ZPERCB ~~ safety_ZPERC4M
'

# Fit the model
thrive_fit <- cfa(thrive_model, data = rawdata, std.lv = TRUE, missing = "fiml")

# Summarize the fit
summary(thrive_fit, fit.measures = TRUE, standardized = TRUE)
thrive_values <- lavPredict(thrive_fit, method = "EBM")



#Table 1: Demographics ---------------------------------------------------------------------------------

#load in the file called demographics_deidentified.xlsx in github
demographics <- read_xlsx("C:/your file path/demographics_deidentified.xlsx")

#Sex
table(demographics$Sex)

#Race
#Code: 1=White, 2=Black, 3= American Indian/Alaska Native, 4=Asian Indian, 
#5=Chinese, 6= Filipino, 7=Japanese, 8=Korean, 9=Vietnamese, 10=Other Asian, 
#11=Native Hawaiian, 12= Guamanian or Chamorro, 13 = Samoan, 14=Other Pacific Islander, 15 = Other

race <- demographics %>% select(c(MODID, "child_race___1":"child_race___15"))

#see if there are people with multiple races
df_race_count <- race %>%
  mutate(race_count = rowSums(select(., starts_with("child_race")) == 1, na.rm = TRUE))
#there are 4 kids with more than one race listed

#see how many people fall into the other categories (one race only)
one_race_subjects <- df_race_count %>%
  filter(race_count == 1)
column_sums <- colSums(one_race_subjects[sapply(one_race_subjects, is.numeric)], na.rm = TRUE)
print(column_sums)

#ethnicity
table(demographics$child_hispanic)
#0 is not hispanic, 1 is hispanic, 2 is unspecified

#gestational age at birth
demographics %>%
  summarise(
    mean_value = mean(GA_Birth, na.rm = TRUE),
    sd_value = sd(GA_Birth, na.rm = TRUE),
    min_value = min(GA_Birth, na.rm = TRUE),
    max_value = max(GA_Birth, na.rm = TRUE)
  )

#Age at birth MRI
demographics %>%
  summarise(
    mean_value = mean(Age_at_Birth, na.rm = TRUE),
    sd_value = sd(Age_at_Birth, na.rm = TRUE),
    min_value = min(Age_at_Birth, na.rm = TRUE),
    max_value = max(Age_at_Birth, na.rm = TRUE)
  )
#these numbers are in years, so I multiplied by 52 for reporting in the table

#Age at Year 2 MRI
demographics %>%
  summarise(
    mean_value = mean(Age_at_Year2, na.rm = TRUE),
    sd_value = sd(Age_at_Year2, na.rm = TRUE),
    min_value = min(Age_at_Year2, na.rm = TRUE),
    max_value = max(Age_at_Year2, na.rm = TRUE)
  )

#Age at Year 3 MRI
demographics %>%
  summarise(
    mean_value = mean(Age_at_Year3, na.rm = TRUE),
    sd_value = sd(Age_at_Year3, na.rm = TRUE),
    min_value = min(Age_at_Year3, na.rm = TRUE),
    max_value = max(Age_at_Year3, na.rm = TRUE)
  )

#Prenatal disadvantage
demographics %>%
  summarise(
    mean_value = mean(disadv_prenatal, na.rm = TRUE),
    sd_value = sd(disadv_prenatal, na.rm = TRUE),
    min_value = min(disadv_prenatal, na.rm = TRUE),
    max_value = max(disadv_prenatal, na.rm = TRUE)
  )

#Thrive
demographics %>%
  summarise(
    mean_value = mean(Thrive, na.rm = TRUE),
    sd_value = sd(Thrive, na.rm = TRUE),
    min_value = min(Thrive, na.rm = TRUE),
    max_value = max(Thrive, na.rm = TRUE)
  )


#ADI
demographics$ADI <- as.numeric(demographics$ADI)
demographics %>%
  summarise(
    mean_value = mean(ADI, na.rm = TRUE),
    sd_value = sd(ADI, na.rm = TRUE),
    min_value = min(ADI, na.rm = TRUE),
    max_value = max(ADI, na.rm = TRUE)
  )

#INR
demographics %>%
  summarise(
    mean_value = mean(AVG_INR, na.rm = TRUE),
    sd_value = sd(AVG_INR, na.rm = TRUE),
    min_value = min(AVG_INR, na.rm = TRUE),
    max_value = max(AVG_INR, na.rm = TRUE)
  )

#HEI
demographics %>%
  summarise(
    mean_value = mean(HEI, na.rm = TRUE),
    sd_value = sd(HEI, na.rm = TRUE),
    min_value = min(HEI, na.rm = TRUE),
    max_value = max(HEI, na.rm = TRUE)
  )

#Insurance
table(demographics$INSUR)
#0 = public, 1 = private

#Maternal education
table(demographics$EDU)
#0 = did not complete high school, 1 = completed high school, 2 = completed college, 3 = completed graduate school


#Checking to see if those who did not get scanned at year 2 differed demographically from those that did
demographics$scannedat2 <- ifelse(!is.na(demographics$Age_at_Year2), TRUE, FALSE )
demographics$ADI <- as.numeric(demographics$ADI)

t.test(disadv_prenatal ~ scannedat2, data = demographics)
t.test(ADI ~ scannedat2, data = demographics)
t.test(Thrive ~ scannedat2, data = demographics)

year2 <- demographics %>% filter(scannedat2 == "TRUE")
range(year2$ADI)



#Figure 1: Expansion per month figure ------------------------------------------------------------------

#the rest of this figure was made in workbench and/or powerpoint, this code is for panel C

#Load in the data, the file in github is called dataforfigure1_deidentified.csv 
figure1data <- read.csv("C:/your file path/dataforfigure1_deidentified.csv")

#create variables for midpoint age and expansion per month
figure1data$midpointage <- (figure1data$Age_Scan1 + figure1data$Age_Scan2)/2
figure1data$expansionpermonth <- figure1data$tot_expansion / figure1data$Time_Between_Months

#Plot it
plot <- ggplot(figure1data, aes(x = midpointage, y = expansionpermonth)) +
  geom_point() + 
  geom_smooth(color = "indianred") +
  labs(x = "MidPoint Age Between Scans", y = "Total Brain Expansion Per Month", title = " ")
plot


#Figure 2/Supplementary Table 1: Mean expansion Bayesian results -----------------------------------------------------------------

#the rest of this figure was made in workbench and powerpoint
#it is helpful to be mindful of your working directory since bayesian model files are extremely large 
setwd("C:/your folder/")

#Load in data, the file on github is called dataforfigures2and3_deidentified.csv
birthcohort <- read.csv("C:/your file path/dataforfigures2and3_deidentified.csv")
birthcohort$ID <- as.character(birthcohort$ID)

#Set priors
prior.t <- c(prior(normal(0,1), class = b),
             prior(normal(0,2), class = Intercept),
             prior(cauchy(0,2), class = sd))

#Run bayesian model 
MCMCcenteredv1 <- brm(value ~ 1 + Age_Scan1 + Time_Between_Centered + 
                        GA_Birth_Centered + Sex + (1 + disadv_prenatal | ROI) + 
                        (1 | ID),
                      prior = prior.t,
                      data = birthcohort,
                      cores = 6, chains = 4,iter = 4000,
                      control = list(max_treedepth = 15, adapt_delta = 0.99),
                      file = "birthonlyrandomMCMC",
                      backend = "cmdstanr")

summary(MCMCcenteredv1) #this is reported in supplementary table 1

#Get results for each of 360 ROIs
Q1results <- MCMCcenteredv1 %>% 
  spread_draws(r_ROI[ROI,term]) %>%
  filter(term == "disadv_prenatal") %>% 
  median_qi()

colnames(Q1results)[3] = "slope_MCMCv1"
colnames(Q1results)[4] = "lower_MCMCv1"
colnames(Q1results)[5] = "upper_MCMCv1"
colnames(Q1results)[1] = "region"
Q1results$region <- gsub("_ROI", "", Q1results$region)
Q1results <- Q1results %>%
  mutate(hemi = ifelse(
    grepl("^L_", region), "left", "right"
  ))
Q1results$region <- substr(Q1results$region, 3, nchar(Q1results$region))
Q1results$region <- gsub("\\.", "-", Q1results$region)

Q1results <- Q1results %>%
  mutate(MCMCv1_includes0 = (lower_MCMCv1 <= 0 & upper_MCMCv1 >= 0))

#graph the results 
ggseg(Q1results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = slope_MCMCv1)) +
  theme_classic() +
  scale_fill_viridis_c(option = "plasma", na.value = "white", direction = 1) +
  labs(title = "Results for Figure 2, all ROIs shown", fill = "Effect Size")

ggseg(Q1results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = ifelse(MCMCv1_includes0, NA, slope_MCMCv1))) +
  theme_classic() +
  scale_fill_viridis_c(option = "plasma", na.value = "white", direction = 1) +
  labs(title = "Results for Figure 2, only significant ROIs shown", fill = "Effect Size")
#this shows us which ROIs to plot (L_TE1a, L_TE1m, and L_TE1p)


#Make a plot to contextualize the effect size for an example ROI (TE1p)
#here, we are generating predictions for expansion between birth and year 2
predictions(model = MCMCcenteredv1,
            newdata = datagrid(disadv_prenatal = seq(from = -2.2, to = 2.2, length.out = 1000), 
                               Time_Between_Centered = -6.417, #this is equivalent to 24 months
                               Age_Scan1 = 0, #simulating birth 
                               GA_Birth_Centered = -0.34694, #this is equivalent to 38 weeks
                               ROI = "L_TE1p_ROI" #picking one of our significant ROIs
            ),
            allow_new_levels = TRUE) %>%
  posterior_draws() %>%
  mutate(transformed_value = 2^draw) %>% #this is because expansion = log base 2 (SA at older time point/ SA at younger time point)
  ggplot(aes(x = disadv_prenatal, y = transformed_value)) +
  stat_lineribbon(aes(y = transformed_value),
                  .width = .95,
                  alpha = .5,
                  show.legend = F,
                  fill = "gold") +
  labs(
    title = "Disadvantage and Mean Expansion in L_TE1p",
    x = "Prenatal Disadvantage",
    y = "Ratio of SA at Age 2/SA at Birth"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(2.05, 2.3))


#code for plotting just a subset of ROIs in the color you want: 
Q1results <- Q1results %>%
  mutate(highlight = if_else((region == "TE1a" & hemi == "left") | (region == "TE1p" & hemi == "left") | (region == "TE1m" & hemi == "left"), 1, NA))

ggseg(Q1results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = highlight)) +
  theme_classic() +
  scale_fill_viridis_c(option = "plasma", na.value = "white", direction = 1) 







#Figure 3/Supplementary Table 2: Variability Bayesian results -------------------------------------------------------------------

prior.t <- c(prior(normal(0, 2), class = Intercept),
             prior(normal(0, 1), class = b),
             prior(cauchy(0, 1), class = sd),
             prior(normal(0, 2), class = Intercept, dpar = sigma),
             prior(cauchy(0, 0.5), class = sd, dpar = sigma))

Q2_MCMC_yesmain <- brm(bf(value ~ 1 + Age_Scan1 + Time_Between_Centered + 
                            GA_Birth_Centered + Sex + (1 + disadv_prenatal | ROI), 
                          sigma ~  1 + disadv_prenatal + (1 + disadv_prenatal | ROI)),
                       prior = prior.t,
                       data = birthcohort,
                       cores = 6, chains = 4,iter = 4000,
                       control = list(max_treedepth = 15, adapt_delta = 0.99),
                       file = "Q2_MCMC_yesmain",
                       backend = "cmdstanr")

summary(Q2_MCMC_yesmain) #this is reported in supplementary table 2

completesigma <- Q2_MCMC_yesmain %>%
  spread_draws(b_sigma_disadv_prenatal, r_ROI__sigma[ROI,term]) %>%
  filter(term == "disadv_prenatal") %>%
  mutate(slope =exp(r_ROI__sigma + b_sigma_disadv_prenatal)) %>% #if exp is included here, it gives you the 13-14% increase in variability. delete exp in order to check to make sure effects don't include zero.
  select(ROI, slope) %>%
  median_qi()

colnames(completesigma)[3] = "slope_mcmcyes"
colnames(completesigma)[4] = "lower_mcmcyes"
colnames(completesigma)[5] = "upper_mcmcyes"
colnames(completesigma)[1] = "region"
completesigma <- completesigma %>%
  mutate(hemi = ifelse(
    grepl("^L_", region), "left", "right"
  ))
completesigma$region <- substr(completesigma$region, 3, (nchar(completesigma$region))-4)
completesigma$region <- gsub("\\.", "-", completesigma$region)

completesigma <- completesigma %>% 
  mutate(MCMCyes_includes0 = (lower_mcmcyes <= 0 & upper_mcmcyes >= 0))

#plot the results 
ggseg(completesigma,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = slope_mcmcyes)) +
  theme_classic() +
  scale_fill_viridis_c(option = "plasma", na.value = "white", direction = -1, limits = c(1.126, 1.15)) +
  labs(title = " ", fill = "Effect Size") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,   
                               barheight = 1)) 




#Making Panel C of this figure (using a sample ROI):
birthcohortrefined <- birthcohort %>% filter(ROI == "R_V1_ROI")

p <- plot(conditional_effects(Q2_MCMC_yesmain, effects = "disadv_prenatal", dpar = "sigma"))[[1]]

p + labs(x = "Prenatal Disadvantage",
         y = "Residual Standard Deviation") +
  theme_classic()+
  scale_y_continuous(
    limits = c(0.05, 0.15)
  )






#Supplementary Table 3: Surface Area Disadvantage GAMM --------------------------------------------------------------------------------------------

#Help writing these GAMM functions was given by Dr. Ursula Tooley and Dr. Max Herzberg (authors on this paper)

#load in data
complete_data <- read.csv("C:/your file path/dataforfigures4and5_deidentified.csv")
complete_data$MODID <- as.character(complete_data$MODID)

#Run the model 
model2_SA <- gamm(Total_SA ~ Sex + GA_Birth + s(Age_at_Scan, k = 4, bs = "cr") + s(disadv_prenatal, k = 4, bs = "cr") + ti(Age_at_Scan, disadv_prenatal, k = 4), 
                  random = list(MODID = ~1), 
                  data = complete_data, 
                  method = "REML")
summary(model2_SA$lme);summary(model2_SA$gam)
#these results are reported in supplementary table 3

#plot this interaction
percentile_25 <- quantile(complete_data$disadv_prenatal, 0.25, na.rm = TRUE)
percentile_75 <- quantile(complete_data$disadv_prenatal, 0.75, na.rm = TRUE)
age_scan_seq <- seq(min(complete_data$Age_at_Scan, na.rm = TRUE),
                    max(complete_data$Age_at_Scan, na.rm = TRUE),
                    length.out = 100)
newdata <- expand.grid(
  Age_at_Scan = age_scan_seq,
  disadv_prenatal = c(percentile_25, percentile_75),
  GA_Birth = mean(complete_data$GA_Birth, na.rm = TRUE),
  Sex = "Male",  
  MODID = NA 
)
pred <- predict(model2_SA$gam, newdata = newdata, se.fit = TRUE)
pred_df <- data.frame(
  Age_at_Scan = newdata$Age_at_Scan,
  disadv_prenatal = newdata$disadv_prenatal,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)
pred_df$disadv_prenatal_label <- factor(pred_df$disadv_prenatal,
                                        levels = c(percentile_25, percentile_75),
                                        labels = c(paste0(round(percentile_25, 3), " (lowest 25%)"),
                                                   paste0(round(percentile_75, 3), " (highest 25%)")))

SA_plot_combined <- ggplot() +
  geom_line(data = complete_data, 
            aes(x = Age_at_Scan, y = Total_SA, group = MODID), 
            alpha = 0.3, color = "gray34") +
  geom_point(data = complete_data, 
             aes(x = Age_at_Scan, y = Total_SA), 
             alpha = 0.4, size = 0.7, color = "black") +
  geom_ribbon(data = pred_df, 
              aes(x = Age_at_Scan, ymin = lower, ymax = upper, fill = disadv_prenatal_label), 
              alpha = 0.2) +
  geom_line(data = pred_df, 
            aes(x = Age_at_Scan, y = fit, color = disadv_prenatal_label), 
            size = 1) +
  labs(x = "Age at Scan (Years)", 
       y = "Total Brain Surface Area (mm²)", 
       title = "Effect of Disadvantage on Trajectory of Total SA", 
       color = "Disadvantage", 
       fill = "Disadvantage") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) + 
  scale_y_continuous(breaks = seq(40000, 200000, by = 15000))

SA_plot_combined

#Supplementary Table 4: Volume Disadvantage GAMM -----------------------------------------------------------------------------------------
model2_vol <- gamm(Total_Vol ~ Sex + GA_Birth + s(Age_at_Scan, k = 4, bs = "cr") + s(disadv_prenatal, k = 4, bs = "cr") + ti(Age_at_Scan, disadv_prenatal, k = 4), 
                   random = list(MODID = ~1), 
                   data = complete_data, 
                   method = "REML")
summary(model2_vol$lme);summary(model2_vol$gam)


#plot this interaction
pred <- predict(model2_vol$gam, newdata = newdata, se.fit = TRUE)
pred_df <- data.frame(
  Age_at_Scan = newdata$Age_at_Scan,
  disadv_prenatal = newdata$disadv_prenatal,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)

pred_df$disadv_prenatal_label <- factor(pred_df$disadv_prenatal,
                                        levels = c(percentile_25, percentile_75),
                                        labels = c(paste0(round(percentile_25, 3), " (lowest 25%)"),
                                                   paste0(round(percentile_75, 3), " (highest 25%)")))

vol_plot_combined <- ggplot() +
  geom_line(data = complete_data, 
            aes(x = Age_at_Scan, y = Total_Vol, group = MODID), 
            alpha = 0.3, color = "gray34") +
  geom_point(data = complete_data, 
             aes(x = Age_at_Scan, y = Total_Vol), 
             alpha = 0.4, size = 0.7, color = "black") +
  geom_ribbon(data = pred_df, 
              aes(x = Age_at_Scan, ymin = lower, ymax = upper, fill = disadv_prenatal_label), 
              alpha = 0.2) +
  geom_line(data = pred_df, 
            aes(x = Age_at_Scan, y = fit, color = disadv_prenatal_label), 
            size = 1) +
  labs(x = "Age at Scan (Years)", 
       y = "Total Cortical Gray Matter Volume (mm3)", 
       title = "Effect of Disadvantage on Trajectory of Gray Matter Volume", 
       color = "Disadvantage", 
       fill = "Disadvantage") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) 

vol_plot_combined

#Supplementary Table 5: Gyrification Index GAMM -------------------------------------------------------------------------------
model2_GI <- gamm(Avg_GI ~ Sex + GA_Birth + s(Age_at_Scan, k = 4, bs = "cr") + s(disadv_prenatal, k = 4, bs = "cr") + ti(Age_at_Scan, disadv_prenatal, k = 4), 
                  random = list(MODID = ~1), 
                  data = complete_data, 
                  method = "REML")
summary(model2_GI$lme);summary(model2_GI$gam)

#plot this interaction
pred <- predict(model2_GI$gam, newdata = newdata, se.fit = TRUE)
pred_df <- data.frame(
  Age_at_Scan = newdata$Age_at_Scan,
  disadv_prenatal = newdata$disadv_prenatal,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)

pred_df$disadv_prenatal_label <- factor(pred_df$disadv_prenatal,
                                        levels = c(percentile_25, percentile_75),
                                        labels = c(paste0(round(percentile_25, 3), " (lowest 25%)"),
                                                   paste0(round(percentile_75, 3), " (highest 25%)")))

GI_plot_combined <- ggplot() +
  geom_line(data = complete_data, 
            aes(x = Age_at_Scan, y = Avg_GI, group = MODID), 
            alpha = 0.3, color = "gray34") +
  geom_point(data = complete_data, 
             aes(x = Age_at_Scan, y = Avg_GI), 
             alpha = 0.4, size = 0.7, color = "black") +
  geom_ribbon(data = pred_df, 
              aes(x = Age_at_Scan, ymin = lower, ymax = upper, fill = disadv_prenatal_label), 
              alpha = 0.2) +
  geom_line(data = pred_df, 
            aes(x = Age_at_Scan, y = fit, color = disadv_prenatal_label), 
            size = 1) +
  labs(x = "Age at Scan (Years)", 
       y = "Average Gyrification Index", 
       title = "Effect of Disadvantage on Average GI", 
       color = "Disadvantage", 
       fill = "Disadvantage") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) 

GI_plot_combined


#Figure 4: GAMMs put together ----------------------------------------------------------
combined_disadvantage_plot <- (SA_plot_combined + vol_plot_combined + GI_plot_combined) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")  

combined_disadvantage_plot <- combined_disadvantage_plot + 
  plot_annotation(
    title = "Effect of Prenatal Disadvantage on Trajectories",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

combined_disadvantage_plot


#Figure 5/Supplementary Table 6: Thrive disadvantage interaction for Surface Area -------------------------------------------------------------

#For surface area 
model3_SA <- gamm(
  Total_SA ~ Sex + GA_Birth + 
    s(Age_at_Scan, k = 4, bs = "cr") + 
    s(disadv_prenatal, k = 4, bs = "cr") + 
    s(Thrive, k = 4, bs = "cr") + 
    ti(Age_at_Scan, disadv_prenatal, k = 4) + 
    ti(Age_at_Scan, Thrive, k = 4) + 
    ti(disadv_prenatal, Thrive, k = 4) + 
    ti(Age_at_Scan, disadv_prenatal, Thrive, k = 4),
  random = list(MODID = ~1),
  data = complete_data,
  method = "REML"
)

summary(model3_SA$lme);summary(model3_SA$gam)
#these results are reported in the supplement

vis.gam(model3_SA$gam, view = c("disadv_prenatal", "Thrive"), theta = 30)


#plot this to visualize interaction

#creating three levels of thrive
thrive_10 <- quantile(complete_data$Thrive, 0.1, na.rm = TRUE)
thrive_50 <- quantile(complete_data$Thrive, 0.5, na.rm = TRUE)
thrive_90 <- quantile(complete_data$Thrive, 0.9, na.rm = TRUE)

#making a data grid
newdata <- expand.grid(
  Age_at_Scan = 2,
  disadv_prenatal = seq(min(complete_data$disadv_prenatal, na.rm = TRUE),
                        max(complete_data$disadv_prenatal, na.rm = TRUE),
                        length.out = 1000),
  Thrive = c(thrive_10, thrive_50, thrive_90),
  Sex = "Male",
  GA_Birth = mean(complete_data$GA_Birth, na.rm = TRUE),
  MODID = NA
)

#getting predictions for that data grid 
pred <- predict(model3_SA$gam, newdata = newdata, se.fit = TRUE)
pred_df <- data.frame(
  Age_at_Scan = newdata$Age_at_Scan,
  disadv_prenatal = newdata$disadv_prenatal,
  Thrive = newdata$Thrive,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)

#making the thrive labels interpretable
pred_df$Thrive_label <- factor(pred_df$Thrive,
                               levels = c(thrive_90, thrive_50, thrive_10),
                               labels = c(paste0(round(thrive_90, 2), " (High Thrive)"),
                                          paste0(round(thrive_50, 2), " (Medium Thrive)"),
                                          paste0(round(thrive_10, 2), " (Low Thrive)")
                               ))


 

#now we calculate significant difference regions between thrive 10 and thrive 90

#new data grid at 10% thrive
newdata_10 <- expand.grid(
  Age_at_Scan = 2,
  disadv_prenatal = seq(min(complete_data$disadv_prenatal, na.rm = TRUE),
                        max(complete_data$disadv_prenatal, na.rm = TRUE),
                        length.out = 1000),
  Thrive = thrive_10,
  Sex = "Male",
  GA_Birth = mean(complete_data$GA_Birth, na.rm = TRUE),
  MODID = NA
)

#new data grid at 90% thrive
newdata_90 <- expand.grid(
  Age_at_Scan = 2,
  disadv_prenatal = seq(min(complete_data$disadv_prenatal, na.rm = TRUE),
                        max(complete_data$disadv_prenatal, na.rm = TRUE),
                        length.out = 1000),
  Thrive = thrive_90,
  Sex = "Male",
  GA_Birth = mean(complete_data$GA_Birth, na.rm = TRUE),
  MODID = NA
)

#code for creating grey bar that shows where there is a difference between 10th percentile thrive and 90th percentile thrive
Xp_10 <- predict(model3_SA$gam, newdata = newdata_10, type = "lpmatrix")
Xp_90 <- predict(model3_SA$gam, newdata = newdata_90, type = "lpmatrix")
Xp_diff <- Xp_90 - Xp_10
beta <- coef(model3_SA$gam)
Vb <- vcov(model3_SA$gam)
fit_diff <- Xp_diff %*% beta
se_diff <- sqrt(rowSums((Xp_diff %*% Vb) * Xp_diff))
lower_diff <- fit_diff - 1.96 * se_diff
upper_diff <- fit_diff + 1.96 * se_diff

diff_df <- data.frame(
  Age_at_Scan = newdata_10$Age_at_Scan,
  disadv_prenatal = newdata_10$disadv_prenatal,
  fit_diff = as.vector(fit_diff),
  lower = as.vector(lower_diff),
  upper = as.vector(upper_diff)
) %>%
  mutate(
    significant = (lower > 0) | (upper < 0)
    )
  

# Helper function to extract significant contiguous intervals
get_intervals <- function(df) {
  r <- rle(df$significant)
  ends <- cumsum(r$lengths)
  starts <- c(1, head(ends, -1) + 1)
  intervals <- data.frame(start_idx = starts, end_idx = ends, sig = r$values)
  sig_intervals <- intervals[intervals$sig == TRUE, ]
  tibble(
    xmin = df$disadv_prenatal[sig_intervals$start_idx],
    xmax = df$disadv_prenatal[sig_intervals$end_idx]
  )
}

sig_intervals <- get_intervals(diff_df)

# Calculate y-axis mins to position bars just below data lines
ymins <- pred_df %>%
  summarise(min_fit = min(fit, na.rm = TRUE), max_fit = max(fit, na.rm = TRUE))

sig_intervals <- sig_intervals %>%
  mutate(
    min_fit = ymins$min_fit,
    max_fit = ymins$max_fit,
    ymin = min_fit - 0.2 * (max_fit - min_fit), #this is just me manually positioning the grey bar on the graph
    ymax = min_fit - 0.15 * (max_fit - min_fit)
  )

# Plot with bars showing significant Thrive 10 vs 90 differences
plot <- ggplot(pred_df, aes(x = disadv_prenatal, y = fit, color = Thrive_label)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Thrive_label), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  geom_rect(
    data = sig_intervals,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "black",
    alpha = 0.3
  ) +
  labs(
    title = "Predicted Surface Area Given Varying Disadvantage and Thrive",
    x = "Prenatal Disadvantage",
    y = "Predicted Total Surface Area (mm²)",
    color = "Thrive Level",
    fill = "Thrive Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


plot

#Figure 5/Supplementary Table 7: Thrive disadvantage interaction for Volumes -------------------------------------------------------------
complete_data$MODID <- as.character(complete_data$MODID)

model3_Vol <- gamm(
  Total_Vol ~ Sex + GA_Birth + 
    s(Age_at_Scan, k = 4, bs = "cr") + 
    s(disadv_prenatal, k = 4, bs = "cr") + 
    s(Thrive, k = 4, bs = "cr") + 
    ti(Age_at_Scan, disadv_prenatal, k = 4) + 
    ti(Age_at_Scan, Thrive, k = 4) + 
    ti(disadv_prenatal, Thrive, k = 4) + 
    ti(Age_at_Scan, disadv_prenatal, Thrive, k = 4),
  random = list(MODID = ~1),
  data = complete_data,
  method = "REML"
)

summary(model3_Vol$lme);summary(model3_Vol$gam)
#this data is reported in the supplement


vis.gam(model3_Vol$gam, view = c("disadv_prenatal", "Thrive"), theta = 30)


#plot this to visualize interaction
thrive_10 <- quantile(complete_data$Thrive, 0.1, na.rm = TRUE)
thrive_50 <- quantile(complete_data$Thrive, 0.5, na.rm = TRUE)
thrive_90 <- quantile(complete_data$Thrive, 0.9, na.rm = TRUE)

newdata <- expand.grid(
  Age_at_Scan = 2,
  disadv_prenatal = seq(min(complete_data$disadv_prenatal, na.rm = TRUE),
                        max(complete_data$disadv_prenatal, na.rm = TRUE),
                        length.out = 1000),
  Thrive = c(thrive_10, thrive_50, thrive_90),
  Sex = "Male",
  GA_Birth = mean(complete_data$GA_Birth, na.rm = TRUE),
  MODID = NA
)

pred <- predict(model3_Vol$gam, newdata = newdata, se.fit = TRUE)
pred_df <- data.frame(
  Age_at_Scan = newdata$Age_at_Scan,
  disadv_prenatal = newdata$disadv_prenatal,
  Thrive = newdata$Thrive,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)

pred_df$Thrive_label <- factor(pred_df$Thrive,
                               levels = c(thrive_90, thrive_50, thrive_10),
                               labels = c(paste0(round(thrive_90, 2), " (High Thrive)"),
                                          paste0(round(thrive_50, 2), " (Medium Thrive)"),
                                          paste0(round(thrive_10, 2), " (Low Thrive)")
                               ))


#now we calculate significant difference regions between thrive 10 and thrive 90
newdata_10 <- expand.grid(
  Age_at_Scan = 2,
  disadv_prenatal = seq(min(complete_data$disadv_prenatal, na.rm = TRUE),
                        max(complete_data$disadv_prenatal, na.rm = TRUE),
                        length.out = 1000),
  Thrive = thrive_10,
  Sex = "Male",
  GA_Birth = mean(complete_data$GA_Birth, na.rm = TRUE),
  MODID = NA
)
newdata_90 <- expand.grid(
  Age_at_Scan = 2,
  disadv_prenatal = seq(min(complete_data$disadv_prenatal, na.rm = TRUE),
                        max(complete_data$disadv_prenatal, na.rm = TRUE),
                        length.out = 1000),
  Thrive = thrive_90,
  Sex = "Male",
  GA_Birth = mean(complete_data$GA_Birth, na.rm = TRUE),
  MODID = NA
)
Xp_10 <- predict(model3_Vol$gam, newdata = newdata_10, type = "lpmatrix")
Xp_90 <- predict(model3_Vol$gam, newdata = newdata_90, type = "lpmatrix")
Xp_diff <- Xp_90 - Xp_10
beta <- coef(model3_Vol$gam)
Vb <- vcov(model3_Vol$gam)
fit_diff <- Xp_diff %*% beta
se_diff <- sqrt(rowSums((Xp_diff %*% Vb) * Xp_diff))
lower_diff <- fit_diff - 1.96 * se_diff
upper_diff <- fit_diff + 1.96 * se_diff

diff_df <- data.frame(
  Age_at_Scan = newdata_10$Age_at_Scan,
  disadv_prenatal = newdata_10$disadv_prenatal,
  fit_diff = as.vector(fit_diff),
  lower = as.vector(lower_diff),
  upper = as.vector(upper_diff)
) %>%
  mutate(
    significant = (lower > 0) | (upper < 0)
  )


# Helper function to extract significant contiguous intervals
get_intervals <- function(df) {
  r <- rle(df$significant)
  ends <- cumsum(r$lengths)
  starts <- c(1, head(ends, -1) + 1)
  intervals <- data.frame(start_idx = starts, end_idx = ends, sig = r$values)
  sig_intervals <- intervals[intervals$sig == TRUE, ]
  tibble(
    xmin = df$disadv_prenatal[sig_intervals$start_idx],
    xmax = df$disadv_prenatal[sig_intervals$end_idx]
  )
}

sig_intervals <- get_intervals(diff_df)

# Calculate y-axis mins to position bars just below data lines
ymins <- pred_df %>%
  summarise(min_fit = min(fit, na.rm = TRUE), max_fit = max(fit, na.rm = TRUE))

sig_intervals <- sig_intervals %>%
  mutate(
    min_fit = ymins$min_fit,
    max_fit = ymins$max_fit,
    ymin = min_fit - 0.2 * (max_fit - min_fit), #this is just me manually positioning the grey bar on the graph
    ymax = min_fit - 0.15 * (max_fit - min_fit)
  )


# Plot with bars showing significant Thrive 10 vs 90 differences

plot <- ggplot(pred_df, aes(x = disadv_prenatal, y = fit, color = Thrive_label)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Thrive_label), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  geom_rect(
    data = sig_intervals,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "black",
    alpha = 0.3
  ) +
  labs(
    title = "Predicted Cortical Volume Given Varying Disadvantage and Thrive",
    x = "Prenatal Disadvantage",
    y = "Predicted Cortical Volume (mm3)",
    color = "Thrive Level",
    fill = "Thrive Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


plot


#Supplementary Table 8: Thrive disadvantage interaction for GI -------------------------------------------------------------
complete_data$MODID <- as.character(complete_data$MODID)

model3_GI <- gamm(
  Avg_GI ~ Sex + GA_Birth + 
    s(Age_at_Scan, k = 4, bs = "cr") + 
    s(disadv_prenatal, k = 4, bs = "cr") + 
    s(Thrive, k = 4, bs = "cr") + 
    ti(Age_at_Scan, disadv_prenatal, k = 4) + 
    ti(Age_at_Scan, Thrive, k = 4) + 
    ti(disadv_prenatal, Thrive, k = 4) + 
    ti(Age_at_Scan, disadv_prenatal, Thrive, k = 4),
  random = list(MODID = ~1),
  data = complete_data,
  method = "REML"
)

summary(model3_GI$lme);summary(model3_GI$gam)
#results reported in supplement

#there wasn't a significant interaction so I didn't plot it











