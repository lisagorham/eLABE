#The Impact of Prenatal Disadvantage and Postnatal Enrichment on Early Structural Brain Development

#This is the code for all analyses done in the eLABE cortical expansion paper. See OSF repository for corresponding data files.
#Code was written by Lisa Gorham, with help from Dr. Joshua Jackson, Dr. Ursula Tooley, Dr. Max Herzberg, and Shelby Leverett. 

#Set up environment and load relevant packages ------------------------------------------------
rm(list=ls())
list.of.packages <- c("readxl", "openxlsx", "ggplot2", "tidyr", "gapminder", "writexl", "car", "reshape2", "MatchIt", 
                      "lm.beta", "matrixStats", "sp", "imager", "ggseg", "ggseg3d", "mgcv", "visreg", "viridis",
                      "patchwork", "ggsegGlasser", "purrr", "brms", "tidybayes", "rstan", "StanHeaders", "tidyverse", "modelr",
                      "marginaleffects", "parameters", "modelsummary", "psych", "tinytable", "lme4", "lmerTest", "foreign", 
                      "lavaan", "semTools", "dplyr")
invisible(lapply(list.of.packages, library, character.only = TRUE))

#set your working directory to whatever file path is appropriate
setwd("C:/put path here")


#Load in data files  --------------------------------------------------------------------------------

#demographic information
demographics <- read_xlsx("put your demographics.xlsx file here")

#whole brain surface area/volume/GI measures
complete_data <- read_xlsx("put your complete_data.xlsx file here")

#whole brain expansion numbers
figure1data <- read_xlsx("put your figure1data.xlsx file here")

#parcellated expansion numbers (Glasser)
birthcohort <- read_xlsx("put your birthcohort.xlsx file here")

#raw data for deriving thrive
rawdata <- read_xlsx("put your rawdata.xlsx file here")

#parcellated surface area data (Glasser)
year2SA <- read_xlsx("put your year2SA.xlsx file here")
year3SA <- read_xlsx("put your year3SA.xlsx file here")

#Derivation of Thrive variable -----------------------------------------------------------------

#note that we generated these values for the entire eLABE cohort, not just the children included in this manuscript's analyses 

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

#these values were added to the demographics dataset (this code is just for demonstration purposes)

#Supplementary Table 1 (Demographics) -----------------------------------------------------------------------

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
    total_n = n(),
    mean_value = mean(GA_Birth, na.rm = TRUE),
    sd_value = sd(GA_Birth, na.rm = TRUE),
    min_value = min(GA_Birth, na.rm = TRUE),
    max_value = max(GA_Birth, na.rm = TRUE)
  )

#Age at birth MRI
birthmri <- demographics %>% filter(!is.na(Age_at_Birth))
birthmri %>%
  summarise(
    total_n = n(),
    mean_value = mean(Age_at_Birth, na.rm = TRUE),
    sd_value = sd(Age_at_Birth, na.rm = TRUE),
    min_value = min(Age_at_Birth, na.rm = TRUE),
    max_value = max(Age_at_Birth, na.rm = TRUE)
  )
#these numbers are in years, so I multiplied by 52 for reporting in the table

#Age at Year 2 MRI
year2mri <- demographics %>% filter(!is.na(Age_at_Year2))
year2mri %>%
  summarise(
    total_n = n(),
    mean_value = mean(Age_at_Year2, na.rm = TRUE),
    sd_value = sd(Age_at_Year2, na.rm = TRUE),
    min_value = min(Age_at_Year2, na.rm = TRUE),
    max_value = max(Age_at_Year2, na.rm = TRUE)
  )

#Age at Year 3 MRI
year3mri <- demographics %>% filter(!is.na(Age_at_Year3))
year3mri %>%
  summarise(
    total_n = n(),
    mean_value = mean(Age_at_Year3, na.rm = TRUE),
    sd_value = sd(Age_at_Year3, na.rm = TRUE),
    min_value = min(Age_at_Year3, na.rm = TRUE),
    max_value = max(Age_at_Year3, na.rm = TRUE)
  )

#Prenatal disadvantage
demographics %>%
  summarise(
    total_n = n(),
    mean_value = mean(disadv_prenatal, na.rm = TRUE),
    sd_value = sd(disadv_prenatal, na.rm = TRUE),
    min_value = min(disadv_prenatal, na.rm = TRUE),
    max_value = max(disadv_prenatal, na.rm = TRUE)
  )

#Thrive
demographics %>%
  summarise(
    total_n = n(),
    mean_value = mean(Thrive, na.rm = TRUE),
    sd_value = sd(Thrive, na.rm = TRUE),
    min_value = min(Thrive, na.rm = TRUE),
    max_value = max(Thrive, na.rm = TRUE)
  )


#ADI
demographics$ADI <- as.numeric(demographics$ADI)
demographics %>%
  summarise(
    total_n = n(),
    mean_value = mean(ADI, na.rm = TRUE),
    sd_value = sd(ADI, na.rm = TRUE),
    min_value = min(ADI, na.rm = TRUE),
    max_value = max(ADI, na.rm = TRUE)
  )

#INR
INR <- demographics %>% filter(!is.na(AVG_INR))
INR %>%
  summarise(
    total_n = n(),
    mean_value = mean(AVG_INR, na.rm = TRUE),
    sd_value = sd(AVG_INR, na.rm = TRUE),
    min_value = min(AVG_INR, na.rm = TRUE),
    max_value = max(AVG_INR, na.rm = TRUE)
  )

#HEI
HEI <- demographics %>% filter(!is.na(HEI))
HEI %>%
  summarise(
    total_n = n(),
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

#Supplementary Figure 1 and generating groups for Supplementary Figure 4 -------------------------------------

#Graph for 292 subjects
complete_data$MODID <- as.character(complete_data$MODID)
sliced_data <- complete_data %>% group_by(MODID) %>% slice(1)

ggplot(sliced_data, aes(
  x = disadv_prenatal, 
  y = Thrive)) + 
  geom_point() +
  labs(x = "PSD", y = "Thrive") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

#Graph for 83 subjects

#First, we had to create the 6 categories
colnames(figure1data)[1] = "MODID"
pullingIDs <- figure1data %>% left_join(demographics)
pullingIDs <- pullingIDs %>% filter(Age_Scan1 < 0.5) %>% filter(!is.na(Age_at_Year2)) %>% filter(Age_Scan2 == Age_at_Year2)

pullingIDs$disadv_group <- cut(pullingIDs$disadv_prenatal,
                               breaks = quantile(pullingIDs$disadv_prenatal, probs = c(0, 1/3, 2/3, 1)),
                               labels = c("LowDis", "MediumDis", "HighDis"),
                               include.lowest = TRUE)
pullingIDs <- pullingIDs %>%
  group_by(disadv_group) %>%
  mutate(
    thrive_strat2 = cut(
      Thrive,   
      breaks = quantile(Thrive, probs = c(0, 1/2, 1), na.rm = TRUE),
      labels = c("LowThrive", "HighThrive"),
      include.lowest = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(
    group3x2_strat2 = paste0(disadv_group, "_", thrive_strat2)
  )

#graphing this
ggplot(pullingIDs, aes(
  x = disadv_prenatal, 
  y = Thrive, 
  color = group3x2_strat2)) + 
  geom_point() +
  labs(x = "PSD", y = "Thrive", color = "Group") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"))


#Getting 12 subjects per group based on matching 
matched_strat2 <- pullingIDs %>%
  group_by(group3x2_strat2) %>%            
  arrange(abs(Age_Scan1 - 0.056)) %>% 
  slice_head(n = 12) %>%              
  ungroup()

matched_strat2 %>%
  group_by(group3x2_strat2) %>%
  summarise(
    Thrive_mean = mean(Thrive),
    Thrive_min = min(Thrive),
    Thrive_max = max(Thrive),
    Disadv_mean = mean(disadv_prenatal),
    Disadv_min = min(disadv_prenatal),
    Disadv_max = max(disadv_prenatal),
    total_n = n()
  )

#These are the categories reported in Supplementary Figure 4


#Figure 1 and corresponding text -----------------------------------------------------------

#Figure 1A

#we need to determine who has longitudinal data for color coding purposes 
seg_data <- complete_data %>%
  group_by(MODID) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  inner_join(
    complete_data %>%
      group_by(MODID) %>%
      mutate(row_id = row_number()) %>%
      ungroup(),
    by = "MODID",
    suffix = c("_start", "_end")
  ) %>%
  filter(row_id_start < row_id_end) %>%  
  mutate(
    segment_type = case_when(
      Time_Point_start == "Birth"  & Time_Point_end == "Year 2" ~ "Birth–Year2",
      Time_Point_start == "Year 2" & Time_Point_end == "Year 3" ~ "Year2–Year3",
      Time_Point_start == "Birth"  & Time_Point_end == "Year 3" ~ "Birth–Year3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(segment_type))

SA <- ggplot(complete_data, aes(x = Age_at_Scan, y = Total_SA)) +
  geom_segment(
    data = seg_data,
    aes(
      x    = Age_at_Scan_start,
      y    = Total_SA_start,
      xend = Age_at_Scan_end,
      yend = Total_SA_end,
      color = segment_type
    ),
    alpha = 0.7
  ) +
  geom_point(alpha = 0.8, size = 1) +
  scale_color_manual(
    values = c(
      "Birth–Year2" = "coral3",
      "Year2–Year3" = "turquoise3",
      "Birth–Year3" = "slategray4"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Total Brain Surface Area",
    x = "Age at Scan (Years)",
    y = "Total Brain Surface Area"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(60000, 200000, by = 20000)) + 
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank(),  # remove minor gridlines
    axis.line = element_line(color = "black")  # draw axis lines
  )

SA

#Figure 1B

birth <- complete_data %>% filter(Time_Point == "Birth")
year2 <- complete_data %>% filter(Time_Point == "Year 2")
year3 <- complete_data %>% filter(Time_Point == "Year 3")

mean(birth$Total_SA)
mean(year2$Total_SA)
mean(year3$Total_SA)

#Figure 1C

#create variables for midpoint age and percent expansion per year
figure1data$midpointage <- (figure1data$Age_Scan1 + figure1data$Age_Scan2)/2
figure1data$timebetweenyears <- figure1data$Time_Between_Months / 12
figure1data$transformedexp <- (((2^(figure1data$tot_expansion)) - 1)*100) #this turns it into percent change
figure1data$percentperyear <- figure1data$transformedexp / figure1data$timebetweenyears

#add a variable for category to help with color coding
figure1data <- figure1data %>%
  mutate(
    category = case_when(
      Age_Scan1 < 0.2 & Age_Scan2 < 2.7 ~ "Birth_Year2",
      Age_Scan1 < 0.2 & Age_Scan2 > 2.7 ~ "Birth_Year3",
      TRUE ~ "Year2_Year3"
    )
  )

#Plot it
plot <- ggplot(figure1data, aes(x = midpointage, y = percentperyear, color = category)) +
  geom_point() + 
  labs(x = "MidPoint Age Between Scans", y = "% Change Per Year", title = " ") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank(),  # remove minor gridlines
    axis.line = element_line(color = "black")  # draw axis lines
  )+
  theme(legend.position = "none")+
  scale_color_manual(
    values = c(
      "Birth_Year2" = "coral3",
      "Birth_Year3" = "slategray4",
      "Year2_Year3" = "turquoise3"
    )
  )+
  scale_x_continuous(
    breaks = seq(0, 4, by = 1),
    limits = c(0, 4)
  )

plot 


#Figure 1D and 1E were made in workbench. 

#grabbing values for this section of main text
#absolute rate per year for longitudinal kids

#for birth to age 2
birth_year2_wide <- complete_data %>%
  filter(Time_Point %in% c("Birth", "Year 2")) %>%
  group_by(MODID) %>%
  filter(n_distinct(Time_Point) == 2) %>%
  ungroup() %>%
  select(MODID, Time_Point, Age_at_Scan, Total_SA) %>%
  pivot_wider(
    names_from = Time_Point,
    values_from = c(Age_at_Scan, Total_SA),
    names_glue = "{.value}_{Time_Point}"
  )

birth_year2_wide <- birth_year2_wide %>%
  mutate(
    rawdiff = `Total_SA_Year 2` - `Total_SA_Birth`,
    timebetween = `Age_at_Scan_Year 2` - `Age_at_Scan_Birth`,
    mm2peryear = rawdiff/timebetween
  )
mean(birth_year2_wide$mm2peryear)


#for age 2 to age 3
year2_year3_wide <- complete_data %>%
  filter(Time_Point %in% c("Year 2", "Year 3")) %>%
  group_by(MODID) %>%
  filter(n_distinct(Time_Point) == 2) %>%
  ungroup() %>%
  select(MODID, Time_Point, Age_at_Scan, Total_SA) %>%
  pivot_wider(
    names_from = Time_Point,
    values_from = c(Age_at_Scan, Total_SA),
    names_glue = "{.value}_{Time_Point}"
  )%>%
  filter(MODID != "MOD2617") #this subject failed aMSM so we aren't including their longitudinal data 

year2_year3_wide <- year2_year3_wide %>%
  mutate(
    rawdiff = `Total_SA_Year 3` - `Total_SA_Year 2`,
    timebetween = `Age_at_Scan_Year 3` - `Age_at_Scan_Year 2`,
    mm2peryear = rawdiff/timebetween
  )
mean(year2_year3_wide$mm2peryear)

#percent per year
b2expansion <- figure1data %>% filter(category == "Birth_Year2")
mean(b2expansion$percentperyear)
a23expansion <- figure1data %>% filter(category == "Year2_Year3")
mean(a23expansion$percentperyear)



#Cross sectional PSD linear models at birth -------------------------------

birth <- birth %>%
  mutate(
    GA_centered = GA_Birth - mean(GA_Birth),
    Age_days = Age_at_Scan * 365,
    Age_days_centered = Age_days - mean(Age_days)
    
  )
model_birth <- lm(Total_SA ~ disadv_prenatal + Sex + GA_centered + Age_days_centered, data = birth)
summary(model_birth)
model2_birth <- lm(Total_Vol ~ disadv_prenatal + Sex + GA_centered + Age_days_centered, data = birth)
summary(model2_birth)
model3_birth <- lm(Avg_GI ~ disadv_prenatal + Sex + GA_centered + Age_days_centered, data = birth)
summary(model3_birth)

#Supplementary Table 2 (Total Brain Surface Area) --------------------------------------------
model2_SA <- gamm(Total_SA ~ Sex + GA_Birth + s(Age_at_Scan, k = 4, bs = "cr") 
                  + s(disadv_prenatal, k = 4, bs = "cr") 
                  + ti(Age_at_Scan, disadv_prenatal, k = 4), 
                  random = list(MODID = ~1), 
                  data = complete_data, 
                  method = "REML")
summary(model2_SA$lme);summary(model2_SA$gam)


#Supplementary Table 3 (Grey Matter Volume) --------------------------------------------------

model2_vol <- gamm(Total_Vol ~ Sex + GA_Birth + s(Age_at_Scan, k = 4, bs = "cr") + s(disadv_prenatal, k = 4, bs = "cr") + ti(Age_at_Scan, disadv_prenatal, k = 4), 
                   random = list(MODID = ~1), 
                   data = complete_data, 
                   method = "REML")
summary(model2_vol$lme);summary(model2_vol$gam)


#Supplementary Table 4 (GI) ------------------------------------------------------------------

model2_GI <- gamm(Avg_GI ~ Sex + GA_Birth + s(Age_at_Scan, k = 4, bs = "cr") + s(disadv_prenatal, k = 4, bs = "cr") + ti(Age_at_Scan, disadv_prenatal, k = 4), 
                  random = list(MODID = ~1), 
                  data = complete_data, 
                  method = "REML")
summary(model2_GI$lme);summary(model2_GI$gam)


#Figure 2 ------------------------------------------------------------------------------------

#creating data grid for all 3 plots
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

#Creating plot for surface area
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

#Creating plot for volumes
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

#Creating plot for GI
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

#Combining the plots
combined_disadvantage_plot <- (SA_plot_combined + vol_plot_combined + GI_plot_combined) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")  

combined_disadvantage_plot <- combined_disadvantage_plot + 
  plot_annotation(
    title = "Effect of Prenatal Disadvantage on Trajectories",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

combined_disadvantage_plot


#Supplementary Table 5 (Whole brain expansion model) ---------------------------------------------------------------

global_exp <- birthcohort %>% filter(ROI == "R_V1_ROI")
global_exp <- global_exp %>% select(ID:disadv_prenatal)
colnames(global_exp)[1] = "MODID"
colnames(figure1data)[1] = "MODID"
global_exp <- global_exp %>% left_join(figure1data)

model <- lmer(tot_expansion ~ Sex + GA_Birth + Age_Scan1 + Time_Between_Months + disadv_prenatal + (1 | MODID),
              data = global_exp)
summary(model)





#Figure 3A, 3D/Supplementary Table 6/Supplementary Figure 2A- mean surface area at birth of each parcel ------------

#we derived the birth values from the expansion values and the later time point surface areas to optimize point correspondence.
#A2/A1 = expansion, so A1 = A2/expansion

#preparing the expansion values
dataforchad <- birthcohort 
colnames(dataforchad)[10] = "expansion"
dataforchad <- dataforchad %>%
  mutate(
    ratio = 2^expansion
  )

#creating a time point column
dataforchad <- dataforchad %>%
  mutate(
    Age_Scan2 = Age_Scan1 + (Time_Between_Months/12),
    Time_Range = ifelse(Age_Scan2 < 2.7, "Birth_Year2", "Birth_Year3")
  )

#splitting it by time points for backtracking
chadbirthto2 <- dataforchad %>% filter(Time_Range == "Birth_Year2")
chadbirthto3 <- dataforchad %>% filter(Time_Range == "Birth_Year3")

#getting the y2 parcellated surface area data set up 
colnames(year2SA)[1] = "ID"
year2SA$ID <- substr(year2SA$ID, 1, 7)

year2SA_long <- year2SA %>%
  pivot_longer(
    cols = -c(ID, sum_year2, Time_Point),
    names_to = "ROI",
    values_to = "Y2_SA"
  ) 
year2SA_long <- year2SA_long %>% dplyr::select(ID, ROI, Y2_SA)

#getting the y3 parcellated surface area data set up 
colnames(year3SA)[1] = "ID"
year3SA$ID <- substr(year3SA$ID, 1, 7)

year3SA_long <- year3SA %>%
  pivot_longer(
    cols = -c(ID, Time_Point),
    names_to = "ROI",
    values_to = "Y3_SA"
  ) %>%
  dplyr::select(ID, ROI, Y3_SA)


#adding in parcellated surface area data to birth to 2 expansion data
completebirthto2 <- chadbirthto2 %>% left_join(year2SA_long)
completebirthto2 <- completebirthto2 %>%
  mutate(
    birthSA_from2 = Y2_SA / ratio
  )
birthto2SA <- completebirthto2 %>% select(c('ID', 'ROI', 'birthSA_from2'))



#adding in parcellated surface area data to birth to 3 expansion data
completebirthto3 <- chadbirthto3 %>% left_join(year3SA_long)
completebirthto3 <- completebirthto3 %>%
  mutate(
    birthSA_from3 = Y3_SA / ratio
  )
birthto3SA <- completebirthto3 %>% select(c('ID', 'ROI', 'birthSA_from3'))
birthbothversions <- birthto2SA %>% full_join(birthto3SA)
birthbothversions <- birthbothversions %>%
  mutate(
    birthSA_USE = ifelse(
      (!is.na(birthSA_from2)), birthSA_from2, birthSA_from3
    )
  )
birthSAfinal <- birthbothversions %>% select(c('ID', 'ROI', 'birthSA_USE'))


parcellatedSAs <- bind_rows(completebirthto2, completebirthto3)
parcellatedSAs <- parcellatedSAs %>% left_join(birthSAfinal)


parcellatedSAs <- parcellatedSAs %>%
  mutate(
    endpointSA = coalesce(Y2_SA, Y3_SA),
    SA_rawdiff = endpointSA - birthSA_USE,
    TimeBetweenYears = Time_Between_Months / 12, 
    rawdiff_peryear = SA_rawdiff / TimeBetweenYears
  )

norepeats <- parcellatedSAs %>% 
  distinct(ID, ROI, birthSA_USE, .keep_all = TRUE)

prior.t <- c(prior(normal(0,50), class = b),
             prior(normal(200,150), class = Intercept),
             prior(normal(0,300), class = sd, group = "ROI", coef = "Intercept"),
             prior(normal(0,50),  class = sd, group = "ROI", coef = "disadv_prenatal"),
             prior(normal(0,100),  class = sd, group = "ID"),
             prior(lkj(2), class = cor))

BirthSA_Model6 <- brm(birthSA_USE ~ 1 + Age_Scan1 + 
                        GA_Birth_Centered + Sex + disadv_prenatal + (1 + disadv_prenatal | ROI) + 
                        (1 | ID),
                      prior = prior.t,
                      data = norepeats,
                      cores = 6, chains = 4,iter = 8000, warmup = 2000,
                      control = list(max_treedepth = 12, adapt_delta = 0.95),
                      file = "birthSA_Model6",
                      backend = "cmdstanr")

summary(BirthSA_Model6) #this is reported in supplementary table 6


BirthSA6_Results <- BirthSA_Model6 %>% 
  spread_draws(b_disadv_prenatal, r_ROI[ROI,term]) %>%
  filter(term == "disadv_prenatal") %>% 
  mutate(slope = (b_disadv_prenatal + r_ROI)) %>%
  select(ROI, slope) %>%
  median_qi()

colnames(BirthSA6_Results)[3] = "slope_BirthSA6"
colnames(BirthSA6_Results)[4] = "lower_BirthSA6"
colnames(BirthSA6_Results)[5] = "upper_BirthSA6"
colnames(BirthSA6_Results)[1] = "region"
BirthSA6_Results$region <- gsub("_ROI", "", BirthSA6_Results$region)
BirthSA6_Results <- BirthSA6_Results %>%
  mutate(hemi = ifelse(
    grepl("^L_", region), "left", "right"
  ))
BirthSA6_Results$region <- substr(BirthSA6_Results$region, 3, nchar(BirthSA6_Results$region))
BirthSA6_Results$region <- gsub("\\.", "-", BirthSA6_Results$region)

BirthSA6_Results <- BirthSA6_Results %>%
  mutate(BirthSA6_includes0 = (lower_BirthSA6 <= 0 & upper_BirthSA6 >= 0))

#Figure 3A
ggseg(BirthSA6_Results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = ifelse(BirthSA6_includes0, NA, slope_BirthSA6))) +
  theme_classic() +
  scale_fill_gradient2(
    low  = "blue",    # negative
    mid  = "white",   # zero
    high = "red",     # positive
    midpoint = 0,
    na.value = "white"
  ) +
  labs(title = "PSD's effect on parcel SA at Birth, only significant ROIs shown", fill = "Effect Size") +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    ))

#version without thresholding (Supplementary Figure 2)
ggseg(BirthSA6_Results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = slope_BirthSA6)) +
  theme_classic() +
  scale_fill_gradient2(
    low  = "blue",    # negative
    mid  = "white",   # zero
    high = "red",     # positive
    midpoint = 0,
    na.value = "white"
  ) +
  labs(title = "PSD's effect on parcel SA at Birth, all ROIs shown", fill = "Effect Size") +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    ))



#making a line graph for Figure 3D
predictions(model = BirthSA_Model6,
            newdata = datagrid(disadv_prenatal = seq(from = -2.2, to = 2.2, length.out = 1000), 
                               Age_Scan1 = 0, #simulating birth 
                               GA_Birth_Centered = -0.34694, #this is equivalent to 38 weeks
                               ROI = "L_TE1p_ROI" #picking left temporal lobe as an example
            ),
            allow_new_levels = TRUE) %>%
  posterior_draws() %>%  
  ggplot(aes(x = disadv_prenatal, y = draw)) +
  stat_lineribbon(aes(y = draw),
                  .width = .95,
                  alpha = .5,
                  show.legend = F,
                  fill = "slateblue") +
  labs(
    x = " ",
    y = " "
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

#Supplementary Table 7/Supplementary Figure 3- change in mm2 of each parcel ---------------

#trying adding in a fixed effect
prior.t <- c(prior(normal(0,50), class = b),
             prior(normal(200,150), class = Intercept),
             prior(normal(0,300), class = sd, group = "ROI", coef = "Intercept"),
             prior(normal(0,50),  class = sd, group = "ROI", coef = "disadv_prenatal"),
             prior(normal(0,100),  class = sd, group = "ID"),
             prior(lkj(2), class = cor))

#PSD on absolute change in SA per year in each parcel 
RawDiff_Model4 <- brm(SA_rawdiff ~ 1 + Age_Scan1 + Time_Between_Centered + 
                        GA_Birth_Centered + Sex + disadv_prenatal + (1 + disadv_prenatal | ROI) + 
                        (1 | ID),
                      prior = prior.t,
                      data = parcellatedSAs,
                      cores = 6, chains = 4,iter = 4000,
                      control = list(max_treedepth = 15, adapt_delta = 0.99),
                      file = "RawDiff_Model4",
                      backend = "cmdstanr")

summary(RawDiff_Model4)

#pulling out ROI values
RawDiff4_Results <- RawDiff_Model4 %>% 
  spread_draws(b_disadv_prenatal, r_ROI[ROI,term]) %>%
  filter(term == "disadv_prenatal") %>% 
  mutate(slope = (b_disadv_prenatal + r_ROI)) %>%
  select(ROI, slope) %>%
  median_qi()

colnames(RawDiff4_Results)[3] = "slope_RawDiff4"
colnames(RawDiff4_Results)[4] = "lower_RawDiff4"
colnames(RawDiff4_Results)[5] = "upper_RawDiff4"
colnames(RawDiff4_Results)[1] = "region"
RawDiff4_Results$region <- gsub("_ROI", "", RawDiff4_Results$region)
RawDiff4_Results <- RawDiff4_Results %>%
  mutate(hemi = ifelse(
    grepl("^L_", region), "left", "right"
  ))
RawDiff4_Results$region <- substr(RawDiff4_Results$region, 3, nchar(RawDiff4_Results$region))
RawDiff4_Results$region <- gsub("\\.", "-", RawDiff4_Results$region)

RawDiff4_Results <- RawDiff4_Results %>%
  mutate(RawDiff4_includes0 = (lower_RawDiff4 <= 0 & upper_RawDiff4 >= 0))

ggseg(RawDiff4_Results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = ifelse(RawDiff4_includes0, NA, slope_RawDiff4))) +
  theme_classic() +
  scale_fill_gradient2(
    low  = "blue",    # negative
    mid  = "white",   # zero
    high = "red",     # positive
    midpoint = 0,
    na.value = "white"
  ) +
  labs(title = "PSD's effect on change in mm2 (fixed effect)", fill = "Effect Size") +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    ))


#non thresholded version
ggseg(RawDiff4_Results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = slope_RawDiff4)) +
  theme_classic() +
  scale_fill_gradient2(
    low  = "blue",    # negative
    mid  = "white",   # zero
    high = "red",     # positive
    midpoint = 0,
    na.value = "white"
  ) +
  labs(title = "PSD's effect on change in mm2 (fixed effect)", fill = "Effect Size") +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    ))



#making a line graph for panel D
predictions(model = RawDiff_Model4,
            newdata = datagrid(disadv_prenatal = seq(from = -2.2, to = 2.2, length.out = 1000), 
                               Age_Scan1 = 0, #simulating birth 
                               GA_Birth_Centered = -0.34694, #this is equivalent to 38 weeks
                               ROI = "L_TE1p_ROI" #picking left temporal lobe as an example
            ),
            allow_new_levels = TRUE) %>%
  posterior_draws() %>%  
  ggplot(aes(x = disadv_prenatal, y = draw)) +
  stat_lineribbon(aes(y = draw),
                  .width = .95,
                  alpha = .5,
                  show.legend = F,
                  fill = "slateblue") +
  labs(
    x = " ",
    y = " "
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 





#Figure 3B, 3D/Supplementary Table 8/Supplementary Figure 2B- Mean Expansion -----------------------------------------------------------

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

summary(MCMCcenteredv1) #this is reported in supplementary table 8

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

#Figure 3B
ggseg(Q1results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = ifelse(MCMCv1_includes0, NA, slope_MCMCv1))) +
  theme_classic() +
  scale_fill_viridis_c(option = "plasma", na.value = "white", direction = -1, limits = c(0.004,0.007)) +
  labs(title = "Figure 3B", fill = "Effect Size") +
  guides(fill = guide_colorbar(direction = "horizontal", title.position = "top")) + 
  theme(legend.position = "bottom")

#non thresholded version for Supplementary Figure 2
ggseg(Q1results,
      colour = "black", 
      atlas = glasser, 
      position = "stacked",
      mapping = aes(fill = slope_MCMCv1)) +
  theme_classic() +
  scale_fill_viridis_c(option = "plasma", na.value = "white", direction = -1) +
  labs(title = "Figure 3B", fill = "Effect Size") +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    ))

#Figure 3D
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
    x = " ",
    y = " "
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(2.1, 2.35), n.breaks= (6)) 












#Figure 3C, 3D/Supplementary Table 9- Variability in Expansion -----------------------------------------

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

summary(Q2_MCMC_yesmain) #this is reported in supplementary table 9

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

#Figure 3C
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




#Figure 3D
predictions(model = Q2_MCMC_yesmain,
            newdata = datagrid(disadv_prenatal = seq(from = -2.2, to = 2.2, length.out = 1000), 
                               Time_Between_Centered = -6.417, #this is equivalent to 24 months
                               Age_Scan1 = 0, #simulating birth 
                               GA_Birth_Centered = -0.34694, #this is equivalent to 38 weeks
                               ROI = "L_TE1p_ROI" #picking one of our significant ROIs
            ),
            allow_new_levels = TRUE,
            dpar = "sigma") %>%
  posterior_draws() %>%
  ggplot(aes(x = disadv_prenatal, y = draw)) +
  stat_lineribbon(aes(y = draw),
                  .width = .95,
                  alpha = .5,
                  show.legend = F,
                  fill = "gold") +
  labs(
    x = " ",
    y = " "
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

#Supplementary Table 10 (Grey Matter Volume Thrive) ---------------------------------------

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


#Supplementary Table 11 (GI Thrive) ------------------------------------------------------

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


#Supplementary Table 12 (Surface Area Thrive) ---------------------------------------------

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


#Figure 4A and B --------------------------------------------------------------------------------

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

#getting predictions for that data grid for surface area 
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
    ymin = min_fit - 0.2 * (max_fit - min_fit), #this is just for manually positioning the grey bar on the graph
    ymax = min_fit - 0.15 * (max_fit - min_fit)
  )

# --- Plot with bars showing significant Thrive 10 vs 90 differences ---
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
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7")) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7")) +
  labs(
    title = "Predicted Surface Area Given Varying Disadvantage and Thrive",
    x = "Prenatal Disadvantage",
    y = "Predicted Total Surface Area (mm²)",
    color = "Thrive Level",
    fill = "Thrive Level"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


plot

#now creating the graph for volumes
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


sig_intervals <- get_intervals(diff_df)

# Calculate y-axis mins per facet to position bars just below data lines
ymins <- pred_df %>%
  summarise(min_fit = min(fit, na.rm = TRUE), max_fit = max(fit, na.rm = TRUE))

sig_intervals <- sig_intervals %>%
  mutate(
    min_fit = ymins$min_fit,
    max_fit = ymins$max_fit,
    ymin = min_fit - 0.2 * (max_fit - min_fit), #this is just for manually positioning the grey bar on the graph
    ymax = min_fit - 0.15 * (max_fit - min_fit)
  )


# --- Plot with red bars showing significant Thrive 10 vs 90 differences ---

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
  scale_color_manual(values = c("#E69F00", "#009E73", "#CC79A7")) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#CC79A7")) +
  labs(
    title = "Predicted Cortical Volume Given Varying Disadvantage and Thrive",
    x = "Prenatal Disadvantage",
    y = "Predicted Cortical Volume (mm3)",
    color = "Thrive Level",
    fill = "Thrive Level"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


plot

#other possible color schemes
#"#00438A", "indianred1", "mediumpurple"
#"#0072B2", "#E69F00", "#009E73", "#CC79A7"


#Supplementary Table 13 and Figure 4C (Expansion Thrive) ----------------

#add in thrive to dataset
thrive_nums <- demographics %>% select(c('MODID', 'Thrive'))
global_exp <- global_exp %>% left_join(thrive_nums)

model <- lmer(tot_expansion ~ Sex + GA_Birth + Age_Scan1 + Time_Between_Months + disadv_prenatal*Thrive + (1 | MODID),
              data = global_exp)
summary(model)

#trying with no interaction to confirm (this gives us the same pattern of results)
model2 <- lmer(tot_expansion ~ Sex + GA_Birth + Age_Scan1 + Time_Between_Months + disadv_prenatal + Thrive + (1 | MODID),
              data = global_exp)
summary(model2)




#plot the effect of thrive (Figure 4C)

newdata <- expand.grid(
  Thrive = seq(min(global_exp$Thrive, na.rm = TRUE),
                        max(global_exp$Thrive, na.rm = TRUE),
                        length.out = 1000),
  disadv_prenatal = mean(global_exp$disadv_prenatal),
  Sex = "Male",
  GA_Birth = 38,
  Age_Scan1 = 0,
  Time_Between_Months = 24
)

pred <- predict(model, newdata = newdata, re.form = NA, se.fit = TRUE)
log_fit   <- pred$fit
log_lower <- pred$fit - 1.96 * pred$se.fit
log_upper <- pred$fit + 1.96 * pred$se.fit

newdata$fit <- 2^(log_fit)
newdata$lower <- 2^(log_lower)
newdata$upper <- 2^(log_upper)

ggplot(newdata, aes(x = Thrive, y = fit)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#0072B2", alpha = 0.2)  +
  scale_y_continuous(
    limits = c(1.9, 2.5),
    breaks = seq(1.9, 2.5, by = 0.1)
  ) +
  labs(
    title = " ",
    x = "Thrive",
    y = "Predicted Total Expansion"
  )  +
  theme_classic()


